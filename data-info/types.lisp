;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-defs)

;; hex formatting

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (value (type (eql 'offset)) stream)
  (format stream "\"~A\"" (format-hex-offset value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((value string) (Type (eql 'offset)))
  (parse-hex-offset value))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (value (type (eql 'address)) stream)
  (format stream "\"~A\"" (format-hex-offset value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((value string) (Type (eql 'address)))
  (parse-hex-offset value))

;; Base types

(def (class* eas) comment (xml-serializer)
  ()
  (:documentation "A separate-tag XML item comment."))

(def (class* eas) concrete-item ()
  ()
  (:documentation "A mixin for concrete (instantiatable) types."))

(def (class* eas) data-item (xml-serializer)
  ((comment nil :accessor t)
   (size nil :accessor t :type integer-or-null)
   (alignment nil :accessor t :type integer-or-null)
   (copy-origin nil :accessor t)
   (effective-alignment :accessor t)
   (effective-size :accessor t)
   (effective-finalized? nil :accessor t)
   (effective-has-pointers? :accessor t)
   (effective-tag :accessor t)
   (effective-min-offset :accessor t)
   (effective-max-offset :accessor t))
  (:documentation "An abstract base class for all type items."))

(def (class* eas) code-helper (xml-serializer)
  ((name nil :accessor t :type $-keyword))
  (:documentation "A bit of code to help in type presentation."))

(def (class* eas) code-helper-mixin ()
  ((code-helpers nil :accessor t)
   (effective-code-helpers nil :accessor t)))

(defgeneric public-type-name-of (node)
  (:method ((node data-item))
    (let ((cache (load-time-value (make-hash-table :test #'eq)))
          (class (class-of node)))
      (or (gethash class cache)
          (setf (gethash class cache)
                (xml:xml-tag-name-string node))))))

(defgeneric auto-code-helpers (node)
  (:method-combination append :most-specific-first)
  (:method append ((node code-helper-mixin)) nil))

(defmethod initialize-instance :before ((obj data-item) &key)
  (unless (typep obj 'concrete-item)
    (error "Could not instantiate an abstract type class: ~S" obj)))

(defmethod print-slots ((obj data-item))
  (stable-sort
   (remove-if (lambda (x &aux
                  (sym (closer-mop:slot-definition-name x))
                  (name (symbol-name sym)))
                (or (starts-with-subseq "EFFECTIVE-" name)
                    (starts-with-subseq "SYNTAX-" name)
                    (member sym '(is-created-by-xml-reader file copy-origin default-size))))
              (class-slots (class-of obj)))
   #'< :key (lambda (x &aux (name (closer-mop:slot-definition-name x)))
              (or (position name '(name type-name is-union count offset size alignment))
                  (if (eq name 'comment)
                      (if (stringp (comment-of obj)) 100 -1))
                  99))))

(defgeneric copy-data-definition (obj)
  (:method ((obj t)) obj)
  (:method ((obj cons))
    (cons (copy-data-definition (car obj))
          (copy-data-definition (cdr obj))))
  (:method ((obj data-item))
    (let* ((slots (print-slots obj))
           (vals (loop for slot in slots
                    for name = (closer-mop:slot-definition-name slot)
                    and initargs = (closer-mop:slot-definition-initargs slot)
                    when (and initargs
                              (slot-boundp obj name))
                    collect (first initargs)
                    and collect (copy-data-definition (slot-value obj name)))))
      (apply #'make-instance (class-of obj)
             :file (file obj) :copy-origin obj
             vals))))

;; Abstract type classes

(def (class* eas) data-field (data-item)
  ((name nil :accessor t :type $-keyword)
   (offset nil :accessor t :type offset)
   (syntax-parent nil :accessor t)
   (effective-parent :accessor t)
   (effective-offset :accessor t))
  (:documentation "An abstract type that can be inside a compound structure."))

(def (class* eas) unit-item (data-item)
  ((default-size :accessor t))
  (:documentation "An abstract type for a type that is complete without any arguments."))

(def (class* eas) virtual-compound-item (data-item)
  ((effective-fields :accessor t))
  (:documentation "An abstract type that may contain fake fields."))

(def (class* eas) compound-item (virtual-compound-item)
  ((fields nil :accessor t)
   (is-union nil :accessor t :type boolean))
  (:documentation "An abstract type that may contain real fields."))

(defmethod is-union-p ((item virtual-compound-item)) nil)

(defmethod add-subobject ((obj compound-item) (subobj data-field))
  (nconcf (fields-of obj) (list subobj))
  (setf (syntax-parent-of subobj) obj))

(defmethod initialize-instance :after ((obj compound-item) &key)
  (dolist (field (fields-of obj))
    (setf (syntax-parent-of field) obj)))

(def (class* eas) struct-compound-item (compound-item)
  ((key-field nil :accessor t :type $-keyword))
  (:documentation "An abstract type that contains named fields as part of its own structure."))

(def (class* eas) ref-compound-item (compound-item)
  ((type-name nil :accessor t :type $-keyword-namespace))
  (:documentation "An abstract type that may refer to a global type."))

(def (class* eas) container-item (ref-compound-item)
  ((effective-contained-item :accessor t)
   (effective-element-size :accessor t)
   (key-field nil :accessor t :type $-keyword)
   (has-bad-pointers nil :accessor t :type boolean))
  (:documentation "An abstract type that points to a set of elements."))

(def (class* eas) array-item (container-item code-helper-mixin)
  ((index-refers-to nil :accessor t :type string))
  (:documentation "An abstract container that contains an integer-indexed sequence of items."))

(defmethod auto-code-helpers append ((item array-item))
  (awhen (index-refers-to-of item)
    (list (make-instance 'code-helper :name $index-refers-to :content it))))

;; Concrete compound class

(def (class* eas) compound (data-field struct-compound-item ref-compound-item concrete-item)
  ()
  (:documentation "A structure/union type, that may be a proxy for a global type."))

;; Primitive fields

(def (class* eas) primitive-field (data-field unit-item code-helper-mixin)
  ((refers-to nil :accessor t :type string))
  (:documentation "An abstract type for a primitive field."))

(defmethod auto-code-helpers append ((item primitive-field))
  (awhen (refers-to-of item)
    (list (make-instance 'code-helper :name $refers-to :content it))))

(def (class* eas) padding (primitive-field concrete-item)
  ()
  (:default-initargs :default-size 0 :effective-alignment 1)
  (:documentation "A concrete type for unmarked space."))

;; Integers

(def (class* eas) integer-field (primitive-field)
  ((effective-int-signed? nil :accessor t :type boolean))
  (:documentation "An abstract type for an integer primitive field."))

(macrolet ((def-simple-int (name size signed?)
             `(def (class* eas) ,name (integer-field concrete-item)
                ()
                (:default-initargs :default-size ,size :effective-int-signed? ,signed?))))
  (def-simple-int uint8_t 1 nil)
  (def-simple-int int8_t 1 t)
  (def-simple-int uint16_t 2 nil)
  (def-simple-int int16_t 2 t)
  (def-simple-int uint32_t 4 nil)
  (def-simple-int int32_t 4 t)
  (def-simple-int uint64_t 8 nil)
  (def-simple-int int64_t 8 t)
  (def-simple-int bool 1 nil))

;; Bit

(def (class* eas) flag-bit (primitive-field concrete-item)
  ((size nil :accessor t :type address))
  (:default-initargs :default-size 1/8 :effective-alignment 1/8)
  (:documentation "A bitfield chunk."))

;; Pointer

(def (class* eas) pointer (unit-item container-item data-field concrete-item)
  ()
  (:default-initargs :default-size 4)
  (:documentation "A simple pointer to another object."))

(defmethod public-type-name-of ((name pointer))
  (concatenate 'string (public-type-name-of (effective-contained-item-of name)) "*"))

;; String

(def (class* eas) string-field (primitive-field)
  ()
  (:documentation "A string field."))

(def (class* eas) static-string (string-field concrete-item)
  ()
  (:default-initargs :default-size 0 :effective-alignment 1)
  (:documentation "A null-terminated string buffer embedded in the object."))

(def (class* eas) ptr-string (virtual-compound-item string-field concrete-item)
  ()
  (:documentation "A null-terminated string buffer pointer."))

(def (class* eas) stl-string (virtual-compound-item string-field concrete-item)
  ()
  (:documentation "An STL string buffer pointer."))

;; A static array (elements inline in the object itself)

(def (class* eas) static-array (array-item data-field concrete-item)
  ((count nil :accessor t :type integer-or-null)))

(defmethod public-type-name-of ((name static-array))
  (concatenate 'string (public-type-name-of (effective-contained-item-of name)) "[]"))

;; STL containers

(def (class* eas) stl-vector (array-item unit-item data-field concrete-item)
  ())

(defmethod public-type-name-of ((name stl-vector))
  (concatenate 'string "<" (public-type-name-of (effective-contained-item-of name)) ">"))

;; Global entity definition

(def (class* eas) global-type-definition (data-item code-helper-mixin)
  ((type-name nil :accessor t :type $-keyword)
   (effective-xml-form nil :accessor t))
  (:documentation "An abstract global entity definition."))

(defmethod name-of ((type global-type-definition))
  (type-name-of type))

(defmethod public-type-name-of ((name global-type-definition))
  (cl-linux-debug.data-info::get-$-field-name (type-name-of name)))

(defmethod read-return-value :after ((type global-type-definition))
  (assert (type-name-of type)))

(def (class* eas) struct-type (global-type-definition struct-compound-item concrete-item)
  ()
  (:documentation "A global structure type definition."))

(def (class* eas) inheriting-type (data-item)
  ((inherits-from nil :accessor t :type $-keyword-namespace))
  (:documentation "A type that can inherit."))

(def (class* eas) class-type (global-type-definition struct-compound-item inheriting-type concrete-item)
  ((mangled-name nil :accessor t :type string))
  (:documentation "A global class type definition."))

(defmethod initialize-instance :after ((type class-type) &key)
  (unless (mangled-name-of type)
    (setf (mangled-name-of type)
          (cl-linux-debug.data-info::get-$-field-name (type-name-of type))))
  (when (loop for c across (mangled-name-of type) always (not (xml::digit-char-p c)))
    (setf (mangled-name-of type)
          (format nil "~A~A" (length (mangled-name-of type)) (mangled-name-of type)))))

(def (class* eas) global-object (compound)
  ((effective-xml-form nil :accessor t))
  (:documentation "A global variable definition."))

(defmethod read-return-value :after ((type global-object))
  (assert (name-of type)))

(def (class* eas) data-definition (xml-serializer)
  ((namespace nil :accessor t :type $-keyword)
   (global-type-definitions nil :accessor t)
   (global-objects nil :accessor t))
  (:documentation "A wrapper for a group of related definitions."))

(defmethod add-subobject ((obj data-definition) (subobj global-type-definition))
  (nconcf (global-type-definitions-of obj) (list subobj)))

(defmethod add-subobject ((obj data-definition) (subobj global-object))
  (nconcf (global-objects-of obj) (list subobj)))
