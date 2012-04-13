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

(defmethod print-slots ((obj comment)) nil)

(def (class* eas) concrete-item ()
  ()
  (:documentation "A mixin for concrete (instantiatable) types."))

(def (class* eas) abstract-item (xml-serializer)
  ((comment nil :accessor t)
   (copy-origin nil :accessor t)
   (effective-finalized? nil :accessor t)
   (effective-tag :accessor t)
   (effective-id-string))
  (:documentation "An abstract base class for all type items."))


(defmethod add-subobject ((obj abstract-item) (subobj !--))
  ;; Ignore comments
  (values))

(def (class* eas) data-item (abstract-item)
  ((size nil :accessor t :type integer-or-null)
   (alignment nil :accessor t :type integer-or-null)
   (effective-alignment :accessor t)
   (effective-size :accessor t)
   (effective-has-pointers? :accessor t)
   (effective-min-offset :accessor t)
   (effective-max-offset :accessor t))
  (:documentation "An abstract base class for all type items."))

(def (generic e) has-methods? (type)
  (:method ((type data-item)) nil)
  (:method ((type null)) nil))

(def (class* eas) code-helper (xml-serializer)
  ((name nil :accessor t :type $-keyword))
  (:documentation "A bit of code to help in type presentation."))

(def (class* eas) code-helper-mixin ()
  ((code-helpers nil :accessor t)
   (effective-code-helpers nil :accessor t)))

(defgeneric public-type-name-of (node)
  (:method ((node abstract-item))
    (let ((cache (load-time-value (make-hash-table :test #'eq)))
          (class (class-of node)))
      (or (gethash class cache)
          (setf (gethash class cache)
                (xml:xml-tag-name-string node))))))

(defgeneric auto-code-helpers (node)
  (:method-combination append :most-specific-first)
  (:method append ((node code-helper-mixin)) nil))

(defmethod initialize-instance :before ((obj abstract-item) &key)
  (unless (typep obj 'concrete-item)
    (error "Could not instantiate an abstract type class: ~S" obj)))

(defmethod print-slots ((obj abstract-item))
  (stable-sort
   (remove-if (lambda (x &aux
                  (sym (closer-mop:slot-definition-name x))
                  (name (symbol-name sym)))
                (or (starts-with-subseq "EFFECTIVE-" name)
                    (starts-with-subseq "SYNTAX-" name)
                    (member sym '(xml::is-created-by-xml-reader file copy-origin default-size))))
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
  (:method ((obj abstract-item))
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

(def (class* eas) abstract-field (abstract-item)
  ((name nil :accessor t :type $-keyword)
   (syntax-parent nil :accessor t)
   (effective-parent :accessor t))
  (:documentation "An abstract type that can be inside a compound structure."))

(def (class* eas) abstract-compound-item (abstract-item)
  ((effective-fields :accessor t))
  (:documentation "An abstract type that may contain fake fields."))

(def (class* eas) abstract-real-compound-item (abstract-compound-item)
  ((fields nil :accessor t))
  (:documentation "An abstract type that may contain real fields."))

(defmethod initialize-instance :after ((obj abstract-real-compound-item) &key)
  (dolist (field (fields-of obj))
    (setf (syntax-parent-of field) obj)))

(defgeneric can-add-subfield? (parent subobj))

(defmethod add-subobject ((obj abstract-real-compound-item) (subobj abstract-field))
  (unless (can-add-subfield? obj subobj)
    (error "Cannot use <~A/> as a subfield of <~A/>"
           (xml:xml-tag-name-string subobj)
           (xml:xml-tag-name-string obj)))
  (nconcf (fields-of obj) (list subobj))
  (setf (syntax-parent-of subobj) obj))

;; Abstract field classes

(def (class* eas) data-field (data-item abstract-field)
  ((offset nil :accessor t :type offset)
   (init-value nil :accessor t :type string)
   (effective-offset :accessor t))
  (:documentation "An abstract type that can be inside a compound structure."))

(def (class* eas) unit-item (data-item)
  ((default-size :accessor t))
  (:documentation "An abstract type for a type that is complete without any arguments."))

(def (class* eas) virtual-compound-item (data-item abstract-compound-item)
  ((effective-inherited-child nil :accessor t))
  (:documentation "An abstract type that may contain fake fields."))

(def (class* eas) compound-item (virtual-compound-item abstract-real-compound-item)
  ((is-union nil :accessor t :type boolean))
  (:documentation "An abstract type that may contain real fields."))

(defmethod is-union-p ((item virtual-compound-item)) nil)

(defmethod can-add-subfield? ((obj compound-item) (subobj data-field)) t)

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

(def (class* eas) sequence-item (container-item code-helper-mixin)
  ((index-refers-to nil :accessor t :type string)
   (index-enum nil :accessor t :type $-keyword-namespace)
   (effective-index-enum-tag nil :accessor t))
  (:documentation "An abstract container that contains an implicitly ordered sequence of items."))

(defmethod auto-code-helpers append ((item sequence-item))
  (awhen (index-refers-to-of item)
    (list (make-instance 'code-helper :name $index-refers-to :content it))))

(def (class* eas) array-item (sequence-item)
  ()
  (:documentation "A subtype of sequence-item with elements arranged adjacent in memory."))

(defmethod auto-code-helpers append ((item array-item))
  (awhen (index-refers-to-of item)
    (list (make-instance 'code-helper :name $index-refers-to :content it))))

;; Concrete compound class

(def (class* eas) proxifiable-item-mixin (concrete-item)
  ()
  (:documentation "An item that can be turned into a proxy"))

(def (generic e) can-proxify-for-type? (proxifiable global-type)
  (:method ((pf proxifiable-item-mixin) (gt data-item)) nil)
  (:method :after ((compound abstract-real-compound-item) type)
    (unless (null (fields-of compound))
      (error "Cannot have both TYPE-NAME and fields in: ~A" compound)))
  (:method :after ((compound compound-item) type)
    (unless (null (is-union-p compound))
      (error "Cannot have both TYPE-NAME and IS-UNION in: ~A" compound))))

(def (class* eas) compound (proxifiable-item-mixin data-field struct-compound-item ref-compound-item)
  ()
  (:documentation "A structure/union type, that may be a proxy for a global type."))

(defmethod can-proxify-for-type? ((type compound) (global struct-compound-item)) t)

;; Primitive fields

(def (class* eas) primitive-field (data-field unit-item code-helper-mixin)
  ((refers-to nil :accessor t :type string)
   (ref-target nil :accessor t :type $-keyword-namespace)
   (aux-value nil :accessor t :type string))
  (:documentation "An abstract type for a primitive field."))

(defmethod auto-code-helpers append ((item primitive-field))
  (awhen (refers-to-of item)
    (list (make-instance 'code-helper :name $refers-to :content it))))

(def (class* eas) padding (primitive-field concrete-item)
  ()
  (:default-initargs :default-size 0 :effective-alignment 1)
  (:documentation "A concrete type for unmarked space."))

;; Integers

(def (class* eas) integer-item (unit-item)
  ((effective-int-signed? nil :accessor t :type boolean))
  (:documentation "An abstract type for an integer primitive object."))

(def (class* eas) integer-field (integer-item primitive-field)
  ()
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

;; Float

(def (class* eas) s-float (primitive-field concrete-item)
  ()
  (:default-initargs :default-size 4)
  (:documentation "A float field."))

;; Bit

(def (class* eas) bit-item (primitive-field)
  ((count 1 :accessor t :type integer))
  (:default-initargs :default-size 1/8 :effective-alignment 1/8)
  (:documentation "A bitfield chunk."))

(def (class* eas) flag-bit (proxifiable-item-mixin bit-item concrete-item)
  ((type-name nil :accessor t :type $-keyword-namespace))
  (:documentation "A bitfield element."))

(def (class* eas) base-type-item (integer-item)
  ((base-type nil :accessor t :type $-keyword)
   (effective-base-type :accessor t)))

(def (class* eas) bitfield-item (base-type-item struct-compound-item)
  ()
  (:default-initargs :base-type $uint32_t))

(def (class* eas) bitfield (data-field bitfield-item concrete-item)
  ())

(defmethod can-add-subfield? ((obj compound-item) (subobj flag-bit)) nil)
(defmethod can-add-subfield? ((obj bitfield-item) (subobj data-field)) nil)
(defmethod can-add-subfield? ((obj bitfield-item) (subobj flag-bit)) t)

;; Pointer

(def (class* eas) pointer-item (unit-item container-item data-field)
  ((is-array nil :type boolean))
  (:default-initargs :default-size 4)
  (:documentation "A superclass of a simple pointer to another object or an array."))

(defmethod public-type-name-of ((name pointer-item))
  (concatenate 'string (public-type-name-of (effective-contained-item-of name)) "*"))

(def (class* eas) pointer (pointer-item concrete-item)
  ()
  (:documentation "A simple pointer to another object."))

(def (class* eas) pointer/array (pointer-item array-item)
  ()
  (:default-initargs :is-array t)
  (:documentation "A pointer to an array of objects."))

(defmethod xml-tag-name-symbol ((obj pointer/array)) 'pointer)

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

(def (class* eas) stl-bit-vector (array-item unit-item data-field concrete-item)
  ()
  (:default-initargs :type-name $flag-bit))

(defmethod xml:xml-tag-name-symbol ((bv stl-bit-vector)) 'stl-bit-vector)

(def (class* eas) stl-deque (sequence-item unit-item data-field concrete-item)
  ())

(defmethod xml:xml-tag-name-symbol ((bv stl-deque)) 'stl-deque)

;; Enums

(def (class* eas) abstract-enum-item (data-item abstract-real-compound-item)
  ((lookup-tables :accessor t)
   (enum-attrs nil :accessor t))
  (:documentation "Abstract type for enums."))

(def (class* eas) enum-attr (abstract-item concrete-item)
  ((name nil :accessor t :type $-keyword)
   (type-name nil :accessor t :type $-keyword-namespace)
   (default-value nil :accessor t :type string)
   (use-key-name nil :accessor t :type boolean)
   (is-list nil :accessor t :type boolean)
   (effective-base-type :accessor t)
   (effective-table :accessor t)))

(defmethod read-return-value :after ((type enum-attr))
  (assert (name-of type)))

(def (class* eas) enum-item (abstract-field concrete-item)
  ((value nil :accessor t :type integer-or-null)
   (item-attrs nil :accessor t)
   (effective-value :accessor t)))

(def (class* eas) item-attr (abstract-item concrete-item)
  ((name nil :accessor t :type $-keyword)
   (value nil :accessor t :type string)))

(defmethod read-return-value :after ((type item-attr))
  (assert (and (name-of type) (value-of type))))

(defmethod can-add-subfield? ((obj abstract-enum-item) (subobj enum-item)) t)

(def (class* eas) enum-field (abstract-enum-item base-type-item)
  ()
  (:documentation "Ad-hoc enum field"))

(def (class* eas) enum (proxifiable-item-mixin enum-field integer-field)
  ((type-name nil :accessor t :type $-keyword-namespace))
  (:documentation "Ad-hoc enum field"))

(defmethod can-proxify-for-type? ((type enum) (global abstract-enum-item)) t)
(defmethod can-proxify-for-type? ((type flag-bit) (global abstract-enum-item)) t)

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

(def (class* eas) inheriting-type (struct-compound-item code-helper-mixin)
  ((inherits-from nil :accessor t :type $-keyword-namespace)
   (instance-vector nil :accessor t :type string)
   (custom-methods nil :accessor t :type boolean))
  (:documentation "A type that can inherit."))

(defmethod auto-code-helpers append ((item inheriting-type))
  (awhen (instance-vector-of item)
    (list (make-instance 'code-helper :name $find-instance
                         :content (if (key-field-of item)
                                      (format nil "(find-by-id ~A ~S $)" it (key-field-of item))
                                      (format nil "~A[$]" it))))))

(def (class* eas) struct-type (global-type-definition inheriting-type concrete-item)
  ((has-methods nil :accessor t :type boolean))
  (:documentation "A global structure type definition."))

(defmethod has-methods? ((type struct-type))
  (or (has-methods-p type) (has-methods? (effective-inherited-child-of type))))

(def (class* eas) class-type (global-type-definition inheriting-type concrete-item)
  ((original-name nil :accessor t :type string)
   (linux-mangling nil :accessor t :type string)
   (windows-mangling nil :accessor t :type string)
   (virtual-methods nil))
  (:documentation "A global class type definition."))

(defmethod has-methods? ((type class-type)) t)

(def (class* eas) bitfield-type (global-type-definition bitfield-item concrete-item)
  ()
  (:documentation "A global bitfield type definition."))

(def (class* eas) enum-type (global-type-definition enum-field concrete-item)
  ()
  (:documentation "A global enum type definition."))

(def (class* eas) global-object (data-field container-item concrete-item)
  ((effective-xml-form nil :accessor t))
  (:documentation "A global variable definition."))

(defmethod read-return-value :after ((type global-object))
  (assert (name-of type)))

(def (class* eas) symbol-table (abstract-item concrete-item)
  ((name nil :accessor t :type string)
   (os-type nil :accessor t :type $-keyword)
   (constraints nil :accessor t)
   (elements nil :accessor t)
   (effective-xml-form nil :accessor t))
  (:documentation "A custom symbol table definition."))

(defmethod read-return-value :after ((type symbol-table))
  (assert (name-of type)))

(def (class* eas) binary-timestamp (xml-serializer)
  ((value nil :accessor t :type address)))

(def (class* eas) md5-hash (xml-serializer)
  ((value nil :accessor t :type string)))

(defmethod add-subobject ((obj symbol-table) (subobj binary-timestamp))
  (nconcf (constraints-of obj) (list subobj)))
(defmethod add-subobject ((obj symbol-table) (subobj md5-hash))
  (nconcf (constraints-of obj) (list subobj)))

(def (class* eas) global-address (xml-serializer)
  ((name nil :accessor t :type $-keyword-namespace)
   (comment nil :accessor t)
   (value nil :accessor t :type address)))

(defmethod add-subobject ((obj symbol-table) (subobj global-address))
  (nconcf (elements-of obj) (list subobj)))

(def (class* eas) vtable-address (xml-serializer)
  ((name nil :accessor t :type $-keyword-namespace)
   (comment nil :accessor t)
   (value nil :accessor t :type address)))

(defmethod add-subobject ((obj symbol-table) (subobj vtable-address))
  (nconcf (elements-of obj) (list subobj)))

(def (class* eas) data-definition (xml-serializer)
  ((namespace nil :accessor t :type $-keyword)
   (global-type-definitions nil :accessor t)
   (global-objects nil :accessor t)
   (symbol-tables nil :accessor t))
  (:documentation "A wrapper for a group of related definitions."))

(defmethod add-subobject ((obj data-definition) (subobj !--))
  ;; Ignore comments
  (values))

(defmethod add-subobject ((obj data-definition) (subobj global-type-definition))
  (nconcf (global-type-definitions-of obj) (list subobj)))

(defmethod add-subobject ((obj data-definition) (subobj global-object))
  (nconcf (global-objects-of obj) (list subobj)))

(defmethod add-subobject ((obj data-definition) (subobj symbol-table))
  (nconcf (symbol-tables-of obj) (list subobj)))

;; Methods

(def (class* eas) virtual-methods (xml-serializer)
  ((methods nil))
  (:documentation "A container for virtual method definitions."))

(def (class* eas) vmethod (abstract-field abstract-real-compound-item concrete-item)
  ((is-destructor nil :type boolean)
   (ret-type nil :type $-keyword-namespace-or-obj))
  (:documentation "A virtual method definition."))

(defmethod add-subobject ((obj virtual-methods) (subobj vmethod))
  (nconcf (methods-of obj) (list subobj)))

(defmethod can-add-subfield? ((obj vmethod) (subobj data-field)) t)

(def (class* eas) ret-type (ref-compound-item concrete-item)
  ()
  (:documentation "A wrapper for ."))
