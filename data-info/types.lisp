;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-defs)

(defvar *known-types* nil)
(defvar *known-types-version* 0)

(defvar *known-globals* nil)
(defvar *known-globals-version* 0)

(defparameter *type-context* nil)

(defun align-up (offset alignment)
  (* alignment (ceiling offset alignment)))

(deftype $-keyword () 'symbol)
(deftype $-keyword-namespace () '(or symbol cons))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (value (type (eql '$-keyword)) stream)
  (format stream "\"~A\"" (cl-linux-debug.data-info::get-$-field-name value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((value string) (Type (eql '$-keyword)))
  (aprog1 (cl-linux-debug.data-info::get-$-field value)
    (assert (symbolp it))))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (value (type (eql '$-keyword-namespace)) stream)
  (format stream "\"~A\"" (cl-linux-debug.data-info::get-$-field-name value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((value string) (Type (eql '$-keyword-namespace)))
  (cl-linux-debug.data-info::get-$-field value))

(deftype offset () '(or signed-byte ratio null))

(defun format-hex-offset (offset)
  (multiple-value-bind (int rest) (floor offset 1)
    (string-downcase
     (format nil "~:[~;-~]0x~X~@[.~A~]"
             (< int 0) (abs int)
             (ecase rest
               (0 nil)
               ((1/8 2/8 3/8 4/8 5/8 6/8 7/8) (* 8 rest)))))))

(defun parse-hex-offset (offset)
  (or (cl-ppcre:register-groups-bind (sign? body tail)
          ("(-)?0x([0-9a-fA-F]+)(?:.([0-7]))?" offset)
        (check-type body string)
        (let* ((iv (parse-integer body :radix 16))
               (siv (if sign? (- iv) iv)))
          (if tail
              (+ siv (/ (parse-integer tail) 8))
              siv)))
      (error "Invalid syntax for offset: '~A'" offset)))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (value (type (eql 'offset)) stream)
  (format stream "\"~A\"" (format-hex-offset value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((value string) (Type (eql 'offset)))
  (parse-hex-offset value))

(deftype address () '(or unsigned-byte ratio null))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (value (type (eql 'address)) stream)
  (format stream "\"~A\"" (format-hex-offset value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((value string) (Type (eql 'address)))
  (parse-hex-offset value))

(def (class* eas) comment (xml-serializer)
  ())

(def (class* eas) data-item (xml-serializer)
  ((comment nil :accessor t)
   (size nil :accessor t :type integer-or-null)
   (alignment nil :accessor t :type integer-or-null)
   (copy-origin nil :accessor t)
   (effective-alignment :accessor t)
   (effective-size :accessor t)
   (effective-finalized? nil :accessor t)
   (effective-tag :accessor t)))

(defgeneric compute-effective-size (obj)
  (:method :around ((obj data-item))
    (or (size-of obj)
        (align-up (call-next-method)
                  (effective-alignment-of obj)))))

(defgeneric compute-effective-alignment (obj)
  (:method :around ((obj data-item))
    (or (alignment-of obj) (call-next-method))))

(defgeneric layout-type-rec (obj)
  (:method ((obj data-item))
    (setf (effective-alignment-of obj)
          (compute-effective-alignment obj)
          (effective-size-of obj)
          (compute-effective-size obj)))
  (:method :after ((obj data-item))
    (setf (effective-finalized? obj) t)))

(defmethod print-slots ((obj data-item))
  (stable-sort
   (remove-if (lambda (x &aux
                  (sym (closer-mop:slot-definition-name x))
                  (name (symbol-name sym)))
                (or (starts-with-subseq "EFFECTIVE-" name)
                    (starts-with-subseq "SYNTAX-" name)
                    (member sym '(is-created-by-xml-reader file copy-origin))))
              (class-slots (class-of obj)))
   #'< :key (lambda (x &aux (name (closer-mop:slot-definition-name x)))
              (or (position name '(name type-name is-union count offset size alignment))
                  (if (eq name 'comment)
                      (if (stringp (comment-of obj)) 100 -1))
                  99))))

(defgeneric copy-data-definition (obj)
  (:method ((obj t)) obj)
  (:method ((obj list))
    (mapcar #'copy-data-definition obj))
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

(def (class* eas) data-field (data-item)
  ((name nil :accessor t :type $-keyword)
   (offset nil :accessor t :type offset)
   (syntax-parent nil :accessor t)
   (effective-parent :accessor t)
   (effective-offset :accessor t)))

(def (class* eas) compound-item (data-item)
  ((fields nil :accessor t)
   (is-union nil :accessor t :type boolean)
   (effective-fields :accessor t)))

(defgeneric layout-fields (obj fields)
  (:method :before ((obj compound-item) fields)
    (dolist (field fields)
      (setf (effective-parent-of field) obj)
      (layout-type-rec field)))
  (:method ((obj compound-item) fields)
    (let ((offset 0))
      (dolist (field fields)
        (setf offset (or (offset-of field)
                         (when (is-union-p obj) 0)
                         (align-up offset (effective-alignment-of field))))
        (setf (effective-offset-of field) offset)
        (incf offset (effective-size-of field))))))

(defmethod compute-effective-size ((obj compound-item))
  (reduce #'max (effective-fields-of obj)
          :key (lambda (x) (+ (effective-offset-of x) (effective-size-of x)))
          :initial-value 0))

(defmethod compute-effective-alignment ((obj compound-item))
  (reduce #'max (effective-fields-of obj)
          :key #'effective-alignment-of
          :initial-value 1))

(defgeneric compute-effective-fields (obj)
  (:method ((obj data-item)) nil)
  (:method ((obj compound-item))
    (fields-of obj)))

(defmethod layout-type-rec ((obj compound-item))
  (let ((fields (compute-effective-fields obj)))
    (setf (effective-fields-of obj) fields)
    (layout-fields obj fields))
  (call-next-method))

(defmethod add-subobject ((obj compound-item) (subobj data-field))
  (nconcf (fields-of obj) (list subobj))
  (setf (syntax-parent-of subobj) obj))

(defmethod copy-data-definition ((obj compound-item))
  (aprog1 (call-next-method)
    (dolist (field (fields-of it))
      (setf (syntax-parent-of field) it))))

(def (class* eas) ref-compound-item (compound-item)
  ((type-name nil :accessor t :type $-keyword-namespace)))

(defgeneric make-proxy-field (obj type))

(defgeneric lookup-type-reference (context referrer name)
  (:method (context referrer (name null))
    (declare (ignore context))
    (error "~A must have fields or TYPE-NAME" (class-name (class-of referrer))))
  (:method (context referrer name)
    (declare (ignore referrer context))
    (error "Unknown type name: ~A" name)))

(defmethod compute-effective-fields ((obj ref-compound-item))
  (cond ((or (type-name-of obj) (null (fields-of obj)))
         (unless (and (null (fields-of obj))
                      (null (is-union-p obj)))
           (error "When TYPE-NAME is given, direct fields are not allowed."))
         (let ((top-type (lookup-type-reference *type-context* obj (type-name-of obj))))
           (awhen (make-proxy-field obj top-type)
             (list it))))
        (t
         (call-next-method))))

(def (class* eas) global-type-proxy (data-field)
  ((type-name nil :accessor t :type $-keyword-namespace)
   (effective-main-type :accessor t)))

(macrolet ((delegate (name)
             `(defmethod ,name ((proxy global-type-proxy))
                (let ((base (effective-main-type-of proxy)))
                  (assert (and base (effective-finalized? base)))
                  (,name base)))))
  (delegate effective-size-of)
  (delegate effective-alignment-of)
  (delegate effective-tag-of))

(defmethod layout-type-rec ((proxy global-type-proxy)))

(def (class* eas) struct-compound-item (compound-item)
  ())

(def (class* eas) compound (data-field struct-compound-item ref-compound-item)
  ())

(defmethod layout-type-rec ((obj compound))
  (if (type-name-of obj)
      (progn
        (unless (and (null (fields-of obj))
                     (not (is-union-p obj))
                     (null (size-of obj))
                     (null (alignment-of obj)))
          (error "COMPOUND with a TYPE-NAME can't have fields."))
        (let ((ref (lookup-type-reference *type-context* obj (type-name-of obj))))
          (change-class obj 'global-type-proxy :effective-main-type ref)))
      (call-next-method)))

(def (class* eas) container-item (ref-compound-item)
  ((effective-contained-item :accessor t)
   (effective-element-size :accessor t)))

(defmethod compute-effective-size :before ((obj container-item))
  (slot-makunbound obj 'effective-element-size))

(defmethod slot-unbound (class (obj container-item) (slot (eql 'effective-element-size)))
  (let ((elt (effective-contained-item-of obj)))
    (setf (effective-element-size-of obj) (effective-size-of elt))))

(defmethod compute-effective-fields ((obj container-item))
  (let ((fields (call-next-method)))
    (setf (effective-contained-item-of obj)
          (if (> (length fields) 1)
              (make-instance 'compound :syntax-parent obj :fields fields)
              (first fields)))
    nil))

(defmethod layout-fields :before ((obj container-item) fields)
  (declare (ignore fields))
  (let ((item (effective-contained-item-of obj)))
    (setf (effective-parent-of item) obj)
    (layout-type-rec item)))

(def (class* eas) primitive-field (data-field)
  ()
  (:default-initargs :effective-finalized? t))

(def (class* eas) padding (primitive-field)
  ())

(defmethod compute-effective-size ((obj padding)) 0)
(defmethod compute-effective-alignment ((obj padding)) 1)

(defmethod make-proxy-field (obj (type primitive-field))
  type)

(def (class* eas) integer-field (primitive-field)
  ((syntax-int-signed? nil :accessor t :type boolean)
   (syntax-int-size nil :accessor t :type integer)))

(defmethod compute-effective-size ((obj integer-field))
  (syntax-int-size-of obj))

(defmethod compute-effective-alignment ((obj integer-field))
  (syntax-int-size-of obj))

(def (class* eas) int8_t (integer-field)
  ()
  (:default-initargs :syntax-int-size 1 :syntax-int-signed? t))

(def (class* eas) uint8_t (integer-field)
  ()
  (:default-initargs :syntax-int-size 1 :syntax-int-signed? nil))

(def (class* eas) int16_t (integer-field)
  ()
  (:default-initargs :syntax-int-size 2 :syntax-int-signed? t))

(def (class* eas) uint16_t (integer-field)
  ()
  (:default-initargs :syntax-int-size 2 :syntax-int-signed? nil))

(def (class* eas) int32_t (integer-field)
  ()
  (:default-initargs :syntax-int-size 4 :syntax-int-signed? t))

(def (class* eas) uint32_t (integer-field)
  ()
  (:default-initargs :syntax-int-size 4 :syntax-int-signed? nil))

(def (class* eas) int64_t (integer-field)
  ()
  (:default-initargs :syntax-int-size 8 :syntax-int-signed? t))

(def (class* eas) uint64_t (integer-field)
  ()
  (:default-initargs :syntax-int-size 8 :syntax-int-signed? nil))

(def (class* eas) pointer (data-field container-item)
  ())

(defmethod make-proxy-field (obj (type pointer))
  type)

(defgeneric size-in-context (context tag)
  (:method (ctx tag)
    (declare (ignore ctx))
    (ecase tag
      ('pointer 4))))

(defmethod compute-effective-size ((obj pointer))
  (size-in-context *type-context* 'pointer))

(defmethod compute-effective-alignment ((obj pointer))
  (size-in-context *type-context* 'pointer))

(defmethod lookup-type-reference (context (obj pointer) (name null))
  (declare (ignore context))
  (make-instance 'padding :syntax-parent obj))

(macrolet ((primitives (&rest names)
             `(progn
                ,@(loop for name in names
                     for kwd = (cl-linux-debug.data-info::get-$-field
                                (string-downcase (symbol-name name)))
                     collect `(defmethod lookup-type-reference
                                  (context ref (name (eql ,kwd)))
                                (declare (ignore context))
                                (make-instance ',name :syntax-parent ref))))))
  (primitives int8_t uint8_t int16_t uint16_t
              int32_t uint32_t int64_t uint64_t
              pointer))

(def (class* eas) array-item (container-item)
  ())

(def (class* eas) static-array (data-field array-item)
  ((count nil :accessor t :type integer-or-null)))

(defmethod compute-effective-size ((obj static-array))
  (* (effective-element-size-of obj) (count-of obj)))

(defmethod compute-effective-alignment ((obj static-array))
  (effective-alignment-of (effective-contained-item-of obj)))

(def (class* eas) global-type-definition (data-item)
  ((type-name nil :accessor t :type $-keyword)))

(defmethod make-proxy-field (obj (type global-type-definition))
  (make-instance 'global-type-proxy :syntax-parent obj
                 :type-name (type-name-of type)
                 :effective-main-type type))

(defmethod read-return-value :after ((type global-type-definition))
  (assert (type-name-of type)))

(def (class* eas) struct-type (struct-compound-item global-type-definition)
  ())

(def (class* eas) global-object (compound)
  ())

(defmethod read-return-value :after ((type global-object))
  (assert (name-of type)))

(def (class* eas) data-definition (xml-serializer)
  ((namespace nil :accessor t :type $-keyword)
   (global-type-definitions nil :accessor t)
   (global-objects nil :accessor t)))

(defmethod add-subobject ((obj data-definition) (subobj global-type-definition))
  (nconcf (global-type-definitions-of obj) (list subobj)))

(defmethod add-subobject ((obj data-definition) (subobj global-object))
  (nconcf (global-objects-of obj) (list subobj)))

(defun name-with-namespace (name namespace)
  (if (and namespace (symbolp name))
      (cons namespace name)
      name))

(defmethod read-return-value ((defs data-definition))
  (flet ((with-namespace (name)
           (name-with-namespace name (namespace-of defs))))
    (awhen (global-type-definitions-of defs)
      (incf *known-types-version*)
      (dolist (type it)
        (setf (assoc-value *known-types* (with-namespace (type-name-of type)) :test #'equal) type)))
    (awhen (global-objects-of defs)
      (incf *known-globals-version*)
      (dolist (type it)
        (setf (assoc-value *known-globals* (with-namespace (name-of type)) :test #'equal) type))))
  (values `(read-return-value ,defs) defs))

