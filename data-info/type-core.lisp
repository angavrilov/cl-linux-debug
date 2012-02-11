;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

;; Global state

(defvar *known-builtin-types* nil)

(defvar *known-types* nil)
(defvar *known-types-version* 0)

(defvar *known-globals* nil)
(defvar *known-globals-version* 0)

(defvar *known-symtables* nil)
(defvar *known-symtables-version* 0)

(defparameter *type-context* nil)

(defun align-up (offset alignment)
  (* alignment (ceiling offset alignment)))

;; Helper

(defparameter cl-linux-debug.data-xml::global nil)

(defmacro %group-all (&rest args)
  (if (rest args) `(list ,@args) (first args)))

(defun parse-helper (text)
  (when text
    (let* ((*package* (find-package :cl-linux-debug.data-xml))
           (str (concatenate 'string "(" text ")")))
      (read-from-string str))))

(defun compile-helper (text &key group?)
  (let* ((code (if (stringp text) (parse-helper text) text)))
    (compile nil `(lambda ($ $$)
                    (declare (ignorable $ $$))
                    ,@(if group? `((%group-all ,@code)) code)))))

(defun call-helper (cb ref base &key context-ref)
  (let ((cl-linux-debug.data-xml::global
         (get-context-of-memory (or context-ref ref))))
    (funcall cb ref base)))

(defun call-helper-if-found (node name ref base &key context-ref)
  (awhen (assoc-value (effective-code-helpers-of node) name)
    (ignore-errors (values (call-helper it ref base :context-ref context-ref) t))))

;; Proxy

(def (class* eas) global-type-proxy-base (data-field concrete-item)
  ((type-name nil :accessor t :type $-keyword-namespace)
   (effective-main-type-tag nil :accessor t))
  (:documentation "A proxy type object; used to stand in for global types as fields."))

(defmethod initialize-instance :after ((type global-type-proxy-base) &key effective-main-type)
  (when effective-main-type
    (setf (effective-main-type-tag-of type) (effective-tag-of effective-main-type))
    (setf (type-name-of type) (type-name-of effective-main-type))))

(defmethod update-instance-for-different-class :after
    (old (type global-type-proxy-base) &key effective-main-type)
  (when effective-main-type
    (setf (effective-main-type-tag-of type) (effective-tag-of effective-main-type))
    (setf (type-name-of type) (type-name-of effective-main-type))))

(defmethod copy-data-definition ((type global-type-proxy-base))
  (aprog1 (call-next-method)
    (setf (effective-main-type-tag-of it) (effective-main-type-tag-of type))))

(defparameter *strong-ref-table* nil)

(declaim (inline verify-finalized))
(defun verify-finalized (type ref)
  (assert (and type (effective-finalized? type)))
  (awhen *strong-ref-table*
    (pushnew ref (gethash type *strong-ref-table*))))

(macrolet ((delegate (name)
             `(defmethod ,name ((proxy global-type-proxy-base))
                (let ((base (effective-main-type-of proxy)))
                  (verify-finalized base proxy)
                  (,name base)))))
  (delegate public-type-name-of))

(def (class* eas) global-type-proxy (global-type-proxy-base)
  ()
  (:documentation "A proxy type object; used to stand in for global types as fields."))

(defmethod xml:xml-tag-name-symbol ((str global-type-proxy)) 'compound)

(defmethod initialize-instance :after ((type global-type-proxy) &key effective-main-type)
  (when effective-main-type
    (setf (effective-tag-of type) (effective-tag-of effective-main-type))))

(defmethod update-instance-for-different-class :after
    (old (type global-type-proxy) &key effective-main-type)
  (when effective-main-type
    (setf (effective-tag-of type) (effective-tag-of effective-main-type))))

(defgeneric effective-main-type-of (obj)
  (:method ((obj abstract-item)) obj)
  (:method ((obj global-type-proxy-base))
    (car (effective-main-type-tag-of obj))))

(defmethod copy-data-definition ((type global-type-proxy))
  (aprog1 (call-next-method)
    (setf (effective-tag-of it) (effective-tag-of type))))

(macrolet ((delegate (name)
             `(defmethod ,name ((proxy global-type-proxy))
                (let ((base (effective-main-type-of proxy)))
                  (verify-finalized base proxy)
                  (,name base)))))
  (delegate effective-size-of)
  (delegate effective-alignment-of)
  (delegate effective-has-pointers?)
  (delegate effective-min-offset-of)
  (delegate effective-max-offset-of)
  (delegate has-methods?))

(def (class* eas) enum/global (global-type-proxy-base enum-field integer-field)
  ())

(defmethod xml:xml-tag-name-symbol ((str enum/global)) 'enum)

(def (class* eas) flag-bit/enum-global (global-type-proxy-base abstract-enum-item bit-item)
  ())

(defmethod xml:xml-tag-name-symbol ((str flag-bit/enum-global)) 'flag-bit)

;; Misc

(defmacro tag-attr (tag attr &optional (default nil d-p))
  `(getf (cdr ,tag) ,attr ,@(if d-p (list default))))

(defgeneric type-field-sequence (type)
  (:method ((obj abstract-field))
    (aif (name-of obj) (list it)))
  (:method ((obj data-field))
    (nconc (call-next-method) (type-field-sequence (effective-parent-of obj))))
  (:method ((obj global-type-definition))
    (aif (type-name-of obj) (list it)
         (call-next-method)))
  (:method :around ((obj global-type-proxy-base))
    (type-field-sequence (effective-main-type-of obj))))

(defgeneric os-type-of (context))

;; Type layout

(defgeneric compute-effective-size (context obj)
  (:method :around (context (obj data-item))
    (or (size-of obj)
        (align-up (call-next-method)
                  (effective-alignment-of obj))))
  (:method (context (obj virtual-compound-item))
    (reduce #'max (effective-fields-of obj)
            :key (lambda (x) (+ (effective-offset-of x) (effective-size-of x)))
            :initial-value 0))
  (:method (context (obj unit-item))
    (default-size-of obj)))

(defgeneric compute-effective-alignment (context obj)
  (:method :around (context (obj data-item))
    (or (alignment-of obj) (call-next-method)))
  (:method (context (obj virtual-compound-item))
    (reduce #'max (effective-fields-of obj)
            :key #'effective-alignment-of
            :initial-value 1))
  (:method (context (obj unit-item))
    (max 1 (case (os-type-of context)
             ($linux (min 4 (default-size-of obj)))
             (otherwise (default-size-of obj))))))

(defgeneric compute-effective-has-pointers? (context obj)
  (:method (context (obj data-item))
    nil)
  (:method (context (obj virtual-compound-item))
    (some #'effective-has-pointers? (effective-fields-of obj)))
  (:method (context (obj compound-item))
    (and (not (is-union-p obj))
         (call-next-method)))
  (:method (context (obj container-item))
    (and (not (has-bad-pointers-p obj))
         (effective-has-pointers? (effective-contained-item-of obj))))
  (:method (context (obj primitive-field))
    nil)
  (:method (context (obj pointer))
    (not (typep (effective-contained-item-of obj) 'padding))))

(defgeneric lookup-type-reference (context referrer name)
  (:method (context referrer (name null))
    (declare (ignore context))
    (error "~A must have fields or TYPE-NAME" (class-name (class-of referrer))))
  (:method (context referrer name)
    (declare (ignore context))
    (aif (assoc-value *known-builtin-types* name)
         (make-instance it :syntax-parent referrer)
         (error "Unknown type name: ~A" name)))
  (:method (context (referrer unit-item) (name null))
    (make-instance 'padding :syntax-parent referrer :default-size 4)))

(defgeneric proxy-class-for (obj type)
  (:method (obj (type global-type-definition))
    'global-type-proxy)
  (:method ((obj enum) (type enum-type))
    'enum/global)
  (:method ((obj flag-bit) (type enum-type))
    'flag-bit/enum-global))

(defgeneric make-proxy-field (obj type)
  (:method (obj (type global-type-definition))
    (make-instance (proxy-class-for obj type) :syntax-parent obj
                   :effective-main-type type))
  (:method (obj (type unit-item))
    (assert (not (effective-finalized? type)))
    type))

(defgeneric compute-offset-range (context obj)
  (:method (context (obj data-item))
    (values 0 (effective-size-of obj)))
  (:method (context (obj struct-compound-item))
    (values 0 0))
  (:method :around (context (obj virtual-compound-item))
    (multiple-value-bind (min max)
        (call-next-method)
      (loop for item in (effective-fields-of obj)
         for offset = (effective-offset-of item)
         minimizing (+ offset (effective-min-offset-of item)) into minv
         maximizing (+ offset (effective-max-offset-of item)) into maxv
         finally (return (values (min min minv) (max max maxv))))))
  (:method (context (obj bitfield-item))
    (values 0 (effective-size-of obj)))
  (:method :around (context (obj bitfield-item))
    (multiple-value-bind (min max)
        (call-next-method)
      (unless (<= 0 min max (effective-size-of obj))
        (error "Bitfield contents not in bounds: ~A" obj))
      (values min max))))

(defgeneric inherited-base-size (obj field)
  (:method (obj (field data-item))
    (if (and (has-methods? field)
             (eq (os-type-of *type-context*) $linux))
        (effective-max-offset-of field)
        (effective-size-of field))))

(defgeneric layout-fields (obj fields)
  (:method :before ((obj abstract-compound-item) fields)
    (dolist (field fields)
      (setf (effective-parent-of field) obj)
      (layout-type-rec field)))
  (:method :before ((obj container-item) fields)
    (declare (ignore fields))
    (let ((item (effective-contained-item-of obj)))
      (setf (effective-parent-of item) obj)
      (layout-type-rec item)))
  (:method ((obj virtual-compound-item) fields)
    (let ((offset 0)
          (inherited (effective-inherited-child-of obj)))
      (dolist (field fields)
        (setf offset (or (offset-of field)
                         (when (is-union-p obj) 0)
                         (align-up offset (effective-alignment-of field))))
        (setf (effective-offset-of field) offset)
        (incf offset
              (if (eq field inherited)
                  (inherited-base-size obj field)
                  (effective-size-of field)))))))

(defgeneric compute-effective-fields (obj)
  (:method ((obj abstract-compound-item)) nil)
  (:method ((obj abstract-real-compound-item))
    (fields-of obj))
  (:method :before ((obj container-item))
    (setf (effective-contained-item-of obj)
          (cond ((awhen (aand (type-name-of obj)
                              (lookup-type-reference *type-context* obj it))
                   (when (fields-of obj)
                     (error "Cannot specify both fields and TYPE-NAME: ~A" obj))
                   (make-proxy-field obj it)))
                ((rest (fields-of obj))
                 (make-instance 'compound :syntax-parent obj :fields (fields-of obj)
                                :key-field (key-field-of obj)))
                ((fields-of obj)
                 (first (fields-of obj)))
                (t
                 (make-instance 'padding :syntax-parent obj :default-size 4)))))
  (:method ((obj container-item))
    nil)
  (:method :after ((obj global-object))
    (setf (name-of (effective-contained-item-of obj)) (name-of obj))))

(defgeneric special-code-helpers (obj)
  (:method-combination append)
  (:method append ((obj code-helper-mixin)) nil))

(defgeneric layout-type-rec (obj)
  (:method :after ((obj abstract-item))
    (setf (effective-finalized? obj) t))
  (:method ((obj data-item))
    (setf (effective-alignment-of obj)
          (compute-effective-alignment *type-context* obj)
          (effective-size-of obj)
          (compute-effective-size *type-context* obj)
          (effective-has-pointers? obj)
          (compute-effective-has-pointers? *type-context* obj)
          (values (effective-min-offset-of obj)
                  (effective-max-offset-of obj))
          (compute-offset-range *type-context* obj)))
  (:method ((obj abstract-item)))
  (:method :before ((obj abstract-compound-item))
    (let ((fields (compute-effective-fields obj)))
      (setf (effective-fields-of obj) fields)
      (layout-fields obj fields)))
  (:method :after ((obj code-helper-mixin))
    (setf (effective-code-helpers-of obj)
          (append
           (mapcar (lambda (ch)
                     (cons (name-of ch)
                           (compile-helper (xml::content ch) :group? t)))
                   (append (code-helpers-of obj)
                           (auto-code-helpers obj)))
           (special-code-helpers obj))))
  (:method :around ((obj global-type-proxy))
    (setf (effective-finalized? obj) t))
  (:method :around ((obj proxifiable-item-mixin))
    (aif (type-name-of obj)
         (let ((defn (lookup-type-reference *type-context* obj (type-name-of obj))))
           (unless (can-proxify-for-type? obj defn)
             (error "Cannot refer with ~A to ~A" obj defn))
           (layout-type-rec
            (change-class obj (proxy-class-for obj defn)
                          :effective-main-type defn)))
         (call-next-method))))

(defmethod slot-unbound (class (obj container-item) (slot (eql 'effective-element-size)))
  (let ((elt (effective-contained-item-of obj)))
    (setf (effective-element-size-of obj) (effective-size-of elt))))

(defun add-if-changed (table-sym cnt-sym name type)
  (let ((xml-fmt (format nil "~A" type))
        (old-obj (assoc-value (symbol-value table-sym) name :test #'equal)))
    (if (and old-obj (equal (effective-xml-form-of old-obj) xml-fmt))
        old-obj
        (prog1
            (setf (assoc-value (symbol-value table-sym) name :test #'equal) type)
          (setf (effective-xml-form-of type) xml-fmt)
          (incf (symbol-value cnt-sym))))))

(defmethod read-return-value ((defs data-definition))
  (flet ((with-namespace (name)
           (name-with-namespace name (namespace-of defs))))
    (awhen (global-type-definitions-of defs)
      (dolist (type it)
        (let* ((name (with-namespace (type-name-of type))))
          (add-if-changed '*known-types* '*known-types-version* name type))))
    (awhen (global-objects-of defs)
      (dolist (type it)
        (add-if-changed '*known-globals* '*known-globals-version*
                        (with-namespace (name-of type)) type)))
    (awhen (symbol-tables-of defs)
      (dolist (type it)
        (add-if-changed '*known-symtables* '*known-symtables-version*
                        (name-of type) type))))
  (values `(read-return-value ,defs) defs))
