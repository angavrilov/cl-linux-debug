;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

;; Global state

(defvar *known-builtin-types* nil)

(defvar *known-types* nil)
(defvar *known-types-version* 0)

(defvar *known-globals* nil)
(defvar *known-globals-version* 0)

(defvar *known-classes* nil)

(defparameter *type-context* nil)

(defun align-up (offset alignment)
  (* alignment (ceiling offset alignment)))

;; Helper

(defparameter cl-linux-debug.data-xml::global nil)

(defmacro %group-all (&rest args)
  (if (rest args) `(list ,@args) (first args)))

(defun compile-helper (text &key group?)
  (let* ((*package* (find-package :cl-linux-debug.data-xml))
         (str (concatenate 'string "(" text ")"))
         (code (read-from-string str)))
    (compile nil `(lambda ($ $$)
                    (declare (ignorable $ $$))
                    ,@(if group? `((%group-all ,@code)) code)))))

(defun call-helper (cb ref base &key context-ref)
  (let ((cl-linux-debug.data-xml::global
         (get-context-of-memory (or context-ref ref))))
    (funcall cb ref base)))

;; Proxy

(def (class* eas) global-type-proxy (data-field concrete-item)
  ((type-name nil :accessor t :type $-keyword-namespace)
   (effective-main-type :accessor t))
  (:default-initargs :effective-finalized? t)
  (:documentation "A proxy type object; used to stand in for global types as fields."))

(defmethod effective-main-type-of ((obj data-item)) obj)

(defmethod initialize-instance :after ((type global-type-proxy) &key effective-main-type)
  (when effective-main-type
    (setf (type-name-of type) (type-name-of effective-main-type))))

(defmethod copy-data-definition ((type global-type-proxy))
  (aprog1 (call-next-method)
    (setf (effective-main-type-of it) (effective-main-type-of type))))

(macrolet ((delegate (name)
             `(defmethod ,name ((proxy global-type-proxy))
                (let ((base (effective-main-type-of proxy)))
                  (assert (and base (effective-finalized? base)))
                  (,name base)))))
  (delegate effective-size-of)
  (delegate effective-alignment-of)
  (delegate effective-tag-of))

;; Misc

(defgeneric type-field-sequence (type)
  (:method ((obj data-item))
    (aif (name-of obj) (list it)))
  (:method ((obj data-field))
    (nconc (call-next-method) (type-field-sequence (effective-parent-of obj))))
  (:method ((obj global-type-definition))
    (aif (type-name-of obj) (list it)
         (call-next-method)))
  (:method :around ((obj global-type-proxy))
    (type-field-sequence (effective-main-type-of obj))))

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
    (max 1 (default-size-of obj))))

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

(defgeneric make-proxy-field (obj type)
  (:method (obj (type global-type-definition))
    (make-instance 'global-type-proxy :syntax-parent obj
                   :effective-main-type type))
  (:method (obj (type unit-item))
    (assert (not (effective-finalized? type)))
    type))

(defun lookup-ref-compound-target (obj)
  (when (type-name-of obj)
    (unless (and (null (fields-of obj))
                 (null (is-union-p obj)))
      (error "When TYPE-NAME is given, direct fields are not allowed."))
    (lookup-type-reference *type-context* obj (type-name-of obj))))

(defgeneric layout-fields (obj fields)
  (:method :before ((obj virtual-compound-item) fields)
    (dolist (field fields)
      (setf (effective-parent-of field) obj)
      (layout-type-rec field)))
  (:method :before ((obj container-item) fields)
    (declare (ignore fields))
    (let ((item (effective-contained-item-of obj)))
      (setf (effective-parent-of item) obj)
      (layout-type-rec item)))
  (:method ((obj virtual-compound-item) fields)
    (let ((offset 0))
      (dolist (field fields)
        (setf offset (or (offset-of field)
                         (when (is-union-p obj) 0)
                         (align-up offset (effective-alignment-of field))))
        (setf (effective-offset-of field) offset)
        (incf offset (effective-size-of field))))))

(defgeneric compute-effective-fields (obj)
  (:method ((obj virtual-compound-item)) nil)
  (:method ((obj compound-item))
    (fields-of obj))
  (:method :before ((obj container-item))
    (setf (effective-contained-item-of obj)
          (cond ((awhen (lookup-ref-compound-target obj)
                   (make-proxy-field obj it)))
                ((rest (fields-of obj))
                 (make-instance 'compound :syntax-parent obj :fields (fields-of obj)))
                ((fields-of obj)
                 (first (fields-of obj)))
                (t
                 (make-instance 'padding :syntax-parent obj :default-size 4)))))
  (:method ((obj container-item))
    nil))

(defgeneric layout-type-rec (obj)
  (:method :after ((obj data-item))
    (setf (effective-finalized? obj) t))
  (:method ((obj data-item))
    (setf (effective-alignment-of obj)
          (compute-effective-alignment *type-context* obj)
          (effective-size-of obj)
          (compute-effective-size *type-context* obj)))
  (:method :before ((obj virtual-compound-item))
    (let ((fields (compute-effective-fields obj)))
      (setf (effective-fields-of obj) fields)
      (layout-fields obj fields)))
  (:method :around ((obj global-type-proxy))
    nil)
  (:method :around ((obj compound))
    (aif (lookup-ref-compound-target obj)
         (progn
           (unless (and (null (size-of obj)) (null (alignment-of obj)))
             (error "COMPOUND with a TYPE-NAME can't have size."))
           (change-class obj 'global-type-proxy :effective-main-type it))
         (call-next-method)))
  (:method :after ((obj code-helper-mixin))
    (setf (effective-code-helpers-of obj)
          (mapcar (lambda (ch)
                    (cons (name-of ch)
                          (compile-helper (xml::content ch) :group? t)))
                  (code-helpers-of obj)))))

(defmethod slot-unbound (class (obj container-item) (slot (eql 'effective-element-size)))
  (let ((elt (effective-contained-item-of obj)))
    (setf (effective-element-size-of obj) (effective-size-of elt))))

(defmethod read-return-value ((defs data-definition))
  (flet ((with-namespace (name)
           (name-with-namespace name (namespace-of defs))))
    (awhen (global-type-definitions-of defs)
      (incf *known-types-version*)
      (dolist (type it)
        (let ((name (with-namespace (type-name-of type))))
          (setf (assoc-value *known-types* name :test #'equal) type)
          (awhen (and (typep type 'class-type)
                      (mangled-name-of type))
            (setf (assoc-value *known-classes* it :test #'equal) name)))))
    (awhen (global-objects-of defs)
      (incf *known-globals-version*)
      (dolist (type it)
        (setf (assoc-value *known-globals* (with-namespace (name-of type)) :test #'equal) type))))
  (values `(read-return-value ,defs) defs))

