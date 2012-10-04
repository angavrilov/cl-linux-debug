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

(defvar *annotation-file* nil)
(defvar *annotation-changed* nil)
(defvar *annotation-table* (make-hash-table :test #'equal))

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

(defun is-contained-item? (type)
  (when (typep type 'abstract-field)
    (let ((parent (effective-parent-of type)))
      (and (typep parent 'container-item)
           (not (typep parent 'global-object))
           (eq type (effective-contained-item-of parent))))))

(defgeneric type-field-sequence (type)
  (:method ((obj null)) nil)
  (:method ((obj abstract-field))
    (let* ((parent (if (slot-boundp obj 'effective-parent)
                       (effective-parent-of obj)))
           (pseq (type-field-sequence parent)))
      (if (and parent (is-contained-item? obj))
          (list* '* pseq)
          (nconc (cond ((name-of obj) (list (name-of obj)))
                       ((and (typep obj 'data-field)
                             (not (typep obj 'compound)))
                        (list '@)))
                 pseq))))
  (:method ((obj global-type-definition))
    (aif (type-name-of obj) (list it)
         (call-next-method)))
  (:method ((obj global-object))
    (list $global))
  (:method :around ((obj global-type-proxy-base))
    (type-field-sequence (effective-main-type-of obj))))

(defun format-field-seq (type)
  (format nil "~{~A~^.~}"
          (mapcar (lambda (x)
                    (case x
                      ((* @) (symbol-name x))
                      (otherwise (get-$-field-name x))))
                  (nreverse (type-field-sequence type)))))

(defgeneric effective-id-string-of (obj)
  (:method ((obj abstract-item))
    (if (slot-boundp obj 'effective-id-string)
        (slot-value obj 'effective-id-string)
        (setf (slot-value obj 'effective-id-string)
              (format-field-seq obj)))))

;; OS context

(defparameter *type-context* nil)

(def (class e) os-context ()
  ()
  (:documentation "OS context base class"))

(def (class e) os-context/linux (os-context)
  ())

(def (class e) os-context/gcc (os-context)
  ())

(def (class e) os-context/linux/gcc (os-context/linux os-context/gcc)
  ())

(def (class e) os-context/windows (os-context)
  ())

(def (class e) os-context/msvc6 (os-context)
  ())

(def (class e) os-context/msvc2005 (os-context)
  ())

(def (class e) os-context/msvc2010 (os-context)
  ())

(def (class e) os-context/windows/msvc6 (os-context/msvc6 os-context/windows)
  ())

(def (class e) os-context/windows/msvc2005 (os-context/msvc2005 os-context/windows)
  ())

(def (class e) os-context/windows/msvc2010 (os-context/msvc2010 os-context/windows)
  ())

(defgeneric os-type-of (context)
  (:method ((ctx os-context)) nil)
  (:method ((ctx os-context/linux)) $linux)
  (:method ((ctx os-context/windows)) $windows)
  (:method ((ctx os-context/windows/msvc6)) $windows-msvc6)
  (:method ((ctx os-context/windows/msvc2005)) $windows-msvc2005))

(defgeneric os-context-of (context)
  (:method ((ctx os-context)) ctx))

(defparameter *known-os-contexts*
  (macrolet ((ctx (name type)
               `(cons ,name (make-instance ',type))))
    (list (ctx $windows os-context/windows/msvc2010)
          (ctx $linux os-context/linux/gcc)
          (ctx $windows-msvc6 os-context/windows/msvc6)
          (ctx $windows-msvc2005 os-context/windows/msvc2005))))

;; Type layout

(defgeneric compute-effective-size (os-context obj)
  (:argument-precedence-order obj os-context)
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

(defgeneric compute-effective-alignment (os-context obj)
  (:argument-precedence-order obj os-context)
  (:method :around (context (obj data-item))
    (or (alignment-of obj) (call-next-method)))
  (:method (context (obj virtual-compound-item))
    (reduce #'max (effective-fields-of obj)
            :key #'effective-alignment-of
            :initial-value 1))
  (:method (context (obj unit-item))
    (max 1 (default-size-of obj)))
  (:method ((context os-context/gcc) (obj unit-item))
    (max 1 (min 4 (default-size-of obj)))))

(defgeneric compute-effective-has-pointers? (os-context obj)
  (:argument-precedence-order obj os-context)
  (:method (context (obj data-item))
    nil)
  (:method (context (obj virtual-compound-item))
    (let ((inherited (effective-inherited-child-of obj)))
      (loop for field in (effective-fields-of obj)
         when (and (not (eq field inherited))
                   (effective-has-pointers? field))
         return t
         finally (return
                   (when (and inherited (effective-has-pointers? inherited))
                     :inherited)))))
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

(defgeneric lookup-type-reference (type-context referrer name)
  (:method (context referrer (name null))
    (error "~A must have fields or TYPE-NAME" (class-name (class-of referrer))))
  (:method (context referrer name)
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

(defgeneric compute-offset-range (os-context obj)
  (:argument-precedence-order obj os-context)
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

(defgeneric inherited-base-size (os-context obj field)
  (:argument-precedence-order obj field os-context)
  (:method (context obj (field data-item))
    (effective-size-of field))
  (:method ((context os-context/gcc) obj (field data-item))
    (if (has-methods? field)
        (effective-max-offset-of field)
        (call-next-method))))

(defgeneric layout-fields (os-context obj fields)
  (:argument-precedence-order obj os-context fields)
  (:method :before (context (obj abstract-compound-item) fields)
    (dolist (field fields)
      (setf (effective-parent-of field) obj)
      (layout-type-rec context field)))
  (:method :before (context (obj container-item) fields)
    (declare (ignore fields))
    (let ((item (effective-contained-item-of obj)))
      (setf (effective-parent-of item) obj)
      (layout-type-rec context item)))
  (:method (context (obj virtual-compound-item) fields)
    (let ((offset 0)
          (inherited (effective-inherited-child-of obj)))
      (dolist (field fields)
        (setf offset (or (offset-of field)
                         (when (is-union-p obj) 0)
                         (align-up offset (effective-alignment-of field))))
        (setf (effective-offset-of field) offset)
        (incf offset
              (if (eq field inherited)
                  (inherited-base-size context obj field)
                  (effective-size-of field)))))))

(defgeneric compute-effective-fields (os-context obj)
  (:argument-precedence-order obj os-context)
  (:method (context (obj abstract-compound-item)) nil)
  (:method (context (obj abstract-real-compound-item))
    (fields-of obj))
  (:method :before (context (obj container-item))
    (setf (effective-contained-item-of obj)
          (cond ((awhen (pointer-type-of obj)
                   (when (or (fields-of obj)
                             (not (member (type-name-of obj) '(nil $pointer))))
                     (error "Cannot specify both fields/TYPE-NAME and POINTER-TYPE: ~A" obj))
                   (inherit-attributes
                    obj (make-instance 'pointer :syntax-parent obj :type-name it))))
                ((awhen (aand (type-name-of obj)
                              (lookup-type-reference *type-context* obj it))
                   (when (fields-of obj)
                     (error "Cannot specify both fields and TYPE-NAME: ~A" obj))
                   (inherit-attributes
                    obj (make-proxy-field obj it))))
                ((rest (fields-of obj))
                 (inherit-attributes
                  obj (make-instance 'compound :syntax-parent obj :fields (fields-of obj))))
                ((fields-of obj)
                 (first (fields-of obj)))
                (t
                 (make-instance 'padding :syntax-parent obj :default-size 4)))))
  (:method (context (obj container-item))
    nil)
  (:method :after (context (obj global-object))
    (setf (name-of (effective-contained-item-of obj)) (name-of obj))
    (when (and (typep (effective-contained-item-of obj) 'padding)
               (size-of obj))
      (setf (size-of (effective-contained-item-of obj)) (size-of obj)))))

(defgeneric special-code-helpers (obj)
  (:method-combination append)
  (:method append ((obj code-helper-mixin)) nil))

(defgeneric substitute-type-class (os-context obj)
  (:argument-precedence-order obj os-context)
  (:method (context obj) obj)
  (:method (context (obj proxifiable-item-mixin))
    (aif (type-name-of obj)
         (let ((defn (lookup-type-reference *type-context* obj (type-name-of obj))))
           (unless (can-proxify-for-type? obj defn)
             (error "Cannot refer with ~A to ~A" obj defn))
           (change-class obj (proxy-class-for obj defn)
                         :effective-main-type defn))
         (call-next-method))))

(defgeneric layout-type (os-context obj)
  (:argument-precedence-order obj os-context)
  (:method :after (context (obj abstract-item))
    (setf (effective-finalized? obj) t))
  (:method (context (obj data-item))
    (setf (effective-alignment-of obj)
          (compute-effective-alignment context obj)
          (effective-size-of obj)
          (compute-effective-size context obj)
          (effective-has-pointers? obj)
          (compute-effective-has-pointers? context obj)
          (values (effective-min-offset-of obj)
                  (effective-max-offset-of obj))
          (compute-offset-range context obj)))
  (:method (context (obj abstract-item)))
  (:method :before (context (obj abstract-compound-item))
    (let ((fields (compute-effective-fields context obj)))
      (setf (effective-fields-of obj) fields)
      (layout-fields context obj fields)))
  (:method :after (context (obj code-helper-mixin))
    (setf (effective-code-helpers-of obj)
          (append
           (mapcar (lambda (ch)
                     (cons (name-of ch)
                           (compile-helper (xml::content ch) :group? t)))
                   (append (code-helpers-of obj)
                           (auto-code-helpers obj)))
           (special-code-helpers obj))))
  (:method :around (context (obj global-type-proxy))
    (setf (effective-finalized? obj) t)))

(defun layout-type-rec (os-context obj)
  (layout-type os-context (substitute-type-class os-context obj)))

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

;; Annotations

(defun save-annotations ()
  (when (and *annotation-changed* *annotation-file*)
    (with-open-file (stream *annotation-file* :direction :output :if-exists :supersede)
      (loop for name in (sort (hash-table-keys *annotation-table*) #'string<)
         do (format stream "~S~{ ~S~}~%" name (gethash name *annotation-table*))))
    (setf *annotation-changed* nil)))

(defun open-annotations (filename &key create?)
  (save-annotations)
  (with-open-file (stream filename :direction :input
                          :if-does-not-exist (if create? :create :error))
    (setf *annotation-file* filename)
    (clrhash *annotation-table*)
    (loop for line = (read-line stream nil nil)
       while line
       do (awhen (read-from-string (concatenate 'string "(" line ")"))
            (check-type (first it) string)
            (setf (gethash (first it) *annotation-table*) (rest it))))))

(defun type-annotation (obj key &optional default)
  (getf (gethash (effective-id-string-of obj) *annotation-table*) key default))

(defun (setf type-annotation) (value obj key)
  (setf *annotation-changed* t)
  (setf (getf (gethash (effective-id-string-of obj) *annotation-table*) key) value))

;; Type enumeration

(defgeneric walk-types (type callback &key)
  (:method-combination progn)
  (:method progn ((type abstract-item) callback &key recurse-proxy?)
    (declare (ignore callback recurse-proxy?)))
  (:method progn ((type virtual-compound-item) callback &key)
    (dolist (sub (effective-fields-of type))
      (funcall callback sub)))
  (:method progn ((type global-type-proxy-base) callback &key recurse-proxy?)
    (when recurse-proxy?
      (funcall callback (effective-main-type-of type))))
  (:method progn ((type container-item) callback &key)
    (funcall callback (effective-contained-item-of type))))

(defun annotate-all (context key value &key filter (namespace nil ns-p))
  (labels ((recurse (type)
             (when (and (not (typep type 'global-type-proxy-base))
                        (or (null filter) (funcall filter type)))
               (setf (type-annotation type key) value))
             (walk-types type #'recurse)))
    (dolist (global *known-globals*)
      (when (or (not ns-p) (eq (namespace-by-name (car global)) namespace))
        (let* ((obj (lookup-global-in-context context (car global))))
          (recurse (effective-contained-item-of obj)))))
    (dolist (type *known-types*)
      (when (or (not ns-p) (eq (namespace-by-name (car type)) namespace))
        (recurse (lookup-type-in-context context (car type)))))))

