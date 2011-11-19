;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

;; Memory reference object

(def (structure e) memory-object-ref
  (memory nil)
  (address 0)
  (tag nil :type cons)
  (parent-ref nil)
  (parent-key nil))

(declaim (inline memory-object-ref-type))
(defun memory-object-ref-type (obj)
  (car (memory-object-ref-tag obj)))

(defmethod effective-main-type-of ((obj memory-object-ref))
  (effective-main-type-of (memory-object-ref-type obj)))

(defmethod start-address-of ((ref memory-object-ref))
  (memory-object-ref-address ref))

(defmethod length-of ((ref memory-object-ref))
  (effective-size-of (memory-object-ref-type ref)))

(defun address= (ref1 ref2)
  (= (start-address-of ref1) (start-address-of ref2)))

(defun address- (ref1 ref2)
  (- (start-address-of ref1) (start-address-of ref2)))

(defun format-field-seq (type)
  (format nil "~{~A~^.~}"
          (mapcar #'get-$-field-name (nreverse (type-field-sequence type)))))

(defmethod print-object ((ref memory-object-ref) stream)
  (print-unreadable-object (ref stream :identity t)
    (let ((type (memory-object-ref-type ref)))
      (format stream "REF ~A @~X: ~A"
              (xml-tag-name-string (effective-main-type-of type))
              (memory-object-ref-address ref)
              (ignore-errors (format-field-seq type))))))

(defgeneric lookup-type-in-context (context type-name)
  (:method :around (context type-name)
    (let ((*type-context* context))
      (call-next-method))))

(defgeneric lookup-global-in-context (context type-name)
  (:method :around (context type-name)
    (let ((*type-context* context))
      (call-next-method))))

(defgeneric layout-ad-hoc-in-context (context type-tree)
  (:method :around (context type-tree)
    (let ((*type-context* context))
      (call-next-method))
    type-tree))

(defgeneric resolve-extent-for-addr (extent address))
(defgeneric get-bytes-for-addr (extent address size))

(defun make-memory-ref (memory address type &key parent key local?)
  (let* ((extent (if local? memory (resolve-extent-for-addr memory address)))
         (real-type (if (is-$-keyword-namespace? type)
                        (lookup-type-in-context memory type)
                        (effective-main-type-of type)))
         (tag (effective-tag-of real-type)))
    (assert extent)
    (make-memory-object-ref :memory extent
                            :address address
                            :tag tag
                            :parent-ref parent
                            :parent-key (or key tag))))

(defun resolve-offset-ref (base address type key &key local?)
  (make-memory-ref (memory-object-ref-memory base) address type
                   :parent base :key key :local? local?))

(defun make-ad-hoc-memory-ref (memory address type-tree &key no-copy? parent key local?)
  (assert (not (effective-finalized? type-tree)))
  (let ((type (layout-ad-hoc-in-context
               memory (if no-copy? type-tree
                          (copy-data-definition type-tree)))))
    (make-memory-ref memory address type :parent parent :key key :local? local?)))

(declaim (inline get-bytes-for-ref))
(defun get-bytes-for-ref (ref size)
  (get-bytes-for-addr (memory-object-ref-memory ref)
                      (memory-object-ref-address ref)
                      size))

(defmacro with-bytes-for-ref ((vector offset ref size) &body code)
  `(multiple-value-bind (,vector ,offset)
       (get-bytes-for-ref ,ref ,size)
     (when ,vector
       ,@code)))

(defparameter *safe-dereference* nil)

;;
;; Basic dereferencing
;;

(defun identity-key? (key)
  (case key (($this $it $me $value :value) t)))

(defun offset-memory-reference (ref shift-count shift-step)
  (if (= shift-count 0)
      ref
      (aprog1 (copy-memory-object-ref ref)
        (incf (memory-object-ref-address it) (* shift-count shift-step))
        (when (integerp (memory-object-ref-parent-key it))
          (incf (memory-object-ref-parent-key it) shift-count)))))

(defgeneric %memory-ref-@ (type ref key)
  (:method :around ((type global-type-proxy) ref key)
    (%memory-ref-@ (effective-main-type-of type) ref key))
  (:method (type ref key)
    (if (identity-key? key)
        (%memory-ref-@ type ref t)
        (cerror "ignore" "Invalid field access: ~S in ~S" key ref)))
  (:method (type ref (key integer))
    (offset-memory-reference ref key (effective-size-of type)))
  (:method (type ref (key (eql '*)))
    (declare (ignore type ref))
    nil)
  (:method (type ref (key (eql t)))
    (declare (ignore type))
    ref))

(defgeneric %memory-ref-$ (type ref key)
  (:method :around ((type global-type-proxy) ref key)
    (%memory-ref-$ (effective-main-type-of type) ref key))
  (:method (type ref key)
    (let ((refs (%memory-ref-@ type ref key)))
      (if (eq key '*)
          refs
          ($ refs t))))
  (:method (type ref (key (eql t)))
    (declare (ignore type))
    ref)
  (:method (type ref (key (eql :comment)))
    (atypecase (comment-of type)
      (comment (xml::content it))
      (t it))))

(defmethod @ ((ref memory-object-ref) key &optional default)
  (or (%memory-ref-@ (memory-object-ref-type ref) ref key) default))

(defmethod $ ((ref memory-object-ref) key &optional default)
  (or (%memory-ref-$ (memory-object-ref-type ref) ref key) default))
