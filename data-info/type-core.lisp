;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(defun partition-into-groups (list key)
  "Cut the list into groups that begin with a non-nil key."
  (labels ((pick-nils (list key)
             (loop for head on list
                while (null (funcall key (first head)))
                collect (first head) into cv
                finally (return (values cv head))))
           (main-iter (list key)
             (when list
               (bind ((base-key (funcall key (first list)))
                      ((:values empty tail) (pick-nils (rest list) key)))
                 (list* (cons base-key (list* (first list) empty))
                        (main-iter tail key))))))
    (main-iter list key)))

(defun match-up-lists (main aux &key
                       (key-main #'identity) (key-aux #'identity)
                       (test #'eql))
  "For every item in the first list, find a match in the second one."
  (let ((main-groups (partition-into-groups main key-main))
        (aux-groups (partition-into-groups aux key-aux))
        (refuse nil))
    (loop for group in main-groups
       for match = (cond ((car group)
                          (assoc (car group) aux-groups :test test))
                         ((null (car (first aux-groups)))
                          (first aux-groups)))
       for main-seq = (cdr group)
       and match-seq = (cdr match)
       do (deletef aux-groups match)
       nconc (loop for a in main-seq and b = match-seq then (cdr b)
                collect (cons a (car b))
                finally (nconcf refuse (cdr b)))
       into result
       finally (return (values result (nconc refuse (mapcan #'cdr aux-groups)))))))

(defgeneric tag-type-tree (type old-copy)
  (:method ((type data-item) (old-copy null))
    (setf (effective-tag-of type) (cons type nil)))
  (:method ((type data-item) (old-copy data-item))
    (aprog1 (effective-tag-of old-copy)
      (setf (effective-tag-of type) it
            (car it) type)))
  (:method :around ((type global-type-proxy) old-copy)
    nil)
  (:method :around (type (old-copy global-type-proxy))
    (tag-type-tree type (effective-main-type-of old-copy)))
  (:method :before ((type container-item) old-copy)
    (tag-type-tree (effective-contained-item-of type)
                   (if (typep old-copy 'container-item)
                       (effective-contained-item-of old-copy))))
  (:method :before ((type compound-item) old-copy)
    (let ((matches (match-up-lists (effective-fields-of type)
                                   (if (typep old-copy 'compound-item)
                                       (effective-fields-of old-copy))
                                   :key-main #'name-of
                                   :key-aux #'name-of)))
      (dolist (pair matches)
        (tag-type-tree (car pair) (cdr pair))))))

(def (class* e) type-context ()
  ((last-types-version 0 :accessor t)
   (processed-types (make-hash-table :test #'equal) :accessor t)
   (last-globals-version 0 :accessor t)
   (processed-globals (make-hash-table :test #'equal) :accessor t)))

(defparameter *cur-ctx-namespace* nil)
(defparameter *types-in-processing* nil)
(defparameter *old-processed-types* nil)
(defparameter *old-processed-globals* nil)

(defun namespace-by-name (name &optional default)
  (if (consp name) (car name) default))

(defun do-layout-in-context (full-name ddef table old-table)
  (let ((*cur-ctx-namespace* (namespace-by-name full-name)))
    (layout-type-rec ddef)
    (tag-type-tree ddef (if old-table (gethash full-name old-table)))
    (setf (gethash full-name table) ddef)
    ddef))

(defmethod lookup-type-reference ((context type-context) obj name)
  (let ((full-name (name-with-namespace name *cur-ctx-namespace*))
        (table (processed-types-of context)))
    (or (gethash full-name table)
        (assoc-value *types-in-processing* full-name :test #'equal)
        (awhen (assoc-value *known-types* full-name :test #'equal)
          (aprog1 (copy-data-definition it)
            (let ((*types-in-processing*
                   (list* (cons full-name it) *types-in-processing*)))
              (do-layout-in-context full-name it table *old-processed-types*))))
        (call-next-method))))

(defgeneric lookup-type-in-context (context type-name)
  (:method ((context type-context) type-name)
    (let ((*type-context* context)
          (*cur-ctx-namespace* (namespace-by-name type-name)))
      (lookup-type-reference context nil type-name))))

(defgeneric lookup-global-in-context (context type-name)
  (:method ((context type-context) full-name)
    (let ((*type-context* context)
          (*cur-ctx-namespace* (namespace-by-name full-name))
          (table (processed-globals-of context)))
      (or (gethash full-name table)
          (awhen (assoc-value *known-globals* full-name :test #'equal)
            (do-layout-in-context full-name (copy-data-definition it)
                                  table *old-processed-globals*))
          (error "No such global: ~A" (get-$-field-name full-name))))))

(defgeneric check-refresh-context (context)
  (:method ((context type-context))
    (when (< (last-types-version-of context) *known-types-version*)
      (let ((*old-processed-types* (processed-types-of context)))
        (setf (processed-types-of context) (make-hash-table :test #'equal))
        (loop for name being the hash-keys in *old-processed-types*
           do (with-simple-restart (continue "Skip this type")
                (lookup-type-in-context context name))))
      (setf (last-types-version-of context) *known-types-version*))
    (when (< (last-globals-version-of context) *known-globals-version*)
      (let ((*old-processed-globals* (processed-globals-of context)))
        (setf (processed-globals-of context) (make-hash-table :test #'equal))
        (loop for name being the hash-keys in *old-processed-globals*
           do (with-simple-restart (continue "Skip this global")
                (lookup-global-in-context context name))))
      (setf (last-globals-version-of context) *known-globals-version*))))

(defstruct memory-object-ref
  (memory nil)
  (address 0)
  (tag nil :type cons)
  (parent-ref nil)
  (parent-key nil))

(declaim (inline memory-object-ref-type))
(defun memory-object-ref-type (obj)
  (car (memory-object-ref-tag obj)))

(defmethod start-address-of ((ref memory-object-ref))
  (memory-object-ref-address ref))

(defmethod length-of ((ref memory-object-ref))
  (effective-size-of (memory-object-ref-type ref)))

(defun address= (ref1 ref2)
  (= (start-address-of ref1) (start-address-of ref2)))

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

(defun format-field-seq (type)
  (format nil "~{~A~^.~}"
          (mapcar #'get-$-field-name (nreverse (type-field-sequence type)))))

(defmethod print-object ((ref memory-object-ref) stream)
  (print-unreadable-object (ref stream :identity t)
    (let ((type (memory-object-ref-type ref)))
      (format stream "REF ~A @~X: ~A"
              (xml-tag-name-string
               (typecase type
                 (global-type-proxy (effective-main-type-of type))
                 (t type)))
              (memory-object-ref-address ref)
              (format-field-seq type)))))

(defparameter *safe-dereference* nil)

(defmacro with-safe-dereference ((default) &body code)
  (with-unique-names (bname hbv)
    `(block ,bname
       (let ((,hbv *safe-dereference*)
             (*safe-dereference* nil))
         (handler-bind ((condition
                         (lambda (x)
                           (declare (ignore x))
                           (when ,hbv
                             (return-from ,bname ,default)))))
           (restart-case
               (progn ,@code)
             (continue ()
               (return-from ,bname ,default))))))))

(defgeneric resolve-extent-for-addr (extent address))
(defgeneric get-bytes-for-addr (extent address size))

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

(defun make-memory-ref (memory address type &key parent key local?)
  (awhen (if local? memory (resolve-extent-for-addr memory address))
    (let* ((real-type (typecase type
                        (global-type-proxy
                         (effective-main-type-of type))
                        ((or symbol cons)
                         (lookup-type-in-context memory type))
                        (t type)))
           (tag (effective-tag-of real-type)))
      (make-memory-object-ref :memory it
                              :address address
                              :tag tag
                              :parent-ref parent
                              :parent-key (or key tag)))))

(defun resolve-offset-ref (base address type key &key local?)
  (make-memory-ref (memory-object-ref-memory base) address type
                   :parent base :key key :local? local?))

(defun identity-key? (key)
  (case key (($this $it $me $value) t)))

(defgeneric auto-deref-type? (type)
  (:method ((type data-item)) nil)
  (:method ((type primitive-field)) t)
  (:method ((type pointer)) t)
  (:method ((type global-type-proxy))
    (auto-deref-type? (effective-main-type-of type))))

(defgeneric %memory-ref-@ (type ref key)
  (:method :around (type ref key)
    (with-simple-restart (continue "Skip this dereference")
      (call-next-method)))
  (:method :around ((type global-type-proxy) ref key)
    (%memory-ref-@ (effective-main-type-of type) ref key))
  (:method (type ref key)
    (if (identity-key? key)
        (%memory-ref-@ type ref t)
        (error "Invalid field access: ~S in ~S" key type)))
  (:method (type ref (key integer))
    (if (= key 0)
        ref
        (aprog1 (copy-memory-object-ref ref)
          (incf (memory-object-ref-address it)
                (* key (effective-size-of type)))
          (when (integerp (memory-object-ref-parent-key it))
            (incf (memory-object-ref-parent-key it) key)))))
  (:method (type ref (key (eql '*)))
    (declare (ignore type ref))
    nil)
  (:method (type ref (key (eql t)))
    (declare (ignore type))
    ref))

(defgeneric %memory-ref-$ (type ref key)
  (:method :around (type ref key)
    (with-simple-restart (continue "Skip this dereference")
      (call-next-method)))
  (:method :around ((type global-type-proxy) ref key)
    (%memory-ref-$ (effective-main-type-of type) ref key))
  (:method (type ref key)
    (let ((refs (%memory-ref-@ type ref key)))
      (if (eq key '*)
          refs
          ($ refs t))))
  (:method (type ref (key (eql t)))
    (declare (ignore type))
    ref))

(defmethod %memory-ref-$ ((type integer-field) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (parse-int vector offset size :signed? (syntax-int-signed? type)))))

(defmethod %memory-ref-$ ((type pointer) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (let ((ptr (parse-int vector offset size)))
        (when (/= ptr 0)
          (resolve-offset-ref ref ptr
                              (effective-contained-item-of type)
                              t))))))

(defmethod %memory-ref-$ ((type pointer) ref key)
  ($ (%memory-ref-$ type ref t) key))

(defgeneric array-base-dimensions (type ref))

(defun array-item-ref (type ref index &key (check-bounds? t))
  (multiple-value-bind (base size)
      (array-base-dimensions type ref)
    (when (and base
               (or (< -1 index size)
                   (not check-bounds?)))
      (let ((elt-size (effective-element-size-of type)))
        (values (resolve-offset-ref ref (+ base (* index elt-size))
                                    (effective-contained-item-of type)
                                    index)
                size
                elt-size)))))

(defmethod %memory-ref-$ ((type array-item) ref (key integer))
  ($ (array-item-ref type ref key) t))

(defun all-array-item-refs (type ref)
  (multiple-value-bind (first size step)
      (array-item-ref type ref 0)
    (when first
      (loop
         for i from 0 below size
         and addr from (memory-object-ref-address first) by step
         collect (aprog1 (copy-memory-object-ref first)
                   (setf (memory-object-ref-address it) addr
                         (memory-object-ref-parent-key it) i))))))

(defmethod %memory-ref-$ ((type array-item) ref (key (eql '*)))
  ($ (all-array-item-refs type ref) t))

(defmethod %memory-ref-$ ((type array-item) ref (key (eql $first)))
  (array-item-ref type ref 0 :check-bounds? nil))

(defmethod %memory-ref-$ ((type array-item) ref (key (eql $count)))
  (nth-value 1 (array-base-dimensions type ref)))

(defmethod array-base-dimensions ((type static-array) ref)
  (values (memory-object-ref-address ref) (count-of type)))

(defmethod %memory-ref-@ ((type static-array) ref (key (eql '*)))
  (all-array-item-refs type ref))

(defmethod %memory-ref-@ ((type struct-compound-item) ref (key symbol))
  (aif (find key (effective-fields-of type) :key #'name-of)
       (resolve-offset-ref ref (+ (memory-object-ref-address ref)
                                  (effective-offset-of it))
                           it key :local? t)
       (unless *safe-dereference*
         (cerror "ignore" "Unknown field ~A in ~A" key ref))))

(defmethod %memory-ref-@ ((type struct-compound-item) ref (key (eql '*)))
  (mapcar (lambda (it)
            (resolve-offset-ref ref (+ (memory-object-ref-address ref)
                                       (effective-offset-of it))
                                it (or (name-of it) it) :local? t))
          (effective-fields-of type)))

(defmethod @ ((ref memory-object-ref) key &optional default)
  (or (%memory-ref-@ (memory-object-ref-type ref) ref key) default))

(defmethod $ ((ref memory-object-ref) key &optional default)
  (or (%memory-ref-$ (memory-object-ref-type ref) ref key) default))

