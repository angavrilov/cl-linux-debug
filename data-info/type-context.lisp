;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(def (class* e) type-context ()
  ((last-types-version 0 :accessor t)
   (processed-types (make-hash-table :test #'equal) :accessor t)
   (last-globals-version 0 :accessor t)
   (processed-globals (make-hash-table :test #'equal) :accessor t)
   (vtable-class-cache (make-hash-table :test #'equal) :accessor t)))

(defmethod get-context-of-memory ((context type-context)) context)

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
  (:method :before ((type virtual-compound-item) old-copy)
    (let ((matches (match-up-lists (effective-fields-of type)
                                   (if (typep old-copy 'compound-item)
                                       (effective-fields-of old-copy))
                                   :key-main #'name-of
                                   :key-aux #'name-of)))
      (dolist (pair matches)
        (tag-type-tree (car pair) (cdr pair))))))

(defparameter *cur-ctx-namespace* nil)
(defparameter *types-in-processing* nil)
(defparameter *old-processed-types* nil)
(defparameter *old-processed-globals* nil)

(defun do-layout-in-context (full-name ddef table old-table)
  (let ((*cur-ctx-namespace* (namespace-by-name full-name)))
    (layout-type-rec ddef)
    (tag-type-tree ddef (if old-table (gethash full-name old-table)))
    (when table
      (setf (gethash full-name table) ddef))
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

(defmethod lookup-type-in-context ((context type-context) type-name)
  (let ((*type-context* context)
        (*cur-ctx-namespace* (namespace-by-name type-name)))
    (lookup-type-reference context nil type-name)))

(defmethod layout-ad-hoc-in-context ((context type-context) type-tree)
  (do-layout-in-context nil type-tree nil nil))

(defmethod lookup-global-in-context ((context type-context) full-name)
  (let ((*cur-ctx-namespace* (namespace-by-name full-name))
        (table (processed-globals-of context)))
    (or (gethash full-name table)
        (awhen (assoc-value *known-globals* full-name :test #'equal)
          (do-layout-in-context full-name (copy-data-definition it)
                                table *old-processed-globals*))
        (error "No such global: ~A" (get-$-field-name full-name)))))

(defgeneric get-vtable-class-name (context address))

(defmethod resolve-class-in-context ((context type-context) (address integer))
  (multiple-value-bind (rv found) (gethash address (vtable-class-cache-of context))
    (if found rv
        (setf (gethash address (vtable-class-cache-of context))
              (awhen (aand (get-vtable-class-name context address)
                           (assoc-value *known-classes* it :test #'equal))
                (lookup-type-in-context context it))))))

(defgeneric check-refresh-context (context)
  (:method ((context type-context))
    ;; Check types
    (when (< (last-types-version-of context) *known-types-version*)
      (let ((*old-processed-types* (processed-types-of context)))
        (setf (processed-types-of context) (make-hash-table :test #'equal))
        (loop for name being the hash-keys in *old-processed-types*
           do (with-simple-restart (continue "Skip this type")
                (lookup-type-in-context context name))))
      (setf (last-types-version-of context) *known-types-version*)
      (clrhash (vtable-class-cache-of context)))
    ;; Check globals
    (when (< (last-globals-version-of context) *known-globals-version*)
      (let ((*old-processed-globals* (processed-globals-of context)))
        (setf (processed-globals-of context) (make-hash-table :test #'equal))
        (loop for name being the hash-keys in *old-processed-globals*
           do (with-simple-restart (continue "Skip this global")
                (lookup-global-in-context context name))))
      (setf (last-globals-version-of context) *known-globals-version*))))

(defun load-data-definition (path)
  (let ((*package* (find-package :cl-linux-debug.data-xml)))
    (load path)))
