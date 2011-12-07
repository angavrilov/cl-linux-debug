;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(def (class* e) type-context ()
  ((last-types-version 0 :accessor t)
   (processed-types (make-hash-table :test #'equal) :accessor t)
   (last-globals-version 0 :accessor t)
   (processed-globals (make-hash-table :test #'equal) :accessor t)
   (strong-dep-table (make-hash-table) :accessor t)
   (vtable-class-cache (make-hash-table :test #'equal) :accessor t)
   (known-classes nil :accessor t)
   (data-definition-files nil :accessor t)
   (os-type $linux :accessor t)))

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
    (unless (slot-boundp type 'effective-tag)
      (setf (effective-tag-of type) (cons type nil))))
  (:method ((type data-item) (old-copy data-item))
    (if (slot-boundp type 'effective-tag)
        (assert (eq (effective-tag-of type) (effective-tag-of old-copy)))
        (aprog1 (effective-tag-of old-copy)
          (setf (effective-tag-of type) it
                (car it) type
                (cdr it) nil))))
  (:method :around ((type global-type-proxy) old-copy)
    nil)
  (:method :around (type (old-copy global-type-proxy))
    (tag-type-tree type nil))
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

;; When re-processing changed types, hold temporary tables
(defparameter *new-processed-types* nil)
(defparameter *new-processed-globals* nil)

(defun do-layout-in-context (full-name ddef work-table old-table dep-table)
  (let* ((*cur-ctx-namespace* (namespace-by-name full-name))
         (*strong-ref-table* (make-hash-table))
         (old-def (if old-table (gethash full-name old-table)))
         (old-tag (if old-def (effective-tag-of old-def))))
    ;; Assign the root tag
    (if (and old-tag (not (typep old-def 'global-type-proxy)))
        (setf (effective-tag-of ddef) old-tag
              (car old-tag) ddef)
        (setf (effective-tag-of ddef) (list ddef)
              old-tag nil))
    ;; Layout
    (layout-type-rec ddef)
    ;(effective-size-of ddef) ; ensure strong ref on itself
    ;; Assign all tags
    (when old-tag
      (setf (cdr old-tag) nil)) ; forget attributes
    (tag-type-tree ddef old-def)
    ;; Update tables
    (when old-table
      (setf (gethash full-name old-table) ddef)
      (unless (eq work-table old-table)
        (setf (gethash full-name work-table) ddef))
      (when dep-table
        (setf (gethash ddef dep-table) (hash-table-keys *strong-ref-table*))
        (remhash old-def dep-table)))
    ddef))

(defmethod lookup-type-reference ((context type-context) obj name)
  (let ((full-name (name-with-namespace name *cur-ctx-namespace*))
        (table (or *new-processed-types*
                   (processed-types-of context))))
    (or (gethash full-name table)
        (assoc-value *types-in-processing* full-name :test #'equal)
        (awhen (assoc-value *known-types* full-name :test #'equal)
          (aprog1 (copy-data-definition it)
            (let ((*types-in-processing*
                   (list* (cons full-name it) *types-in-processing*)))
              (do-layout-in-context full-name it
                                    table (processed-types-of context)
                                    (strong-dep-table-of context)))))
        (call-next-method))))

(defmethod lookup-type-in-context ((context type-context) type-name)
  (let ((*type-context* context)
        (*cur-ctx-namespace* (namespace-by-name type-name)))
    (lookup-type-reference context nil type-name)))

(defmethod layout-ad-hoc-in-context ((context type-context) type-tree)
  (do-layout-in-context nil type-tree nil nil nil))

(defmethod lookup-global-in-context ((context type-context) full-name)
  (let ((*cur-ctx-namespace* (namespace-by-name full-name))
        (table (or *new-processed-globals*
                   (processed-globals-of context))))
    (or (gethash full-name table)
        (awhen (assoc-value *known-globals* full-name :test #'equal)
          (do-layout-in-context full-name (copy-data-definition it)
                                table (processed-globals-of context)
                                (strong-dep-table-of context)))
        (error "No such global: ~A" (get-$-field-name full-name)))))

(defgeneric get-vtable-class-name (context address))

(defmethod resolve-class-in-context ((context type-context) (address integer))
  (multiple-value-bind (rv found) (gethash address (vtable-class-cache-of context))
    (if found rv
        (setf (gethash address (vtable-class-cache-of context))
              (awhen (aand (get-vtable-class-name context address)
                           (assoc-value (known-classes-of context) it :test #'equal))
                (lookup-type-in-context context it))))))

(defgeneric compute-mangled-name (context type)
  (:method (context type) nil)
  (:method (context (type class-type))
    (let ((name (or (original-name-of type)
                    (get-$-field-name (type-name-of type)))))
      (ecase (os-type-of context)
        ($windows (or (windows-mangling-of type)
                      (format nil ".?AV~A@@" name)))
        ($linux (or (linux-mangling-of type)
                    (format nil "~A~A" (length name) name)))))))

(defun compute-stable-subset (obj-list dep-table)
  (let ((ssubset (make-hash-table))
        (changed? t))
    (loop while changed?
       do (setf changed? nil)
       do (loop for obj in obj-list
             when (and (not (gethash obj ssubset))
                       (loop for d in (gethash obj dep-table)
                          always (gethash d ssubset)))
             do (setf (gethash obj ssubset) t
                      changed? t)))
    ssubset))

(defun same-pairs (hash assoc)
  (loop for name being the hash-keys of hash using (hash-value def)
     when (eq (copy-origin-of def) (assoc-value assoc name :test #'equal))
     collect def))

(defun copy-stable (hash old-hash stable-map)
  (loop for name being the hash-keys of old-hash using (hash-value def)
     do (if (gethash def stable-map)
            (setf (gethash name hash) def)
            (format t "Would update ~A~%" (get-$-field-name name)))))

(defgeneric check-refresh-context (context)
  (:method :around ((context type-context))
    (reload-data-definitions context)
    (when (or (< (last-types-version-of context) *known-types-version*)
              (< (last-globals-version-of context) *known-globals-version*))
      (call-next-method)
      t))
  (:method ((context type-context))
    (let* ((same-objs (nconc (same-pairs (processed-types-of context) *known-types*)
                             (same-pairs (processed-globals-of context) *known-globals*)))
           (ssubset (compute-stable-subset same-objs (strong-dep-table-of context)))
           (changed? nil))
      ;; Update manglings
      (setf (known-classes-of context)
            (loop for (name . type) in *known-types*
               for mname = (compute-mangled-name context type)
               when mname collect (cons mname name)))
      ;; Check types
      (let ((*new-processed-types* (make-hash-table :test #'equal))
            (types (processed-types-of context)))
        (copy-stable *new-processed-types* types ssubset)
        (loop for name being the hash-keys in types
           unless (gethash name *new-processed-types*)
           do (with-simple-restart (continue "Skip type ~A" name)
                (setf changed? t)
                (lookup-type-in-context context name)))
        (setf (last-types-version-of context) *known-types-version*))
      ;; Check globals
      (let ((*new-processed-globals* (make-hash-table :test #'equal))
            (globals (processed-globals-of context)))
        (copy-stable *new-processed-globals* globals ssubset)
        (loop for name being the hash-keys in globals
           unless (gethash name *new-processed-globals*)
           do (with-simple-restart (continue "Skip global ~A" name)
                (setf changed? t)
                (lookup-global-in-context context name)))
        (setf (last-globals-version-of context) *known-globals-version*))
      ;; Finalize
      (when changed?
        (clrhash (vtable-class-cache-of context))))))

(defun load-data-definition (path)
  (let ((*package* (find-package :cl-linux-debug.data-xml)))
    (load path)))

(defun reload-data-definitions (context)
  (loop for rec in (data-definition-files-of context)
     for (path . date) = rec
     do (loop
           for cur-date = (file-write-date path)
           while (> cur-date date)
           do (setf (cdr rec) cur-date)
           do (restart-case
                  (progn
                    (format t "Loading ~A...~%" path)
                    (load-data-definition path)
                    (return))
                (retry () :report (lambda (s) (format s "Retry loading ~A" path)))
                (abort () :report (lambda (s) (format s "Abort loading ~A" path))
                       (return))))))

(defun register-data-definition (context path)
  (let ((cur-date (file-write-date path)))
    (unless cur-date
      (error "Could not find file: ~A" path))
    (unless (find path (data-definition-files-of context) :key #'car :test #'equal)
      (push (cons path cur-date) (data-definition-files-of context))
      (load-data-definition path))))
