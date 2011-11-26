;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.gui)

(def (class* e) object-node ()
  ((view :reader t)
   (store-node :reader t)
   (parent nil :reader t)
   (children (make-array 0 :adjustable t :fill-pointer t) :reader t)
   (expanded? nil :reader t)
   (separator? nil :accessor t)))

(def (class* e) object-tree-view ()
  ((tree-view (make-instance 'tree-view :headers-visible t :rules-hint t) :reader t)
   (tree-model :reader t)
   (tree-root :reader t)
   (column-accessors :reader t)))

(defmethod separator? ((node null)) nil)

(defmethod initialize-instance :after ((node object-node) &key parent add-child-index)
  (when parent
    (setf (slot-value node 'view) (view-of parent)
          (slot-value node 'parent) nil))
  (when (typep (view-of node) 'object-node)
    (setf (slot-value node 'view) (view-of (view-of node))))
  (assert (typep (view-of node) 'object-tree-view))
  (when parent
    (add-child parent node add-child-index)))

(defgeneric build-node-tree (parent node index)
  (:method ((parent object-node) (node object-node) index)
    (let* ((view (view-of node))
           (values (mapcar (lambda (cb) (funcall cb node))
                           (column-accessors-of view))))
      (setf (slot-value node 'store-node)
            (apply #'tree-store-insert-with-values
                   (tree-model-of view) (store-node-of parent) index values))
      (loop for child across (children-of node) and i from 0
         do (build-node-tree node child i)))))

(defun build-added-subtree (node child index &key first?)
  (build-node-tree node child index)
  (when (expanded? node)
    (if first?
        (sync-expanded-state node)
        (when (expanded? child)
          (sync-expanded-state child)))))

(defgeneric add-child (node child &optional index)
  (:method ((node object-node) (child object-node) &optional index)
    (assert (null (parent-of child)))
    (assert (eq (view-of child) (view-of node)))
    (let* ((clen (length (children-of node)))
           (iv (or index clen)))
      (setf (slot-value child 'parent) node)
      (gtk::array-insert-at (children-of node) child iv)
      (when (slot-boundp node 'store-node)
        (build-added-subtree node child iv)))))

(defgeneric child-index-of (node child)
  (:method ((node object-node) (child integer))
    (assert (<= 0 child (1- (length (children-of node)))))
    child)
  (:method ((node object-node) (child object-node))
    (position child (children-of node))))

(defgeneric child-at-index (node index)
  (:method ((node object-node) (index integer))
    (aref (children-of node) index))
  (:method ((node object-node) (index null))
    node)
  (:method ((node object-node) (index list))
    (child-at-index (child-at-index node (first index)) (rest index))))

(defun find-child-path (node)
  (when (slot-boundp node 'store-node)
    (let ((path nil))
      (loop
         for cur = node then parent
         and parent = (parent-of node) then (parent-of parent)
         while parent
         do (push (child-index-of parent cur) path))
      path)))

(defgeneric destroy-node-tree (parent node)
  (:method ((parent object-node) (node object-node))
    (loop for child across (children-of node)
       do (destroy-node-tree node child))
    (let ((sn (store-node-of node)))
      (slot-makunbound node 'store-node)
      (tree-store-remove (tree-model-of (view-of node)) sn))))

(defgeneric remove-child (node child)
  (:method ((node object-node) child)
    (remove-child node (child-index-of node child)))
  (:method ((node object-node) (child integer))
    (let ((child-obj (aref (children-of node) child)))
      (assert (eq (parent-of child-obj) node))
      (when (slot-boundp child-obj 'store-node)
        (destroy-node-tree node child-obj))
      (setf (slot-value child-obj 'parent) nil)
      (gtk::array-remove-at (children-of node) child))))

(defgeneric remove-all-children (node)
  (:method ((node object-node))
    (loop for len = (length (children-of node))
       while (> len 0)
       do (remove-child node (1- len)))))

(defgeneric on-node-activated (node column)
  (:method ((node object-node) column) nil))

(defgeneric on-node-expanded (node)
  (:method :around ((node object-node))
    (unless (expanded? node)
      (setf (slot-value node 'expanded?) t)
      (sync-expanded-state node)
      (call-next-method)))
  (:method ((node object-node)) nil))

(defgeneric on-node-collapsed (node)
  (:method :around ((node object-node))
    (when (expanded? node)
      (setf (slot-value node 'expanded?) nil)
      (call-next-method)))
  (:method ((node object-node)) nil))

(defgeneric sync-expanded-state (node)
  (:method ((node object-node))
    (when (and (slot-boundp node 'store-node)
               (> (length (children-of node)) 0))
      (let* ((view (view-of node))
             (indices (find-child-path node))
             (path (make-instance 'tree-path)))
        (setf (tree-path-indices path) indices)
        (if (expanded? node)
            (progn
              (tree-view-expand-row (tree-view-of view) path nil)
              (loop for child across (children-of node)
                 do (when (expanded? child)
                      (sync-expanded-state child))))
            (tree-view-collapse-row (tree-view-of view) path))))))

(defgeneric (setf expanded?) (value node)
  (:method :around (value (node object-node))
    (unless (eq (not value) (not (expanded? node)))
      (call-next-method)))
  (:method (value (node object-node))
    (setf (slot-value node 'expanded?) value)
    (sync-expanded-state node)))

(defun node-expand-callback (tree callback)
  (lambda (tv iter path)
    (declare (ignore tv iter))
    (funcall callback (child-at-index (tree-root-of tree) (tree-path-indices path)))))

(defun node-column-callback (tree callback)
  (lambda (tv path column)
    (declare (ignore tv))
    (funcall callback (child-at-index (tree-root-of tree) (tree-path-indices path)) column)))

(defmethod initialize-instance :after ((tree object-tree-view) &key
                                       column-types column-accessors (root-class 'object-node))
  (let* ((types (or column-types (mapcar (constantly "gchararray") column-accessors)))
         (model (make-instance 'tree-store :column-types types))
         (root (make-instance root-class :view tree :store-node nil))
         (view (tree-view-of tree)))
    (assert (= (length types) (length column-accessors)))
    (with-slots (tree-model tree-root) tree
      (setf tree-model model
            tree-root root))
    (setf (tree-view-model view) model)
    #+nil
    (setf (tree-view-row-separator-func view)
          (lambda (model iter)
            (separator? (gtk::get-node-by-iter model iter))))
    (connect-signal view "row-activated" (node-column-callback tree #'on-node-activated))
    (connect-signal view "row-expanded" (node-expand-callback tree #'on-node-expanded))
    (connect-signal view "row-collapsed" (node-expand-callback tree #'on-node-collapsed))))

(defun set-tree-view-root (view node)
  (check-type node object-node)
  (let ((new-children (children-of node))
        (old-root (tree-root-of view)))
    (assert (not (eq node old-root)))
    (assert (eq view (view-of node)))
    (loop for child across (children-of old-root)
       do (destroy-node-tree old-root child))
    (slot-makunbound old-root 'store-node)
    (setf (slot-value view 'tree-root) node
          (slot-value node 'store-node) nil)
    (loop for child across new-children and i from 0
       do (build-added-subtree node child i))
    node))

(def (class* e) lazy-expanding-node (object-node)
  ((lazy-expanded? nil :reader t)
   (lazy-placeholder nil :reader t)))

(defun create-lazy-placeholder (node lazy-placeholder-class)
  (with-slots (lazy-placeholder) node
    (unless lazy-placeholder
      (setf lazy-placeholder
            (make-instance lazy-placeholder-class :parent node :add-child-index 0)))))

(defmethod initialize-instance :after ((node lazy-expanding-node) &key lazy-placeholder-class)
  (create-lazy-placeholder node lazy-placeholder-class))

(defmethod update-instance-for-different-class :after (old (node lazy-expanding-node)
                                                &key lazy-placeholder-class)
  (unless (lazy-expanded? node)
    (create-lazy-placeholder node lazy-placeholder-class)))

(defgeneric on-lazy-expand-node (node)
  (:method :before ((node lazy-expanding-node))
    (setf (slot-value node 'lazy-expanded?) t))
  (:method :after ((node lazy-expanding-node))
    (with-slots (lazy-placeholder) node
      (when (and lazy-placeholder
                 (eq (parent-of lazy-placeholder) node))
        (remove-child node lazy-placeholder))
      (setf lazy-placeholder nil))))

(defmethod on-node-expanded :before ((node lazy-expanding-node))
  (unless (lazy-expanded? node)
    (setf (slot-value node 'lazy-expanded?) t)
    (within-main-loop
      (with-simple-restart (continue "Cancel expanding node")
        (on-lazy-expand-node node)
        (sync-expanded-state node)))))

(defmethod (setf expanded?) :before (value (node lazy-expanding-node))
  (when (and value (not (lazy-expanded? node)))
    (on-lazy-expand-node node)))

(defun ensure-string (data)
  (if (stringp data) data (format nil "~S" data)))

(defparameter *safe-return-table* nil)

(defun try-return-by-tag (tag c)
  (declare (ignore c))
  (assoc-value *safe-return-table* tag))

(defun query-string-value ()
  (format *query-io* "Enter string to return: ")
  (list (read-line *query-io*)))

(defmacro with-safe-return ((tag) &body code)
  (with-unique-names (blk redo)
    `(block ,blk
       (tagbody
          ,redo
          (return-from ,blk
            (restart-case
                (handler-bind ((condition
                                (lambda (c) (awhen (try-return-by-tag ,tag c)
                                         (return-from ,blk it)))))
                  (aprog1 (progn ,@code)
                    (check-type it string)))
              (retry () (go ,redo))
              (return-value (v)
                :report "Return a string this one time"
                :interactive query-string-value v)
              (return-value-forever (v)
                :report "Return a string, and remember forever for this function."
                :interactive query-string-value
                (setf (assoc-value *safe-return-table* ,tag) v))))))))

(defmacro def-column-slot (name class)
  (let ((getter (if (ends-with-subseq "?" (symbol-name name))
                    name
                    (symbolicate name '#:-of))))
    `(defgeneric ,getter (node)
       (:method :around ((node ,class))
         (aif (slot-value node ',name) it
              (setf (slot-value node ',name)
                    (ensure-string (call-next-method))))))))
