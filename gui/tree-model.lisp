;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.gui)

(def (class* e) object-node ()
  ((view :reader t)
   (store-node :reader t)
   (expanded? nil :reader t)
   (separator? nil :accessor t)))

(defmethod store-node-of ((node tree-node))
  node)

(defmethod separator? ((node null)) nil)

(defmethod separator? ((node tree-node))
  (separator? (tree-node-item node)))

(defgeneric object-node-of (node)
  (:method ((node tree-node))
    (tree-node-item node))
  (:method ((node object-node))
    node))

(defmethod initialize-instance :after ((node object-node) &key)
  (with-slots (store-node) node
    (unless (slot-boundp node 'store-node)
      (setf store-node (make-tree-node :item node)))))

(defgeneric parent-of (node)
  (:method ((node tree-node))
    (tree-node-item (tree-node-parent node)))
  (:method ((node object-node))
    (parent-of (store-node-of node))))

(defgeneric children-of (node)
  (:method ((node tree-node))
    (map 'list #'tree-node-item (tree-node-children node)))
  (:method ((node object-node))
    (children-of (store-node-of node))))

(defgeneric on-added-to-tree (node)
  (:method ((node tree-node))
    (on-added-to-tree (tree-node-item node)))
  (:method ((node object-node)) nil))

(defgeneric add-child (node child &optional index)
  (:method ((node object-node) child &optional index)
    (add-child (store-node-of node) child index))
  (:method ((node tree-node) child &optional index)
    (tree-node-insert-at node (store-node-of child)
                         (or index (length (tree-node-children node))))
    (on-added-to-tree child)))

(defgeneric child-index-of (node child)
  (:method (node (child integer))
    child)
  (:method ((node object-node) child)
    (child-index-of (store-node-of node) child))
  (:method ((node tree-node) (child object-node))
    (position child (tree-node-children node) :key #'tree-node-item)))

(defun find-child-path (node)
  (let ((path nil)
        (tn (store-node-of node)))
    (loop
       for cur = tn then parent
       and parent = (tree-node-parent tn) then (tree-node-parent parent)
       while parent
       do (push (position cur (tree-node-children parent)) path))
    path))

(defgeneric remove-child (node child)
  (:method ((node object-node) child)
    (remove-child (store-node-of node) child))
  (:method ((node tree-node) child)
    (tree-node-remove-at node (child-index-of node child))))

(defgeneric remove-all-children (node)
  (:method ((node object-node))
    (remove-all-children (store-node-of node)))
  (:method ((node tree-node))
    (loop for len = (length (tree-node-children node))
       while (> len 0) do (tree-node-remove-at node (1- len)))))

(defgeneric tree-child-at-index (node index)
  (:method ((node object-node) index)
    (tree-child-at-index (store-node-of node) index))
  (:method ((node tree-node) (index null))
    node)
  (:method ((node tree-node) (index integer))
    (tree-node-child-at node index))
  (:method ((node tree-node) (index cons))
    (tree-child-at-index (tree-node-child-at node (first index)) (rest index))))

(defun child-at-index (node index)
  (tree-node-item (tree-child-at-index node index)))

(defgeneric on-node-activated (node column)
  (:method ((node object-node) column) nil))

(defgeneric on-node-expanded (node)
  (:method :before ((node object-node))
    (setf (slot-value node 'expanded?) t))
  (:method ((node object-node)) nil))

(defgeneric on-node-collapsed (node)
  (:method :before ((node object-node))
    (setf (slot-value node 'expanded?) nil))
  (:method ((node object-node)) nil))

(def (class* e) object-tree-view ()
  ((tree-view (make-instance 'tree-view :headers-visible t :rules-hint t) :reader t)
   (tree-model (make-instance 'tree-lisp-store) :reader t)))

(defun node-expand-callback (root callback)
  (lambda (tv iter path)
    (declare (ignore tv iter))
    (funcall callback (child-at-index root (tree-path-indices path)))))

(defun node-column-callback (root callback)
  (lambda (tv path column)
    (declare (ignore tv))
    (funcall callback (child-at-index root (tree-path-indices path)) column)))

(defmethod initialize-instance :after ((tree object-tree-view) &key)
  (let* ((model (tree-model-of tree))
         (view (tree-view-of tree))
         (root (tree-lisp-store-root model)))
    (setf (tree-view-model view) model)
    #+nil
    (setf (tree-view-row-separator-func view)
          (lambda (model iter)
            (separator? (gtk::get-node-by-iter model iter))))
    (connect-signal view "row-activated" (node-column-callback root #'on-node-activated))
    (connect-signal view "row-expanded" (node-expand-callback root #'on-node-expanded))
    (connect-signal view "row-collapsed" (node-expand-callback root #'on-node-collapsed))))

(defun set-tree-view-root (view node)
  (check-type node object-node)
  (let ((new-children (coerce (tree-node-children (store-node-of node)) 'list))
        (old-root (tree-lisp-store-root (tree-model-of view))))
    (remove-all-children old-root)
    (remove-all-children node)
    (setf (tree-node-item old-root) node
          (slot-value node 'store-node) old-root)
    (dolist (child new-children)
      (add-child old-root child))
    node))

(defgeneric (setf expanded?) (value node)
  (:method (value (node object-node))
    (setf (slot-value node 'expanded?) value)
    (awhen (tree-node-tree (store-node-of node))
      (let* ((view (view-of node))
             (indices (find-child-path node))
             (path (make-instance 'tree-path)))
        (assert (eq it (tree-model-of view)))
        (setf (tree-path-indices path) indices)
        (if value
            (tree-view-expand-row (tree-view-of view) path nil)
            (tree-view-collapse-row (tree-view-of view) path))))))

(defmethod on-added-to-tree :after ((node object-node))
  (when (tree-node-tree (store-node-of node))
    (labels ((rec (node &aux (c (children-of node)))
               (when (and c (expanded? node))
                 (setf (expanded? node) t))
               (mapc #'rec c)))
      (rec node))))

(def (class* e) lazy-expanding-node (object-node)
  ((lazy-expanded? nil :reader t)))

(defgeneric on-lazy-expand-node (node))

(defmethod on-node-expanded :before ((node lazy-expanding-node))
  (unless (lazy-expanded? node)
    (setf (slot-value node 'lazy-expanded?) t)
    (within-main-loop
      (with-simple-restart (continue "Cancel expanding node")
        (on-lazy-expand-node node))
      (setf (expanded? node) t))))

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

(defun add-model-columns (tree &rest getters)
  (dolist (cb getters)
    (tree-lisp-store-add-column (tree-model-of tree) "gchararray"
                                (lambda (val)
                                  (with-safe-return (cb)
                                    (funcall cb val))))))
