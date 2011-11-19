;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.gui)

(defmethod store-node-of ((node tree-node))
  node)

(defun tree-node-add-child (node child &optional index)
  (let ((sn (store-node-of node)))
    (tree-node-insert-at sn (store-node-of child)
                         (or index (length (tree-node-children sn))))))

(defun tree-node-remove-children (node)
  (let ((sn (store-node-of node)))
    (loop for len = (length (tree-node-children sn))
       while (> len 0) do (tree-node-remove-at sn (1- len)))))


(def (class* e) memory-object-tree ()
  ((widget :reader t)
   (tree-view :reader t)
   (tree-model (make-instance 'tree-lisp-store) :reader t)))

(def (class* e) memory-object-node ()
  ((ref :reader t)
   (store-node :reader t)
   (column-cache nil :accessor t)
   (master-node :reader t)))

(defun ensure-string (data)
  (if (stringp data) data (format nil "~S" data)))

(defun add-tree-columns (tree model &rest getters)
  (let ((size (length getters)))
    (labels ((get-cache (obj)
               (aif (column-cache-of obj) it
                    (setf (column-cache-of obj)
                          (make-array size :initial-element nil)))))
      (loop for i from -1 and cb in getters
         do (tree-lisp-store-add-column
             model "gchararray"
             (let ((i i) (cb cb))
               (lambda (obj)
                 (or (ignore-errors
                       (let ((cache (get-cache obj)))
                         (aif (svref cache i) it
                              (setf (svref cache i)
                                    (ensure-string (funcall cb tree obj))))))
                     "???"))))))))

(defmethod initialize-instance :after ((node memory-object-node) &key)
  (with-slots (store-node) node
    (unless (slot-boundp node 'store-node)
      (setf store-node (make-tree-node :item node)))))

(defmethod start-address-of ((node memory-object-node))
  (start-address-of (ref-of node)))

(defgeneric describe-node-offset (tree node)
  (:method (tree node)
    (let ((master (start-address-of (master-node-of node)))
          (cur (start-address-of node)))
      (if (= master cur)
          (format-hex-offset cur)
          (format-hex-offset (- cur master) :force-sign? t)))))

(defgeneric describe-node-name (tree node)
  (:method (tree node)
    (let* ((type (memory-object-ref-type (ref-of node)))
           (main-type (effective-main-type-of type)))
      (or (awhen (or (and (typep type 'data-field)
                          (name-of type))
                     (and (typep main-type 'global-type-definition)
                          (type-name-of main-type)))
            (get-$-field-name it))
          (xml:xml-tag-name-string main-type)))))

(defgeneric describe-node-type (tree node)
  (:method (tree node)
    (let ((type (effective-main-type-of (ref-of node))))
      (typecase type
        (global-type-definition
         (get-$-field-name (type-name-of type)))
        (t (xml:xml-tag-name-string type))))))

(defgeneric describe-node-value (tree node)
  (:method (tree node)
    (let ((value ($ (ref-of node) t)))
      (if (typep value 'memory-object-ref)
          ""
          (format nil "~S" value)))))

(defun construct-memory-object-tree (tree width-request height-request)
  (let* ((model (tree-model-of tree))
         (scroll (make-instance 'scrolled-window
                                :hscrollbar-policy :automatic
                                :vscrollbar-policy :automatic))
         (view (make-instance 'tree-view
                              :width-request width-request
                              :height-request height-request
                              :headers-visible t :rules-hint t))
         (label (make-instance 'label :label "Test of status bar" :xalign 0.0 :yalign 0.5))
         (v-box (make-instance 'v-box)))
    (add-tree-columns tree model
                      #'describe-node-offset
                      #'describe-node-name
                      #'describe-node-type
                      #'describe-node-value)
    (setf (tree-view-model view) model
          (tree-view-tooltip-column view) 0)
    (connect-signal view "row-activated"
                    (lambda (tv path column)
                      (declare (ignore tv column))
                      (show-message (format nil "You clicked on row ~A" (tree-path-indices path)))))
    (box-pack-start v-box label :expand nil)
    (box-pack-start v-box scroll :expand t)
    (container-add scroll view)
    (flet ((add-column (id title min-width width xalign)
             (let ((column (make-instance 'tree-view-column :title title
                                          :min-width min-width :width width
                                          :resizable t))
                   (renderer (make-instance 'cell-renderer-text :text "A text" :xalign xalign)))
               (tree-view-column-pack-start column renderer)
               (tree-view-column-add-attribute column renderer "text" id)
               (tree-view-append-column view column)
               column)))
      (add-column 0 "Address" 80 80 1.0)
      (setf (tree-view-expander-column view)
            (add-column 1 "Name" 150 200 0.0))
      (add-column 2 "Type" 80 80 0.0)
      (add-column 3 "Value" 100 150 0.0))
    (with-slots (widget tree-view) tree
      (setf widget v-box
            tree-view view))
    #+nil(connect-signal window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
    #+nil(widget-show window)))

(defun layout-memory-object-nodes (ref master)
  (let* ((node (make-instance 'memory-object-node :ref ref :master-node master)))
    (dolist (child-ref (@ ref '*))
      (let ((child (layout-memory-object-nodes child-ref master)))
        (tree-node-add-child node child)))
    node))

(defun layout-memory-object-tree (tree ref)
  (let* ((root (tree-lisp-store-root (tree-model-of tree)))
         (master (make-instance 'memory-object-node :ref ref :store-node root))
         (tree (layout-memory-object-nodes ref master)))
    (tree-node-remove-children root)
    (tree-node-add-child root tree)))

(defmethod initialize-instance :after ((obj memory-object-tree) &key
                                       (width-request 500)
                                       (height-request 300))
  (construct-memory-object-tree obj width-request height-request))

(defun browse-object-in-new-window (ref &key (title "Memory Object"))
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :title title))
           (tree (make-instance 'memory-object-tree)))
      (container-add window (widget-of tree))
      (layout-memory-object-tree tree ref)
      (widget-show window))))
