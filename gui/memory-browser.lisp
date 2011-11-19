;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.gui)

(def (class* e) memory-object-tree (object-tree-view)
  ((widget :reader t)
   (memory :reader t)
   (info-label :reader t)))

(def (class* e) memory-object-node (object-node)
  ((ref :reader t)
   (ref-value nil :accessor t)
   (master-node :reader t)
   (col-offset)
   (col-name)
   (col-type)
   (col-value)
   (col-info)
   (col-comment)))

(defmethod initialize-instance :after ((node memory-object-node) &key)
  (unless (ref-value-of node)
    (setf (ref-value-of node) ($ (ref-of node) t))))

(def-column-slot col-offset memory-object-node)
(def-column-slot col-name memory-object-node)
(def-column-slot col-type memory-object-node)
(def-column-slot col-value memory-object-node)
(def-column-slot col-info memory-object-node)
(def-column-slot col-comment memory-object-node)

(defmethod start-address-of ((node memory-object-node))
  (start-address-of (ref-of node)))

(defmethod length-of ((node memory-object-node))
  (length-of (ref-of node)))

(defmethod col-offset-of ((node memory-object-node))
  (let ((master (master-node-of node))
        (cur (start-address-of node)))
    (if (eq (ref-of master) (ref-of node))
        (format-hex-offset cur)
        (format-hex-offset (- cur (start-address-of master)) :force-sign? t))))

(defgeneric field-name-of-type (type key)
  (:method ((type data-field) key)
    (aif (name-of type) (get-$-field-name it) (call-next-method)))
  (:method ((type global-type-definition) key)
    (aif (type-name-of type) (get-$-field-name it) (call-next-method)))
  (:method (type (key integer))
    key)
  (:method (type key)
    (if (typep type 'global-type-proxy)
        (field-name-of-type (effective-main-type-of type) key)
        (xml:xml-tag-name-string type))))

(defmethod col-name-of ((node memory-object-node))
  (field-name-of-type (memory-object-ref-type (ref-of node))
                      (memory-object-ref-parent-key (ref-of node))))

(defmethod col-type-of ((node memory-object-node))
  (let ((type (effective-main-type-of (ref-of node))))
    (typecase type
      (global-type-definition
       (get-$-field-name (type-name-of type)))
      (t (xml:xml-tag-name-string type)))))

(defmethod col-value-of ((node memory-object-node))
  (let ((ref (ref-of node)))
    (format-ref-value-by-type (memory-object-ref-type ref) ref (ref-value-of node))))

(defmethod col-comment-of ((node memory-object-node))
  (atypecase (comment-of (effective-main-type-of (ref-of node)))
    (comment (xml::content it))
    (null "No comment specified.")
    (t it)))

(defun describe-address-info (addr info)
  (let* ((section (awhen (section-of info)
                    (format nil "~A~A in ~A"
                            (section-name-of it)
                            (format-hex-offset (- addr (start-address-of it)) :force-sign? t)
                            (pathname-name (path-of (image-of (origin-of it)))))))
         (heap (awhen (malloc-chunk-range-of info)
                 (format nil "heap ~A~@[~A~] (~A bytes)"
                         (format-hex-offset (car it))
                         (when (/= addr (car it))
                           (format-hex-offset (- addr (car it)) :force-sign? t))
                         (format-hex-offset (- (cdr it) (car it))))))
         (region (awhen (region-of info)
                   (format nil "~A~@[~A~]"
                           (or (symbol-name-of it)
                               (format-hex-offset (start-address-of it)))
                           (when (/= addr (start-address-of it))
                             (format-hex-offset (- addr (start-address-of it)) :force-sign? t)))))
         (items (remove-if #'null (list region section heap))))
    (when items
      (format nil "~{~A~^ ~}" items))))

(defmethod col-info-of ((node memory-object-node))
  (let ((rv (ref-value-of node)))
    (cond ((and (typep rv 'memory-object-ref)
                (not (eq rv (ref-of node))))
           (or (describe-address-info (start-address-of rv)
                                      (get-address-object-info (memory-of (view-of node))
                                                               (start-address-of rv)))
               "unknown area"))
          ((typep rv 'integer)
           (format-hex-offset rv))
          (t ""))))

(defun construct-memory-object-tree (tree width-request height-request)
  (let* ((view (tree-view-of tree))
         (scroll (make-instance 'scrolled-window
                                :hscrollbar-policy :automatic
                                :vscrollbar-policy :automatic))
         (label (make-instance 'label :label "No object" :xalign 0.0 :yalign 0.5))
         (v-box (make-instance 'v-box)))
    (add-model-columns tree
                       #'col-offset-of #'col-name-of #'col-type-of #'col-value-of
                       #'col-info-of #'col-comment-of)
    (setf (widget-width-request view) width-request
          (widget-height-request view) height-request
          (tree-view-tooltip-column view) 5
          (tree-view-fixed-height-mode view) t)
    (box-pack-start v-box label :expand nil)
    (box-pack-start v-box scroll :expand t)
    (container-add scroll view)
    (flet ((add-column (id title min-width xalign)
             (let ((column (make-instance 'tree-view-column :title title
                                          :min-width min-width
                                          :resizable t))
                   (renderer (make-instance 'cell-renderer-text :text "A text" :xalign xalign)))
               (setf (tree-view-column-sizing column) :fixed)
               (tree-view-column-pack-start column renderer)
               (tree-view-column-add-attribute column renderer "text" id)
               (tree-view-append-column view column)
               column)))
      (add-column 0 "Address" 80 1.0)
      (setf (tree-view-expander-column view)
            (add-column 1 "Name" 200 0.0))
      (add-column 2 "Type" 100 0.0)
      (add-column 3 "Value" 100 0.0)
      (add-column 4 "Info" 100 0.0))
    (with-slots (widget tree-view info-label) tree
      (setf widget v-box
            tree-view view
            info-label label))
    #+nil(connect-signal window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
    #+nil(widget-show window)))

(defun layout-memory-object-nodes (ref master)
  (let* ((node (make-instance 'memory-object-node
                              :view (view-of master)
                              :ref ref :master-node master)))
    (dolist (child-ref (@ ref '*))
      (let ((child (layout-memory-object-nodes child-ref master)))
        (add-child node child)))
    (setf (expanded? node) (eq ref (ref-of master)))
    node))

(defun describe-object-info (ref info)
  (let ((start (format nil "~A at ~A:"
                       (field-name-of-type (memory-object-ref-type ref)
                                           (memory-object-ref-parent-key ref))
                       (format-hex-offset (start-address-of ref)))))
    (concatenate 'string start " "
                 (or (describe-address-info (start-address-of ref) info) "unknown"))))

(defun layout-memory-object-tree (view ref)
  (let* ((info (get-address-object-info (memory-of view) (start-address-of ref)))
         (master (make-instance 'memory-object-node :ref ref :view view))
         (tree (layout-memory-object-nodes ref master)))
    (setf (label-label (info-label-of view)) (describe-object-info ref info))
    (set-tree-view-root view master)
    (add-child master tree)))

(defmethod initialize-instance :after ((obj memory-object-tree) &key
                                       (width-request 640)
                                       (height-request 400))
  (construct-memory-object-tree obj width-request height-request))

(defun browse-object-in-new-window (memory ref &key (title "Memory Object"))
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :title title))
           (tree (make-instance 'memory-object-tree :memory memory)))
      (container-add window (widget-of tree))
      (layout-memory-object-tree tree ref)
      (widget-show window))))
