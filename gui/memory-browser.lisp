;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.gui)

;; Core widget and node classes

(def (class* e) memory-object-browser (memory-object-tree)
  ((widget :reader t)
   (info-label :reader t)
   (state-cache (make-hash-table :test #'eq) :reader t)))

;; Tree initialization

(defun describe-object-info (ref node info)
  (let ((start (format nil "~A at ~A:"
                       (if node (col-name-of node) "data")
                       (format-hex-offset (start-address-of ref)))))
    (apply #'concatenate 'string start " "
           (or (describe-address-in-context info (start-address-of ref)) (list "unknown")))))

(defun apply-expand-from-tree (tree old)
  (if (expanded? old)
      (progn
        (setf (expanded? tree) t)
        (map nil #'apply-expand-from-tree (children-of tree) (children-of old)))
      (setf (expanded? tree) nil)))

(defun populate-memory-object-tree (view ref)
  (bind (((:values master info nodes) (layout-memory-object-tree view ref))
         (type (cl-linux-debug.data-info::memory-object-ref-tag ref))
         (cache (state-cache-of view)))
    (setf (label-label (info-label-of view))
          (describe-object-info ref (first nodes) info))
    (awhen (gethash type cache)
      (apply-expand-from-tree master it))
    (setf (gethash type cache) master)
    (set-tree-view-root view master)
    (first nodes)))

;; Callbacks

(defmethod on-tree-node-activated ((view memory-object-browser) (node pointer-object-node) column)
  (awhen (ref-value-of node)
    (browse-object-in-new-window (memory-of view) it
                                 :title (col-name-of node))))

(defmethod on-tree-node-activated ((view memory-object-browser) (node array-object-node) column)
  (awhen ($ (ref-of node) '@)
    (browse-object-in-new-window (memory-of view) it
                                 :title (col-name-of node))))

;; Widget creation

(defun construct-memory-object-tree (tree width-request height-request)
  (let* ((view (tree-view-of tree))
         (scroll (make-instance 'scrolled-window
                                :hscrollbar-policy :automatic
                                :vscrollbar-policy :automatic))
         (label (make-instance 'label :label "No object" :xalign 0.0 :yalign 0.5))
         (v-box (make-instance 'v-box)))
    (setf (widget-width-request view) width-request
          (widget-height-request view) height-request
          (tree-view-tooltip-column view) 5
          (tree-view-fixed-height-mode view) t)
    (box-pack-start v-box label :expand nil)
    (box-pack-start v-box scroll :expand t)
    (container-add scroll view)
    (flet ((add-column (id title min-width xalign expand? &key weight? expander?)
             (let ((column (make-instance 'tree-view-column :title title
                                          :min-width min-width
                                          :resizable t
                                          :expand expand?))
                   (renderer (make-instance 'cell-renderer-text :text "A text" :xalign xalign)))
               (setf (tree-view-column-sizing column) :fixed)
               (tree-view-column-pack-start column renderer)
               (tree-view-column-add-attribute column renderer "text" id)
               (tree-view-column-add-attribute column renderer "foreground" 6)
               (when weight?
                 (tree-view-column-add-attribute column renderer "weight" 7))
               (tree-view-append-column view column)
               (when expander?
                 (setf (tree-view-expander-column view) column))
               column)))
      (add-column 0 "Address" 80 1.0 nil)
      (add-column 1 "Name" 150 0.0 t :weight? t :expander? t)
      (add-column 2 "Type" 100 0.0 nil)
      (add-column 3 "Value" 100 0.0 nil)
      (add-column 4 "Info" 200 0.0 t))
    (with-slots (widget tree-view info-label) tree
      (setf widget v-box
            info-label label))
    #+nil(connect-signal window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
    #+nil(widget-show window)))

(defmethod initialize-instance :after ((obj memory-object-browser) &key
                                       (width-request 800)
                                       (height-request 600))
  (construct-memory-object-tree obj width-request height-request))

(defgeneric browse-object-in-new-window (memory ref &key title)
  (:method (memory (ref memory-object-ref) &key title)
    (within-main-loop
      (let* ((window (make-instance 'gtk-window))
             (view (make-instance 'memory-object-browser :memory memory))
             (root (populate-memory-object-tree view ref)))
        (setf (gtk-window-title window)
              (format nil "Object~@[: ~A~]" (or title (if root (col-name-of root)))))
        (container-add window (widget-of view))
        (widget-show window)))))
