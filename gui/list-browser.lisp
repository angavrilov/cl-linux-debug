;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.gui)

(def (class* e) memory-object-list ()
  ((widget :reader t)
   (memory :reader t)
   (object-tree :reader t)
   (list-view (make-instance 'tree-view :headers-visible t :rules-hint t) :reader t)
   (list-model :reader t)
   (obj-set #() :accessor t)
   (obj-value-set #() :accessor t)))

(defun memory-object-list-select (list selection)
  (awhen (tree-selection-selected-rows selection)
    (let ((idx (first (tree-path-indices (first it)))))
      (when (and idx (< -1 idx (length (obj-set-of list))))
        (let* ((vref (aref (obj-value-set-of list) idx)))
          (layout-memory-object-tree (object-tree-of list)
                                     (if (typep vref 'memory-object-ref)
                                         vref
                                         (aref (obj-set-of list) idx))))))))

(defun populate-memory-object-list (list objects)
  (setf (obj-set-of list) (coerce objects 'vector)
        (obj-value-set-of list)
        (coerce (mapcar (lambda (x) (ignore-errors ($ x t))) objects) 'vector))
  (let ((store (list-model-of list)))
    (setf (tree-view-model (list-view-of list)) nil)
    (unwind-protect
         (progn
           (list-store-clear store)
           (loop for i from 0
              and obj across (obj-set-of list)
              and val across (obj-value-set-of list)
              for vfmt = (if (typep val 'memory-object-ref)
                             (format-hex-offset (start-address-of val))
                             (format-ref-value obj val))
              and vinfo = (format nil "~{~A~^; ~}" (describe-ref-value obj val))
              do (list-store-insert-with-values
                  store i i (ensure-string vfmt) (ensure-string vinfo))))
      (setf (tree-view-model (list-view-of list)) store))))

(defun construct-memory-object-list (list width-request height-request)
  (let* ((view (list-view-of list))
         (scroll (make-instance 'scrolled-window
                                :hscrollbar-policy :never
                                :vscrollbar-policy :automatic))
         (model (make-instance 'list-store :column-types '("gint" "gchararray" "gchararray")))
         (tree (make-instance 'memory-object-tree
                              :memory (memory-of list)
                              :width-request (round  (* width-request 0.6))
                              :height-request height-request))
         (h-box (make-instance 'h-paned))
         (selection (tree-view-selection view)))
    (setf (widget-width-request view) (round (* width-request 0.4))
          (widget-height-request view) height-request
          (tree-view-fixed-height-mode view) t
          (tree-view-model view) model
          (tree-selection-mode selection) :browse)
    (paned-pack-1 h-box scroll)
    (paned-pack-2 h-box (widget-of tree))
    (container-add scroll view)
    (connect-signal selection "changed" (lambda (s) (memory-object-list-select list s)))
    (flet ((add-column (id title min-width xalign expand?)
             (let ((column (make-instance 'tree-view-column :title title
                                          :min-width min-width
                                          :resizable t
                                          :expand expand?))
                   (renderer (make-instance 'cell-renderer-text :text "A text" :xalign xalign)))
               (setf (tree-view-column-sizing column) :fixed)
               (tree-view-column-pack-start column renderer)
               (tree-view-column-add-attribute column renderer "text" id)
               (tree-view-append-column view column)
               column)))
      (add-column 0 "Index" 50 0.0 nil)
      (add-column 1 "Value" 100 0.0 nil)
      (add-column 2 "Info" 100 0.0 t))
    (with-slots (widget object-tree list-view list-model) list
      (setf widget h-box
            object-tree tree
            list-view view
            list-model model))))

(defmethod initialize-instance :after ((obj memory-object-list) &key
                                       (width-request 800)
                                       (height-request 400))
  (construct-memory-object-list obj width-request height-request))

(defun browse-object-in-new-window (memory ref &key (title "Memory Object"))
  (within-main-loop
    (let* ((window (make-instance 'gtk-window :title title))
           (tree (if (listp ref)
                     (aprog1 (make-instance 'memory-object-list :memory memory)
                       (populate-memory-object-list it ref))
                     (aprog1 (make-instance 'memory-object-tree :memory memory)
                       (layout-memory-object-tree it ref)))))
      (container-add window (widget-of tree))
      (widget-show window))))
