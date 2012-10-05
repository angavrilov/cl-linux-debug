;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.gui)

(def (class* e) memory-object-list ()
  ((widget :reader t)
   (memory :reader t)
   (object-tree :reader t)
   (filter-entry :reader t)
   (list-view (make-instance 'tree-view :headers-visible t :rules-hint t) :reader t)
   (list-model :reader t)
   (filtered-model :reader t)
   (obj-subset nil :accessor t)
   (obj-set #() :accessor t)
   (obj-value-set #() :accessor t)
   (obj-addr-expand-set #() :accessor t)))

(defun memory-object-list-select (list selection)
  (awhen (tree-selection-selected-rows selection)
    (let ((idx (first (tree-path-indices (first it)))))
      (when (aand (obj-subset-of list) (< -1 idx (length it)))
        (setf idx (aref (obj-subset-of list) idx)))
      (when (and idx (< -1 idx (length (obj-set-of list))))
        (let* ((vref (aref (obj-value-set-of list) idx)))
          (populate-memory-object-tree (object-tree-of list)
                                       (if (typep vref 'memory-object-ref)
                                           vref
                                           (aref (obj-set-of list) idx))
                                       :expand-to-addr
                                       (aif (obj-addr-expand-set-of list)
                                            (aref it idx))))))))

(defun populate-store (list store subset)
  (let ((obj-set (obj-set-of list))
        (val-set (obj-value-set-of list))
        (memory (memory-of list)))
    (list-store-clear store)
    (loop for i from 0 below (if subset (length subset) (length obj-set))
       for idx = (if subset (aref subset i) i)
       for obj = (aref obj-set idx)
       for val = (aref val-set idx)
       for obj-info = (if (keywordp (memory-object-ref-parent-ref obj))
                          (describe-address-in-context
                           (get-address-object-info memory (start-address-of obj))
                           (start-address-of obj)))
       for vfmt = (if (typep val 'memory-object-ref)
                      (format nil "<~A>"
                              (format-hex-offset (start-address-of val)))
                      (format-ref-value obj val))
       and vinfo = (format nil "~{~A~^; ~}" (append (describe-ref-value obj val) obj-info))
       do (list-store-insert-with-values
           store i idx (ensure-string vfmt) (ensure-string vinfo)))))

(defun populate-memory-object-list (list objects &key expand-to-addr)
  (setf (obj-set-of list) (coerce objects 'vector)
        (obj-addr-expand-set-of list) (aif expand-to-addr (coerce it 'vector))
        (obj-value-set-of list)
        (coerce (mapcar (lambda (x) (ignore-errors ($ x t))) objects) 'vector)
        (obj-subset-of list) nil)
  (let ((store (list-model-of list)))
    (setf (tree-view-model (list-view-of list)) nil)
    (unwind-protect
         (populate-store list store nil)
      (setf (tree-view-model (list-view-of list)) store))))

(defun memory-object-list-filter (list filter)
  (let* ((all? (or (null filter)
                   (loop for c across filter always (eql c #\ )))))
    (if all?
        (setf (tree-view-model (list-view-of list)) (list-model-of list)
              (obj-subset-of list) nil)
        (let* ((store (filtered-model-of list))
               (helper (compile-helper filter))
               (obj-set (obj-set-of list))
               (val-set (obj-value-set-of list))
               (items (loop for i from 0 below (length obj-set)
                         for val = (aref val-set i) and obj = (aref obj-set i)
                         when (let ((cl-linux-debug.data-info::*safe-dereference* t))
                                (call-helper helper val obj :context-ref obj))
                         collect i))
               (subset (coerce items 'vector)))
          (populate-store list store subset)
          (setf (tree-view-model (list-view-of list)) store
                (obj-subset-of list) subset)))))

(defun construct-memory-object-list (list width-request height-request)
  (let* ((view (list-view-of list))
         (scroll (make-instance 'scrolled-window
                                :hscrollbar-policy :never
                                :vscrollbar-policy :automatic))
         (model (make-instance 'list-store :column-types '("gint" "gchararray" "gchararray")))
         (model2 (make-instance 'list-store :column-types '("gint" "gchararray" "gchararray")))
         (tree (make-instance 'memory-object-browser
                              :memory (memory-of list)
                              :width-request (round  (* width-request 0.7))
                              :height-request height-request))
         (h-box (make-instance 'h-paned))
         (v-box (make-instance 'v-box))
         (fh-box (make-instance 'h-box))
         (entry (make-instance 'entry))
         (button (make-instance 'button :label "Filter"))
         (selection (tree-view-selection view)))
    (setf (widget-width-request view) (round (* width-request 0.3))
          (widget-height-request view) height-request
          (tree-view-fixed-height-mode view) t
          (tree-view-model view) model
          (tree-selection-mode selection) :browse
          (container-focus-child v-box) h-box)
    (box-pack-start v-box fh-box :expand nil)
    (box-pack-start v-box h-box)
    (paned-pack-1 h-box scroll)
    (paned-pack-2 h-box (widget-of tree))
    (box-pack-start fh-box entry)
    (box-pack-start fh-box button :expand nil)
    (container-add scroll view)
    (connect-signal selection "changed" (lambda (s) (memory-object-list-select list s)))
    (flet ((on-filter (s)
             (declare (ignore s))
             (handler-case
                 (memory-object-list-filter list (entry-text entry))
               (error (c)
                 (show-message (format nil "~A" c) :message-type :error)))))
      (connect-signal button "clicked" #'on-filter)
      (connect-signal entry "activate" #'on-filter))
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
    (with-slots (widget object-tree list-view list-model filter-entry filtered-model) list
      (setf widget v-box
            object-tree tree
            list-view view
            list-model model
            filter-entry entry
            filtered-model model2))))

(defmethod initialize-instance :after ((obj memory-object-list) &key
                                       (width-request 1000)
                                       (height-request 400))
  (construct-memory-object-list obj width-request height-request))

(defmethod browse-object-in-new-window (memory (ref list) &key title expand-to-addr)
  (within-main-loop
    (let* ((window (make-instance 'gtk-window))
           (view (make-instance 'memory-object-list :memory memory)))
      (populate-memory-object-list view ref :expand-to-addr expand-to-addr)
      (setf (gtk-window-title window)
            (format nil "List~@[: ~A~]" title))
      (container-add window (widget-of view))
      (widget-show window))))
