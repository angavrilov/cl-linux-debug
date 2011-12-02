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

(defgeneric apply-expand-to-addr (node addr leaf-enum-cb)
  (:method :around ((node memory-object-node) addr leaf-enum-cb)
    (if (< -1 (- addr (start-address-of node)) (length-of node))
        (call-next-method)
        0))
  (:method ((node memory-object-node) addr leaf-enum-cb)
    (setf (expanded? node) t)
    (let* ((ccnt (loop for child across (children-of node)
                    summing (apply-expand-to-addr child addr leaf-enum-cb))))
      (if (= ccnt 0)
          (progn
            (funcall leaf-enum-cb node)
            1)
          ccnt)))
  (:method ((node real-memory-object-node) addr leaf-enum-cb)
    (if (and (typep (ref-type-of node) 'unit-item)
             (not (typep node 'padding-object-node)))
        (progn
          (funcall leaf-enum-cb node)
          1)
        (call-next-method))))

(defun populate-memory-object-tree (view ref &key expand-to-addr)
  (bind (((:values master info nodes) (layout-memory-object-tree view ref))
         (type (cl-linux-debug.data-info::memory-object-ref-tag ref))
         (cache (state-cache-of view))
         (select-set nil))
    (setf (label-label (info-label-of view))
          (describe-object-info ref (first nodes) info))
    (if expand-to-addr
        (apply-expand-to-addr master (+ (start-address-of ref) expand-to-addr)
                              (lambda (node) (push node select-set)))
        (awhen (gethash type cache)
          (apply-expand-from-tree master it)))
    (setf (gethash type cache) master)
    (set-tree-view-root view master)
    (when select-set
      (let ((lpath nil)
            (tview (tree-view-of view)))
        (dolist (node select-set)
          (setf lpath (tree-path-of-list (find-child-path node)))
          (tree-selection-select-path (tree-selection-of view) lpath))
        (tree-view-scroll-to-cell tview lpath (first (tree-view-columns tview)))))
    (first nodes)))

(defun memory-object-tree-ranges (node sv lv)
  (nconc (awhen (start-address-of node)
           (unless (and sv (<= 0 (- it sv) (- lv (length-of node))))
             (setf sv it lv (length-of node))
             (list (cons sv lv))))
         (reduce #'nconc
                 (map 'list (lambda (x) (memory-object-tree-ranges x sv lv))
                      (children-of node)))))

(defun refresh-memory-object-tree (view)
  (let ((ranges (memory-object-tree-ranges (tree-root-of view) nil nil)))
    (print ranges)
    (refresh-memory-mirror-ranges (memory-of view) ranges)
    (refresh-node-values (tree-root-of view))))

;; Callbacks

(defgeneric get-node-menu-items (node)
  (:method-combination append :most-specific-first)
  (:method append ((node memory-object-node)) nil))

(defmethod get-node-menu-items append ((node real-memory-object-node))
  (let ((memory (memory-of (view-of node))))
    (nconc
     (awhen (ref-links-of node)
       (loop for (name link) in it
          collect (list (format nil "Browse link ~A" name)
                        (lambda () (browse-object-in-new-window
                               memory link
                               :title (format nil "~A of ~A" name (col-name-of node)))))))
     (let ((info (get-address-object-info (memory-of (view-of node)) (start-address-of node))))
       (when (and info (malloc-chunk-range-of info))
         (list (list "Browse referencing objects"
                     (lambda () (browse-references-for memory node info)))))))))

(defmethod on-tree-node-activated ((view memory-object-browser) (node pointer-object-node) column)
  (awhen (ref-value-of node)
    (browse-object-in-new-window (memory-of view) it
                                 :title (col-name-of node))))

(defmethod get-node-menu-items append ((node pointer-object-node))
  (list (list "Browse target" (lambda () (on-tree-node-activated (view-of node) node nil)))))

(defmethod on-tree-node-activated ((view memory-object-browser) (node array-object-node) column)
  (awhen ($ (ref-of node) '@)
    (browse-object-in-new-window (memory-of view) it
                                 :title (col-name-of node))))

(defmethod get-node-menu-items append ((node array-object-node))
  (list (list "Browse items" (lambda () (on-tree-node-activated (view-of node) node nil)))))

(defmethod on-tree-button-press ((view memory-object-browser) (node memory-object-node) event)
  (awhen (and (eq (event-button-type event) :button-press)
              (eql (event-button-button event) 3)
              (get-node-menu-items node))
    (let* ((menu (make-instance 'menu)))
      (dolist (item-spec it)
        (let ((item (make-instance 'menu-item :label (first item-spec)))
              (callback (second item-spec)))
          (connect-signal item "activate"
                          (lambda (x) (declare (ignore x)) (funcall callback)))
          (menu-shell-append menu item)))
      (widget-show menu)
      (menu-popup menu :button (event-button-button event)
                  :activate-time (event-button-time event)))))

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
    (connect-signal v-box "key-press-event"
                    (lambda (v ev)
                      (declare (ignore v))
                      (when (eq (event-key-type ev) :key-press)
                        (case (event-key-keyval ev)
                          (65474 (refresh-memory-object-tree tree))
                          (otherwise
                           (format t "~A~%" (event-key-keyval ev)))))
                      nil))
    (print (widget-events v-box))
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
                                       (height-request 400))
  (construct-memory-object-tree obj width-request height-request))

(defgeneric browse-object-in-new-window (memory ref &key title)
  (:method (memory (ref memory-object-ref) &key title expand-to-addr)
    (within-main-loop
      (let* ((window (make-instance 'gtk-window))
             (view (make-instance 'memory-object-browser :memory memory))
             (root (populate-memory-object-tree view ref :expand-to-addr expand-to-addr)))
        (setf (gtk-window-title window)
              (format nil "Object~@[: ~A~]" (or title (if root (col-name-of root)))))
        (container-add window (widget-of view))
        (widget-show window)))))

(defun browse-references-for (memory node info)
  (loop for (ref offsets)
     in (get-chunk-range-refs memory (malloc-chunk-range-of info))
     append (loop for offset in offsets collect (list ref offset)) into items
     finally (browse-object-in-new-window
              memory (mapcar #'first items)
              :expand-to-addr (mapcar #'second items)
              :title (format nil "references to ~A at ~X"
                             (col-name-of node)
                             (start-address-of node)))))
