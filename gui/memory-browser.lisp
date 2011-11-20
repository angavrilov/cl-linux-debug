;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.gui)

;; Core widget and node classes

(def (class* e) memory-object-tree (object-tree-view)
  ((widget :reader t)
   (memory :reader t)
   (info-label :reader t))
  (:default-initargs
      :column-accessors
      '(col-offset-of col-name-of col-type-of col-value-of col-info-of col-comment-of)))

(def (class* e) memory-object-node (object-node)
  ((ref :reader t)
   (ref-value nil :accessor t)
   (master-node :reader t)
   (col-offset nil)
   (col-name nil :writer t)
   (col-type nil)
   (col-value nil)
   (col-info nil)
   (col-comment nil)))

(defmethod initialize-instance :after ((node memory-object-node) &key)
  (unless (or (null (ref-of node)) (ref-value-of node))
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

;; Default column definitions

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
    (or (aif (cl-linux-debug.data-info::is-$-keyword? key)
             (cl-linux-debug.data-info::get-$-field-name key))
        (aif (type-name-of type) (get-$-field-name it))
        (call-next-method)))
  (:method (type (key integer))
    key)
  (:method (type key)
    (if (typep type 'global-type-proxy)
        (field-name-of-type (effective-main-type-of type) key)
        (xml:xml-tag-name-string type)))
  (:method :around ((type struct-compound-item) key)
    (format nil "{~A}" (call-next-method))))

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
  (format-ref-value (ref-of node) (ref-value-of node)))

(defmethod col-comment-of ((node memory-object-node))
  (atypecase (comment-of (effective-main-type-of (ref-of node)))
    (comment (xml::content it))
    (null "No comment specified.")
    (t it)))

(defmethod col-info-of ((node memory-object-node))
  (format nil "~{~A~^; ~}"
          (describe-ref-value (ref-of node) (ref-value-of node))))

;; Tree layout

(def (class* e) lazy-placeholder-node (memory-object-node)
  ()
  (:default-initargs :ref nil :ref-value nil :col-offset "" :col-name "Processing..."
                     :col-type "" :col-value "" :col-info "" :col-comment ""))

(defun make-lazy (node class &rest args)
  (when class
    (apply #'change-class node class args))
  (add-child node (make-instance 'lazy-placeholder-node :view (view-of node)) 0))

(def (class* e) pointer-object-node (memory-object-node lazy-expanding-node)
  ())

(defmethod on-lazy-expand-node ((node pointer-object-node))
  (bind ((child (elt (children-of node) 0))
         (ref (ref-value-of node))
         ((:values info r-start r-len)
          (get-address-info-range (memory-of (view-of node)) (start-address-of ref))))
    (declare (ignore info))
    (setf (col-name-of child) "<target>"
          (slot-value child 'ref) ref)
    (layout-children-in-range node (list ref) child r-start r-len)
    (remove-child node 0)))

(def (class* e) array-subgroup-node (lazy-placeholder-node lazy-expanding-node)
  ((items nil :reader t)))

(defun populate-array-subgroup (parent items master)
  (dolist (item items)
    (let ((child (layout-ref-tree-node item master)))
      (setf (col-name-of child)
            (ensure-string (memory-object-ref-parent-key item)))
      (add-child parent child))))

(defmethod on-lazy-expand-node ((node array-subgroup-node))
  (populate-array-subgroup node (items-of node) (or (master-node-of node) node))
  (remove-child node 0))

(defun make-array-subgroup (base items parent master)
  (aprog1
      (make-instance 'array-subgroup-node :view (view-of parent)
                     :master-node master :items items
                     :ref (first items) :ref-value nil
                     :col-name (format nil "~A..~A"
                                       base (+ base (length items) -1)))
    (make-lazy it nil)))

(def (class* e) array-object-node (memory-object-node lazy-expanding-node)
  ())

(defun add-array-contents (node items master)
  (let* ((placeholder (elt (children-of node) 0))
         (parent (if (> (length (children-of node)) 1)
                     (aprog1 (make-instance 'lazy-placeholder-node
                                            :view (view-of node) :col-name "<items>"
                                            :expanded? t)
                       (add-child node it 1))
                     node))
         (num-items (length items)))
    (if (<= num-items 100)
        (progn
          (setf (slot-value placeholder 'ref) (first items))
          (populate-array-subgroup parent items (or master placeholder)))
        (loop
           for base from 0 by 100
           for cnt = (min 100 (- num-items base))
           and rest = items then (subseq rest cnt)
           while (< base num-items)
           do (add-child parent
                         (make-array-subgroup base (subseq rest 0 cnt) node master))))
    (remove-child node 0)))

(defmethod on-lazy-expand-node ((node array-object-node))
  (let* ((items ($ (ref-of node) '@))
         (master (if (typep (memory-object-ref-type (ref-of node)) 'static-array)
                     (master-node-of node)
                     nil)))
    (add-array-contents node items master)))

(def (class* e) padding-node (memory-object-node lazy-expanding-node)
  ()
  (:default-initargs :col-type "" :col-value "" :col-comment ""))

(defmethod col-name-of ((node padding-node))
  (format nil "(~A=0x~X bytes)" (length-of node) (length-of node)))

(defmethod col-info-of ((node padding-node))
  (or (with-bytes-for-ref (vector offset (ref-of node) 1)
        (format nil "~{~2,'0X~^ ~}~A"
                (loop for i from 0 below (min 8 (length-of node))
                   and j from offset below (length vector)
                   collect (aref vector j))
                (if (> (length-of node) 8) "..." "")))
      "?"))

(defmethod on-lazy-expand-node ((node padding-node))
  (let ((items (guess-types-by-data (memory-of (view-of node)) (ref-of node))))
    (add-array-contents node items (master-node-of node))))

(defun layout-children-in-range (parent child-refs master &optional min-addr addr-range)
  (when child-refs
    (let* ((sorted (stable-sort child-refs #'< :key #'start-address-of))
           (handled-base (or min-addr (start-address-of (first sorted))))
           (master-ref (ref-of master))
           (master-addr (start-address-of master-ref))
           (master-padding? (typep (memory-object-ref-type master-ref) 'padding)))
      (labels ((advance-to (start size)
                 (when (< handled-base start)
                   (insert-padding handled-base start))
                 (setf handled-base (max handled-base (+ start size))))
               (insert-padding (start end)
                 (let* ((memory (memory-of (view-of parent)))
                        (type (make-instance 'padding :size (- end start)))
                        (padding-ref (make-ad-hoc-memory-ref memory start type :no-copy? t)))
                   (when (and master-padding? (eql start master-addr))
                     (setf (slot-value master 'ref) padding-ref))
                   (add-child parent (layout-ref-tree-node padding-ref master)))))
        (dolist (ref child-refs)
          (let* ((ref-type (memory-object-ref-type ref)))
            (if (and (typep ref-type 'padding)
                     (null (name-of ref-type)))
                (when (eq ref master-ref)
                  (advance-to (start-address-of ref) 0))
                (progn
                  (advance-to (start-address-of ref) (length-of ref))
                  (add-child parent (layout-ref-tree-node ref master))))))
        (when addr-range
          (advance-to (+ min-addr addr-range) 0))))))

(defgeneric expand-by-default? (type ref master)
  (:method ((type data-item) ref master) t)
  (:method ((type primitive-field) ref master) nil)
  (:method ((type container-item) ref master) nil)
  (:method ((type compound) ref master)
    (null (name-of type)))
  (:method ((type class-type) ref master) t)
  (:method ((type struct-compound-item) ref master)
    (eq ref (ref-of master))))

(defgeneric layout-ref-tree-node/type (type ref master)
  (:method ((type data-item) ref master)
    (let* ((node (make-instance 'memory-object-node
                                :view (view-of master)
                                :expanded? (expand-by-default? type ref master)
                                :ref ref :master-node master)))
      (layout-children-in-range node (@ ref '*) master
                                (start-address-of ref) (length-of ref))
      node))
  (:method ((type pointer) ref master)
    (aprog1 (call-next-method)
      (when (ref-value-of it)
        (make-lazy it 'pointer-object-node))))
  (:method ((type array-item) ref master)
    (aprog1 (call-next-method)
      (make-lazy it 'array-object-node)))
  (:method ((type padding) ref master)
    (aprog1 (make-instance 'padding-node
                           :view (view-of master)
                           :ref ref :master-node master)
      (make-lazy it nil))))

(defun layout-ref-tree-node (ref master)
  (layout-ref-tree-node/type (memory-object-ref-type ref) ref master))

(defun describe-object-info (ref info)
  (let ((start (format nil "~A at ~A:"
                       (field-name-of-type (memory-object-ref-type ref)
                                           (memory-object-ref-parent-key ref))
                       (format-hex-offset (start-address-of ref)))))
    (apply #'concatenate 'string start " "
           (or (describe-address-in-context info (start-address-of ref)) (list "unknown")))))

(defun layout-memory-object-tree (view ref)
  (bind ((master (make-instance 'memory-object-node :ref ref :view view))
         ((:values info r-start r-len)
          (get-address-info-range (memory-of view) (start-address-of ref))))
    (setf (label-label (info-label-of view)) (describe-object-info ref info))
    (layout-children-in-range master (list ref) master r-start r-len)
    (set-tree-view-root view master)))

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
      (add-column 0 "Address" 80 1.0 nil)
      (setf (tree-view-expander-column view)
            (add-column 1 "Name" 200 0.0 t))
      (add-column 2 "Type" 100 0.0 nil)
      (add-column 3 "Value" 100 0.0 nil)
      (add-column 4 "Info" 100 0.0 t))
    (with-slots (widget tree-view info-label) tree
      (setf widget v-box
            tree-view view
            info-label label))
    #+nil(connect-signal window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
    #+nil(widget-show window)))

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
