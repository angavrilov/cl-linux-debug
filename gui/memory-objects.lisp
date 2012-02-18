;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.gui)

;; State colors

(defparameter *state-color-table*
  '((:unchecked . "magenta")
    (:aligned . "blue")
    (:verified . "#00A000")))

(defgeneric state-color-of (obj)
  (:method ((obj null))
    nil)
  (:method ((obj memory-object-ref))
    (state-color-of (memory-object-ref-type obj)))
  (:method ((obj abstract-item))
    (awhen (type-annotation obj :status)
      (or (assoc-value *state-color-table* it)
          "red")))
  (:method ((obj container-item))
    (or (call-next-method)
        (let ((citem (effective-contained-item-of obj)))
          (unless (typep citem 'global-type-proxy-base)
            (state-color-of citem))))))

;; Core widget and node classes

(def (class* e) memory-object-tree (object-tree-view)
  ((memory :reader t))
  (:default-initargs
      :column-types
      '("gchararray" "gchararray" "gchararray" "gchararray" "gchararray"
        "gchararray" "gchararray" "gint" "gchararray")
      :column-accessors
      '(col-offset-of col-name-of col-type-of col-value-of col-info-of
        col-comment-of col-row-color-of col-name-weight-of col-type-color-of)))

(def (class* e) memory-object-node (object-node address-chunk)
  ((master-node nil :reader t))
  (:default-initargs :start-address nil :length 0))

(defgeneric col-offset-of (node)
  (:method ((node memory-object-node))
    (let ((master (master-node-of node))
          (cur (start-address-of node)))
      (cond ((null cur)
             "")
            ((null master)
             (format-hex-offset cur))
            (t
             (format-hex-offset (- cur (start-address-of master)) :force-sign? t))))))

(defgeneric col-name-of (node))
(defgeneric col-type-of (node))
(defgeneric col-value-of (node))
(defgeneric col-info-of (node))
(defgeneric col-comment-of (node))

(defgeneric col-row-color-of (node)
  (:method ((node memory-object-node)) "black"))

(defgeneric col-type-color-of (node)
  (:method ((node memory-object-node))
    nil)
  (:method :around ((node memory-object-node))
    (or (call-next-method)
        (col-row-color-of node))))

(defgeneric col-name-weight-of (node)
  (:method ((node memory-object-node)) 400))

(defgeneric refresh-node-values (node)
  (:method ((node memory-object-node))
    (map nil #'refresh-node-values (children-of node))))

(def (class* e) memory-object-placeholder-node (memory-object-node)
  ((col-name "Processing..." :accessor t)
   (col-type "" :accessor t)
   (col-value "" :accessor t)
   (col-info "" :accessor t)
   (col-comment "" :accessor t)))

(def (class* e) lazy-memory-object-node (memory-object-node lazy-expanding-node)
  ()
  (:default-initargs :lazy-placeholder-class 'memory-object-placeholder-node))

(defun make-lazy (node class &rest args)
  (apply #'change-class node class :lazy-placeholder-class 'memory-object-placeholder-node args))

;; Real reference

(def (class* e) real-memory-object-node (memory-object-node)
  ((ref :reader t)
   (ref-type nil :accessor t)
   (ref-value nil :accessor t)
   (ref-prev-value nil :accessor t)
   (ref-info nil :accessor t)
   (ref-links nil :accessor t)))

(defmethod col-row-color-of ((node real-memory-object-node))
  (if (ref-prev-value-of node) "red"
      (call-next-method)))

(defmethod col-type-color-of ((node real-memory-object-node))
  (state-color-of (ref-of node)))

(defgeneric evaluate-node-ref (node)
  (:method ((node real-memory-object-node))
    (let* ((ref (ref-of node))
           (value ($ ref t)))
      (setf (ref-type-of node) (memory-object-ref-type ref)
            (ref-value-of node) value
            (ref-info-of node) (describe-ref-value ref value)
            (ref-links-of node) (get-ref-links ref value)
            (start-address-of node) (start-address-of ref)
            (length-of node) (length-of ref)))))

(defmethod refresh-node-values :before ((node real-memory-object-node))
  (setf (ref-prev-value-of node) (ref-value-of node))
  (evaluate-node-ref node)
  (if (and (value= (ref-prev-value-of node) (ref-value-of node))
             (or (not (typep (ref-value-of node) 'memory-object-ref))
                 (equal (format-ref-value (ref-of node) (ref-prev-value-of node))
                        (format-ref-value (ref-of node) (ref-value-of node)))))
      (setf (ref-prev-value-of node) nil)
      ;; A hack to make booleans work
      (when (null (ref-prev-value-of node))
        (setf (ref-prev-value-of node) "NIL")))
  (refresh-column-values node))

(defmethod initialize-instance :after ((node real-memory-object-node) &key)
  (evaluate-node-ref node))

(defmethod col-name-of ((node real-memory-object-node))
  (let* ((type (ref-type-of node))
         (ref (ref-of node))
         (key (memory-object-ref-parent-key ref))
         (parent (memory-object-ref-parent-ref ref))
         (ptype (if (typep parent 'memory-object-ref)
                    (memory-object-ref-type parent))))
    (cond ((integerp key)
           (format nil "[~A]"
                   (or (awhen (and (typep ptype 'array-item)
                                   $(car (effective-index-enum-tag-of ptype)).keys[key])
                         (get-$-field-name it))
                       key)))
          ((awhen (and (typep type 'data-field)
                       (name-of type))
             (get-$-field-name it)))
          ((when (is-$-keyword? key)
             (get-$-field-name key)))
          (t
           (col-type-of node)))))

(defmethod col-type-of ((node real-memory-object-node))
  (public-type-name-of (ref-type-of node)))

(defmethod col-value-of ((node real-memory-object-node))
  (ensure-string (format-ref-value (ref-of node) (ref-value-of node))))

(defun format-hex-info (ref count &key ellipsis? address? (length (length-of ref)))
  (when (integerp (start-address-of ref))
    (with-bytes-for-ref (vector offset ref 1)
      (let* ((bytes (loop for i from 0 below (min count length)
                       and j from offset below (length vector)
                       collect (aref vector j)))
             (chars (loop for c in bytes
                       collect (if (>= c 32) (code-char c) #\?))))
        (format nil "~@[~A: ~]~{~2,'0X~^ ~}~A (~A)"
                (if address? (format-hex-offset (start-address-of ref)))
                bytes (if (and ellipsis? (> length count)) "..." "")
                (coerce chars 'string))))))

(defmethod col-comment-of ((node real-memory-object-node))
  (format nil "~A~%~{~A~%~}~A~@[~%Old value: ~A~]"
          (format-hex-info (ref-of node) 16 :address? t)
          (ref-info-of node)
          (atypecase (comment-of (effective-main-type-of (ref-of node)))
            (comment (xml::content it))
            (null "No comment specified.")
            (t it))
          (awhen (ref-prev-value-of node)
            (format-ref-value (ref-of node) it))))

(defmethod col-info-of ((node real-memory-object-node))
  (format nil "~{~A~^; ~}" (ref-info-of node)))

;; Specific node types

;; Pointer

(def (class* e) pointer-object-node (real-memory-object-node lazy-memory-object-node)
  ())

(defmethod on-lazy-expand-node ((node pointer-object-node))
  (bind ((child (lazy-placeholder-of node))
         (ref (ref-value-of node))
         (ref-start (start-address-of ref))
         ((:values info r-start r-len)
          (get-address-info-range (memory-of (view-of node)) ref-start)))
    (when (typep (effective-main-type-of ref) 'padding)
      (awhen (aand (malloc-chunk-range-of info)
                   (cl-linux-debug.data-info::obj-type-of it))
        (when (= ref-start r-start)
          (setf ref (make-memory-ref (memory-object-ref-memory ref)
                                     ref-start it
                                     :parent (ref-of node) :key t)))))
    (setf (start-address-of child) ref-start)
    (if (and (is-array-p (memory-object-ref-type (ref-of node)))
             (> (length-of ref) 0))
        (add-array-contents node
                            (progn
                              (setf (memory-object-ref-parent-key ref) 0)
                              (loop with len = (length-of ref)
                                 for i from 0 below (floor (- r-len (- ref-start r-start)) len)
                                 collect (cl-linux-debug.data-info::offset-memory-reference
                                          ref i len)))
                            child)
        (layout-children-in-range node (list ref) child r-start r-len :root? t))))

;; Array

(def (class* e) array-subgroup-node (memory-object-placeholder-node lazy-memory-object-node)
  ((items nil :reader t)))

(defun populate-array-subgroup (parent items master)
  (dolist (item items)
    (layout-ref-tree-node parent item master)))

(defmethod on-lazy-expand-node ((node array-subgroup-node))
  (populate-array-subgroup node (items-of node) (or (master-node-of node) node)))

(defun item-address-range (items)
  (if items
      (let ((start (start-address-of (first items)))
            (last (car (last items))))
        (values start (- (+ (start-address-of last) (length-of last)) start)))
      (values 0 0)))

(defun make-array-subgroup (parent master items base-idx)
  (multiple-value-bind (start len) (item-address-range items)
    (make-instance 'array-subgroup-node :parent parent
                   :master-node master :items items
                   :start-address start
                   :length len
                   :col-name (format nil "~A..~A"
                                     base-idx (+ base-idx (length items) -1)))))

(def (class* e) array-object-node (real-memory-object-node lazy-memory-object-node)
  ())

(defun add-array-contents (node items master)
  (bind ((placeholder (lazy-placeholder-of node))
         ((:values start len) (item-address-range items))
         (parent (if (> (length (children-of node)) 1)
                     (make-instance 'memory-object-placeholder-node
                                    :parent node :col-name "<items>"
                                    :expanded? t :add-child-index 0
                                    :start-address start :length len)
                     node))
         (num-items (length items)))
    (when items
      (if (<= num-items 100)
          (progn
            (setf (start-address-of placeholder) start)
            (populate-array-subgroup parent items (or master placeholder)))
          (loop
             for base from 0 by 100
             for cnt = (min 100 (- num-items base))
             and rest = items then (subseq rest cnt)
             while (< base num-items)
             do (make-array-subgroup parent master (subseq rest 0 cnt) base))))))

(defmethod on-lazy-expand-node ((node array-object-node))
  (let* ((items (@ (ref-of node) '*))
         (master (if (typep (memory-object-ref-type (ref-of node)) 'static-array)
                     (master-node-of node)
                     nil)))
    (add-array-contents node items master)))

;; Padding

(def (class* e) padding-object-node (real-memory-object-node lazy-memory-object-node)
  ((guessed-items :accessor t)))

(defmethod col-row-color-of ((node padding-object-node)) "darkgray")

(defmethod col-name-of ((node padding-object-node))
  (format nil "(~A=0x~X bytes)" (length-of node) (length-of node)))

(defmethod col-info-of ((node padding-object-node))
  (or (format-hex-info (ref-of node) 8
                       :ellipsis? t :address? nil
                       :length (length-of node))
      "?"))

(defmethod on-lazy-expand-node ((node padding-object-node))
  (let ((items (guess-types-by-data (memory-of (view-of node)) (ref-of node))))
    (setf (guessed-items-of node) items)
    (add-array-contents node items (or (master-node-of node) node))))

(def (class* e) bit-padding-object-node (padding-object-node)
  ())

(defmethod col-name-of ((node bit-padding-object-node))
  (format nil "(~A bits)" (* (length-of node) 8)))

(defmethod col-info-of ((node bit-padding-object-node))
  (or (with-bits-for-ref (vector offset byte-size bit-offset bit-size)
          ((ref-of node) (min 4 (length-of node)))
        (format nil "~B~A"
                (ldb (byte bit-size bit-offset)
                     (parse-int vector offset byte-size))
                (if (> (length-of node) 4) "..." "")))
      "?"))

(defmethod on-lazy-expand-node ((node bit-padding-object-node))
  (let* ((ref (ref-of node))
         (memory (memory-object-ref-memory ref))
         (type (layout-ad-hoc-in-context (get-context-of-memory memory)
                                         (make-instance 'flag-bit)))
         (items (loop for p from (start-address-of node) by 1/8
                   and l from 0 below (length-of node) by 1/8
                   and i from 0
                   collect (make-memory-ref memory p type :parent ref :key i :local? t))))
    (add-array-contents node items (master-node-of node))))

;; Struct

(def (class* e) struct-object-node (real-memory-object-node lazy-memory-object-node)
  ((fields nil :accessor t)))

(defmethod evaluate-node-ref :after ((node struct-object-node))
  (setf (fields-of node) (@ (ref-of node) '@)))

(defmethod col-name-weight-of ((node struct-object-node)) 700)

(defmethod on-lazy-expand-node ((node struct-object-node))
  (let ((ref (ref-of node)))
    (layout-children-in-range node (fields-of node) (or (master-node-of node) node)
                              (start-address-of ref) (length-of ref))))

;; Recursive walk

(defun layout-children-in-range (parent child-refs master min-addr addr-range &key root?)
  (when child-refs
    (let* ((sorted (stable-sort child-refs #'< :key #'start-address-of))
           (handled-base (or min-addr (start-address-of (first sorted))))
           (master-addr (start-address-of master))
           (bitfield? (and (typep parent 'real-memory-object-node)
                           (ref-of parent)
                           (typep (effective-main-type-of (ref-of parent)) 'bitfield-item)))
           (out-children nil))
      (labels ((advance-to (start size)
                 (when (< handled-base start)
                   (insert-padding handled-base start))
                 (setf handled-base (max handled-base (+ start size))))
               (insert-padding (start end)
                 (cond (bitfield?
                        (insert-bit-padding start end))
                       ((and (integerp start) (integerp end))
                        (insert-normal-padding start end))
                       (t
                        (let* ((start-rnd (ceiling start))
                               (start-rnd4 (align-up start 4))
                               (end-rnd (floor end))
                               (end-rnd4 (logand end-rnd -4)))
                          (cond ((<= start start-rnd4 end-rnd4 end)
                                 (insert-bit-padding start start-rnd4)
                                 (insert-padding start-rnd4 end-rnd4)
                                 (insert-bit-padding end-rnd4 end))
                                ((and (<= start start-rnd end-rnd end) (integerp end))
                                 (insert-bit-padding start start-rnd)
                                 (insert-padding start-rnd end))
                                (t
                                 (insert-bit-padding start end)))))))
               (insert-padding-node (start end class)
                 (when (< start end)
                   (let* ((memory (memory-of (view-of parent)))
                          (type (make-instance 'padding :size (- end start)))
                          (padding-ref (make-ad-hoc-memory-ref memory start type :no-copy? t)))
                     (make-instance class :parent parent
                                    :master-node (if (and root? (= start master-addr)) nil master)
                                    :ref padding-ref))))
               (insert-normal-padding (start end)
                 (insert-padding-node start end 'padding-object-node))
               (insert-bit-padding (start end)
                 (insert-padding-node start end 'bit-padding-object-node)))
        (dolist (ref child-refs)
          (let* ((ref-type (memory-object-ref-type ref))
                 (ref-start (start-address-of ref)))
            (if (and (typep ref-type 'padding)
                     (null (name-of ref-type)))
                (when (= ref-start master-addr)
                  (advance-to (start-address-of ref) 0))
                (progn
                  (advance-to (start-address-of ref) (length-of ref))
                  (push (layout-ref-tree-node parent ref (if root? nil master))
                        out-children)))))
        (when addr-range
          (advance-to (+ min-addr addr-range) 0))
        (nreverse out-children)))))

(defun layout-fields (node ref master)
  (layout-children-in-range node (@ ref '@) (or master node)
                            (start-address-of ref) (length-of ref)))

(defgeneric layout-ref-tree-node/type (type parent ref master)
  (:method ((type data-item) parent ref master)
    (aprog1 (make-instance 'real-memory-object-node
                           :parent parent
                           :ref ref :master-node master)
      (layout-fields it ref master)))
  (:method ((type struct-compound-item) parent ref master)
    (aprog1
        (make-instance 'struct-object-node
                       :parent parent
                       :ref ref :master-node master)
      (setf (expanded? it)
            (or (null master)
                (consp (memory-object-ref-parent-key ref))
                (null (cdr (fields-of it)))
                (typep type 'class-type)))))
  (:method ((type pointer) parent ref master)
    (aprog1 (call-next-method)
      (when (ref-value-of it)
        (make-lazy it 'pointer-object-node))))
  (:method ((type array-item) parent ref master)
    (aprog1 (make-instance 'array-object-node
                           :parent parent
                           :ref ref :master-node master)
      (layout-fields it ref master)))
  (:method ((type padding) parent ref master)
    (make-instance 'padding-object-node
                   :parent parent
                   :ref ref :master-node master)))

(defun layout-ref-tree-node (parent ref master)
  (layout-ref-tree-node/type (memory-object-ref-type ref) parent ref master))

(defun layout-memory-object-tree (view ref &key no-auto-base?)
  (bind (((:values info r-start r-len)
          (get-address-info-range (memory-of view) (start-address-of ref)))
         (section (section-of info))
         (known? (or (malloc-chunk-range-of info) (region-of info)))
         (base-ref (or (if (and (null no-auto-base?)
                                (keywordp (memory-object-ref-parent-ref ref)))
                           (get-address-info-ref (memory-of view) info))
                       ref))
         (r-start (max (- (or r-start (start-address-of base-ref)) (if known? 0 4096))
                       (if section (start-address-of section) 0)))
         (r-len (min (+ (or r-len (length-of base-ref)) (if known? 0 8192))
                     (if section (length-of section) +uint32-mask+)))
         (master (make-instance 'memory-object-placeholder-node :view view
                                :col-name "ROOT"
                                :start-address (start-address-of base-ref)
                                :length (max (length-of base-ref)
                                             (if (and r-start r-len)
                                                 (- (+ r-start r-len) (start-address-of base-ref))
                                                 4)))))
    (values master info base-ref
            (layout-children-in-range master (list base-ref) master r-start r-len :root? t))))


