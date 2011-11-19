;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(def (class* e) memory-extent (data-chunk)
  ((mirror :reader t)
   (mapping :reader t)))

(def (class* e) section-extent (memory-extent)
  ((section :reader t)))

(def (class* e) memory-mirror ()
  ((process :reader t)
   (lock (make-recursive-lock "Memory Mirror"))
   (null-extent :reader t)
   (extents nil :accessor t)
   (extent-map (make-chunk-table) :reader t)
   (section-map (make-chunk-table) :reader t)))

(defmethod intialize-instance :after ((mirror memory-mirror))
  (setf (slot-value mirror 'null-extent)
        (make-instance 'memory-extent
                       :start-address 0 :length 0 :mapping nil :mirror mirror
                       :data-bytes (make-array 0 :element-type 'uint8))))

(defmethod mirror-of ((mirror memory-mirror)) mirror)

(defmethod executable-of ((mirror memory-mirror))
  (executable-of (process-of mirror)))

(defmethod resolve-extent-for-addr ((mirror memory-mirror) addr)
  (with-recursive-lock-held ((lock-of mirror))
    (or (lookup-chunk (extent-map-of mirror) addr)
        (lookup-chunk (section-map-of mirror) addr)
        (null-extent-of mirror))))

(defmethod resolve-extent-for-addr ((ext memory-extent) addr)
  (if (< -1 (- addr (start-address-of ext)) (length-of ext))
      ext
      (resolve-extent-for-addr (mirror-of ext) addr)))

(defmethod get-bytes-for-addr ((mirror memory-mirror) addr size)
  (awhen (resolve-extent-for-addr mirror addr)
    (get-bytes-for-addr it addr size)))

(defmethod get-bytes-for-addr ((ext memory-extent) addr size)
  (let* ((start (start-address-of ext))
         (offset (- addr start))
         (bias (start-offset-of ext)))
    (when (<= 0 offset (- (length-of ext) size))
      (values (data-bytes-of ext) (+ offset bias) (- start bias)))))

(defun get-memory-bytes (memory addr size)
  (multiple-value-bind (bytes offset)
      (get-bytes-for-addr memory addr size)
    (when bytes
      (parse-bytes bytes offset size))))

(defun get-memory-integer (memory addr size &key signed?)
  (multiple-value-bind (bytes offset)
      (get-bytes-for-addr memory addr size)
    (when bytes
      (parse-int bytes offset size :signed? signed?))))

;; Mirror synchronization

(defun delete-memory-extent (mirror extent)
  (setf (slot-value extent 'cl-linux-debug.code-info::data-bytes) nil)
  (removef (extents-of mirror) extent)
  (trees:delete (start-address-of extent) (extent-map-of mirror)))

(defun ensure-memory-extent (mirror mapping)
  (bind ((start (memory-mapping-start-addr mapping))
         (len (- (memory-mapping-end-addr mapping) start)))
    (or (lookup-chunk (extent-map-of mirror) start)
        (let ((new-ext (make-instance 'memory-extent
                                      :mirror mirror
                                      :mapping mapping
                                      :start-address start
                                      :length len
                                      :data-bytes (make-array len :element-type 'uint8))))
          (trees:insert new-ext (extent-map-of mirror))
          (push new-ext (extents-of mirror))
          new-ext))))

(defun ensure-section-extent (mirror sect)
  (trees:insert (make-instance 'section-extent
                               :mirror mirror
                               :section sect :mapping (mapping-of sect)
                               :start-address (start-address-of sect)
                               :length (length-of sect)
                               :data-bytes (data-bytes-of sect)
                               :start-offset (start-offset-of sect))
                (section-map-of mirror)))

(defun sync-mirror-to-mappings (mirror mappings)
  (with-recursive-lock-held ((lock-of mirror))
    (dolist (ext (extents-of mirror))
      (let* ((start (start-address-of ext))
             (mapping (find start mappings :key #'memory-mapping-start-addr)))
        (if (null mapping)
            (delete-memory-extent mirror ext)
            (let ((msize (- (memory-mapping-end-addr mapping) start)))
              (when (/= msize (length-of ext))
                (setf (slot-value ext 'cl-linux-debug.code-info::data-bytes)
                      (make-array msize :element-type 'uint8))
                (setf (slot-value ext 'length) msize))
              (setf (slot-value ext 'mapping) mapping)))))
    (dolist (sect (sections-of (executable-of mirror)))
      (unless (null (data-bytes-of sect))
        (ensure-section-extent mirror sect)))
    (loop for map in mappings
       when (and (memory-mapping-writable? map)
                 (not (memory-mapping-shared? map))
                 (not (aand (memory-mapping-file-path map)
                            (starts-with-subseq "/dev/" it))))
       collect (ensure-memory-extent mirror map))))

(def-debug-task do-refresh-mirror (mirror)
  (let* ((process (process-of mirror))
         (mappings (process-memory-maps process)))
    (with-any-thread-suspended (process thread)
      (dolist (ext (sync-mirror-to-mappings mirror mappings))
        (read-process-data thread (start-address-of ext) (data-bytes-of ext))))))

(defgeneric refresh-memory-mirror (mirror)
  (:method ((mirror memory-mirror))
    (call-debug-task #'do-refresh-mirror mirror)))

(defun make-memory-mirror (process &optional (type 'memory-mirror))
  (let ((mirror (make-instance type :process process)))
    (refresh-memory-mirror mirror)
    mirror))

(defgeneric get-memory-global (memory name)
  (:method ((memory memory-mirror) name)
    (let* ((type (lookup-global-in-context memory name)))
      (when type
        (awhen (or (offset-of type)
                   (let* ((symbols (find-regions-by-name
                                    (executable-of memory)
                                    (get-$-field-name name :no-namespace? t)))
                          (addr-sym (find-if
                                     (lambda (x) (typep (origin-of x) 'executable-region-object))
                                     symbols)))
                     (when addr-sym (start-address-of addr-sym))))
          (make-memory-ref memory it type
                           :parent :globals :key name))))))
