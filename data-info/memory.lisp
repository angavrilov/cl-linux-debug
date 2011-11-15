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
   (extents nil :accessor t)
   (extent-map (make-chunk-table) :reader t)
   (section-map (make-chunk-table) :reader t)))

(defmethod executable-of ((mirror memory-mirror))
  (executable-of (process-of mirror)))

(defgeneric resolve-extent-for-addr (mirror addr)
  (:method ((mirror memory-mirror) addr)
    (with-recursive-lock-held ((lock-of mirror))
      (or (lookup-chunk (extent-map-of mirror) addr)
          (lookup-chunk (section-map-of mirror) addr))))
  (:method ((ext memory-extent) addr)
    (if (< -1 (- addr (start-address-of ext)) (length-of ext))
        ext
        (resolve-extent-for-addr (mirror-of ext) addr))))

;; Mirror synchronization

(defun delete-memory-extent (mirror extent)
  (setf (slot-value extent 'data-bytes) nil)
  (removef (extents-of mirror) extent)
  (trees:delete (start-address-of extent) (extent-map-of mirror)))

(defun ensure-memory-extent (mirror mapping)
  (bind ((start (memory-mapping-start-addr mapping))
         (len (- (memory-mapping-end-addr mapping) start)))
    (or (lookup-chunk (extent-map-of mirror) start)
        (let ((new-ext (make-instance 'memory-extent
                                      :mirror mirror
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
        (when (or (null mapping)
                  (/= (- (memory-mapping-end-addr mapping) start) (length-of ext)))
          (delete-memory-extent mirror ext))))
    (dolist (sect (sections-of (executable-of mirror)))
      (unless (or (writable? (origin-of sect))
                  (null (data-bytes-of sect)))
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

(defun make-memory-mirror (process)
  (let ((mirror (make-instance 'memory-mirror :process process)))
    (refresh-memory-mirror mirror)
    mirror))
