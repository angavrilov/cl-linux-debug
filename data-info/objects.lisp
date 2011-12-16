;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(def (class* e) object-memory-mirror (memory-mirror type-context)
  ((malloc-chunks (make-malloc-chunk-map) :reader t)
   (malloc-chunk-reftbl nil :accessor t)
   (malloc-chunk-types nil :accessor t)
   (find-by-id-cache (make-hash-table :test #'equal) :reader t)
   (vtable-names (make-hash-table :test #'eql) :reader t)
   (globals nil :accessor t)
   (global-map (make-chunk-table) :accessor t)))

(def (class* e) malloc-chunk-range (address-chunk)
  ((chunk-id :reader t)
   (references :reader t)
   (obj-type :reader t)))

(def (class* ea) memory-object-info ()
  ((malloc-chunk-range nil :accessor t)
   (section nil :accessor t)
   (region nil :accessor t)
   (is-code? nil :accessor t)
   (global nil :accessor t)))

(defmethod initialize-instance :after ((mirror object-memory-mirror) &key)
  (awhen (process-of mirror)
    (when (typep (origin-of (main-image-of (executable-of it)))
                 'pe-executable-image)
      (setf (os-type-of mirror) $windows))))

(defun precompute-globals (mirror)
  (loop
     with gmap = (make-chunk-table)
     for global in *known-globals*
     for ref = (get-memory-global mirror (car global))
     when ref
     collect ref into gs
     do (trees:insert ref gmap)
     finally (setf (globals-of mirror) (sort gs #'< :key #'start-address-of)
                   (global-map-of mirror) gmap)))

(defun invalidate-object-mirror-caches (mirror)
  (precompute-globals mirror)
  (setf (malloc-chunk-types-of mirror)
        (collect-known-objects mirror (malloc-chunks-of mirror)))
  (clrhash (vtable-names-of mirror))
  (clrhash (find-by-id-cache-of mirror)))

(defmethod check-refresh-context :after ((mirror object-memory-mirror))
  (invalidate-object-mirror-caches mirror))

(defmethod refresh-memory-mirror :after ((mirror object-memory-mirror) &key)
  (with-simple-restart (continue "Ignore malloc chunks")
    (collect-malloc-objects mirror (malloc-chunks-of mirror))
    (with-simple-restart (continue "Skip cross-referencing chunks")
      (setf (malloc-chunk-reftbl-of mirror)
            (collect-chunk-references mirror (malloc-chunks-of mirror)))))
  (unless (check-refresh-context mirror)
    (invalidate-object-mirror-caches mirror)))

(defmethod $ ((obj object-memory-mirror) key &optional default)
  (if (eq key $enum)
      (call-next-method)
      (or (find key (globals-of obj) :key #'memory-object-ref-parent-key
                :test #'equal) default)))

(defmethod $ ((obj object-memory-mirror) (key (eql '*)) &optional default)
  (declare (ignore default))
  (globals-of obj))

(defmethod get-id-search-cache ((context object-memory-mirror) address type field)
  (let ((key (list address type field)))
    (aif (gethash key (find-by-id-cache-of context))
         (values it t)
         (values (setf (gethash key (find-by-id-cache-of context))
                       (make-hash-table :test #'equal)) nil))))

(defun lookup-malloc-chunk-range (mirror address)
  (multiple-value-bind (malloc-id malloc-min malloc-max malloc-ok?)
      (lookup-malloc-object mirror (malloc-chunks-of mirror) (floor address))
    (when malloc-ok?
      (make-instance 'malloc-chunk-range
                     :chunk-id malloc-id
                     :start-address malloc-min :length (- malloc-max malloc-min)
                     :references
                     (get-references (malloc-chunk-reftbl-of mirror) malloc-id)
                     :obj-type
                     (awhen (malloc-chunk-types-of mirror)
                       (car (aref it malloc-id)))))))

(defgeneric get-address-object-info (mirror address)
  (:method (mirror (address null)) nil)
  (:method (mirror (address memory-object-ref))
    (get-address-object-info mirror (start-address-of address)))
  (:method ((mirror object-memory-mirror) (address number))
    (bind ((section (find-section-by-address (executable-of mirror) address))
           (region (find-region-by-address (executable-of mirror) address))
           (global (lookup-chunk (global-map-of mirror) address)))
      (make-instance 'memory-object-info
                     :malloc-chunk-range
                     (lookup-malloc-chunk-range mirror address)
                     :section section
                     :is-code? (and section (executable? (origin-of section)))
                     :global global
                     :region region))))

(defmethod decode-chunk-reference (memory (context object-memory-mirror) ref target)
  (decode-chunk-reference memory (malloc-chunks-of context) ref target))

(defun get-chunk-range-refs (context info)
  (when info
    (loop for addr in
         (remove-duplicates
          (mapcan (lambda (ref) (decode-chunk-reference context context ref (chunk-id-of info)))
                  (references-of info)))
       collect
         (make-ad-hoc-memory-ref context addr (make-instance 'padding)
                                 :parent :back-refs :key (chunk-id-of info)))))

(defmethod describe-address-in-context append ((it loaded-section) addr)
  (list (format nil "~A~A in ~A"
                (section-name-of it)
                (format-hex-offset (- addr (start-address-of it)) :force-sign? t)
                (pathname-name (path-of (image-of (origin-of it)))))))

(defmethod describe-address-in-context append ((it malloc-chunk-range) addr)
  (list (let ((s (start-address-of it)))
          (format nil "heap ~A~@[~A~] (~A bytes~@[, ~A refs~])~@[: ~A~]"
                  (format-hex-offset s)
                  (when (/= addr s)
                    (format-hex-offset (- addr s)  :force-sign?  t))
                  (format-hex-offset (length-of it))
                  (aif (references-of it) (length it))
                  (aif (obj-type-of it) (public-type-name-of it))))))

(defmethod describe-address-in-context append ((it loaded-region) addr)
  (list (format nil "~A~@[~A~]"
                (or (symbol-name-of it)
                    (format-hex-offset (start-address-of it)))
                (when (/= addr (start-address-of it))
                  (format-hex-offset (- addr (start-address-of it)) :force-sign? t)))))

(defmethod describe-address-in-context append ((it memory-object-ref) addr)
  (list (format nil "~A~@[~A~]"
                (get-$-field-name (memory-object-ref-parent-key it))
                (when (/= addr (start-address-of it))
                  (format-hex-offset (- addr (start-address-of it)) :force-sign? t)))))

(defmethod describe-address-in-context append ((info memory-object-info) addr)
  (mapcan (lambda (x) (describe-address-in-context x addr))
          (list (global-of info)
                (region-of info)
                (malloc-chunk-range-of info)
                (section-of info))))

(defmethod describe-address-in-context append ((mirror object-memory-mirror) addr)
  (describe-address-in-context (get-address-object-info mirror addr) addr))

(defun get-address-info-range (memory addr)
  (bind ((info (get-address-object-info memory addr))
         ((:values r-start r-len)
          (awhen (and info
                      (or (malloc-chunk-range-of info)
                          (region-of info)
                          (global-of info)))
            (values (start-address-of it) (length-of it)))))
    (values info r-start r-len)))

(defgeneric get-address-info-ref (mirror info)
  (:method ((mirror object-memory-mirror) (info null)) nil)
  (:method ((mirror object-memory-mirror) (info malloc-chunk-range))
    (aif (obj-type-of info)
         (make-memory-ref mirror (start-address-of info) it)
         (make-ad-hoc-memory-ref mirror (start-address-of info)
                                 (make-instance 'padding :size (length-of info)))))
  (:method ((mirror object-memory-mirror) (info memory-object-info))
    (or (get-address-info-ref mirror (malloc-chunk-range-of info))
        (global-of info))))

(defmethod get-vtable-class-name ((context object-memory-mirror) address)
  (let* ((vtbl (make-memory-ref context address
                                (lookup-type-in-context context (vtable-type-by-os context))))
         (tptr $vtbl.type_info)
         (tinfo (get-address-info-range context tptr))
         (vinfo (get-address-info-range context $vtbl.methods[0])))
    (when (and tptr tinfo vinfo
               (is-code? vinfo)
               (section-of tinfo)
               (not (is-code? tinfo)))
      (if (eq (os-type-of context) $windows)
          $tptr.pTypeDescriptor.name
          $tptr.class_name))))

(defun detect-garbage (mirror value)
  (awhen (garbage-word-of mirror)
    (loop with mask = 0
       for i from 0 below 32 by 8 and j from 0
       when (= (ldb (byte 8 i) it) (ldb (byte 8 i) value))
       do (setf (ldb (byte 1 j) mask) 1)
       finally (return mask))))

(defun is-possible-garbage? (mirror pad)
  (and pad (aif (garbage-word-of mirror) (= it pad) t)))

(defun make-guessed-pointer (info)
  (make-instance 'pointer
                 :fields (aif (aand (malloc-chunk-range-of info)
                                    (obj-type-of it))
                              (list (make-ad-hoc-type-reference it)))))

(defun match-type-by-info (mirror infolist)
  (destructuring-bind (addrv val info start length) (first infolist)
    (declare (ignore addrv))
    (bind (((&optional next nnext item3 item4 item5 item6 &rest tail) (rest infolist))
           (section (section-of info))
           (os (os-type-of mirror)))
      (declare (ignore item3 tail))
      (cond
        ;; string
        ((if (eq os $linux)
             (eql (- val #xC) start)
             (and (is-possible-garbage? mirror (second item6))
                  (bind ((len (second item4))
                         (cap (second item5)))
                    (and
                     (<= 0 len cap) (<= 15 cap)
                     (if (= cap 15)
                         (multiple-value-bind (idx off) (floor len 4)
                           (= (ldb (byte 8 (* 8 off)) (second (nth idx infolist))) 0))
                         (and (eql val start) (<= cap length)
                              (eql (get-memory-integer mirror (+ start len) 1) 0)))))))
         (make-instance 'stl-string))
        ;; garbage
        ((not (eql val start))
         (cond ((or (logtest val 3)
                    (not (or start section)))
                (bind ((cnt 1)
                       (ahead val)
                       (gmask (detect-garbage mirror val))
                       (item
                        (case gmask
                          (#b1111 (make-instance 'padding :size 4))
                          (#b1110 (list (make-instance 'int8_t) (make-instance 'padding :size 3)))
                          (#b1100 (list (make-instance 'int16_t) (make-instance 'padding :size 2)))
                          (otherwise (make-instance 'int32_t)))))
                  (dolist (v (rest infolist))
                    (if (= (second v) val)
                        (incf cnt)
                        (progn
                          (setf ahead (second v))
                          (return))))
                  ;; Detect empty Windows vectors
                  (when (and (= val 0) (eq os $windows)
                             (>= cnt 3)
                             (eql ahead (garbage-word-of mirror)))
                    (incf cnt -3)
                    (when (= cnt 0)
                      (return-from match-type-by-info
                        (make-instance 'stl-vector))))
                  ;; Make arrays
                  (cond ((typep item 'padding)
                         (setf (size-of item) (* (size-of item) cnt))
                         item)
                        ((> cnt 12)
                         (make-instance 'static-array :count cnt :fields (ensure-list item)
                                        :comment (format nil "Value is 0x~X" val)))
                        (t
                         item))))
               ((or start (executable? (origin-of section)))
                (make-instance 'pointer))
               (t
                (make-instance 'pointer))))
        ;; stl vector
        ((and (eql start (fourth next))
              (eql start (fourth nnext))
              (<= val (second next) (second nnext)))
         (bind ((ptr (get-memory-integer mirror val 4))
                ((:values pv-info ds de)
                 (get-address-info-range mirror ptr))
                (size (- (second next) val))
                (elt-type (cond ((and ds de (= ds ptr))
                                 (make-guessed-pointer pv-info))
                                ((logtest size 1)
                                 (make-instance 'int8_t))
                                ((logtest size 2)
                                 (make-instance 'int16_t))
                                (t
                                 (make-instance 'int32_t)))))
           (make-instance 'stl-vector :fields (list elt-type))))
        ;; random pointer
        (t
         (make-guessed-pointer info))))))

(defun guess-types-by-data (mirror ref)
  (with-bytes-for-ref (vector offset ref (length-of ref))
    (bind ((groups)
           (cnt -1)
           (addr (start-address-of ref))
           (cap (+ addr (length-of ref)))
           ((:values root-info root-start root-len)
            (get-address-info-range mirror ref)))
      (declare (ignore root-info root-len))
      (labels ((consume (type)
                 (let ((new (make-ad-hoc-memory-ref
                             mirror addr type :no-copy? t
                             :parent ref :key (incf cnt) :local? t)))
                   (push new groups)
                   (incf addr (length-of new))
                   (incf offset (length-of new))))
               (align ()
                 (when (and (logtest addr 1) (< addr cap))
                   (consume (make-instance 'int8_t)))
                 (when (and (logtest addr 2) (< addr cap))
                   (consume (make-instance 'int16_t)))))
        (align)
        (let ((infolist (loop for addr from addr below (- cap 3) by 4
                           for off from offset by 4
                           for val = (parse-int vector off 4)
                           collect (list* addr val
                                          (multiple-value-list
                                           (get-address-info-range mirror val))))))
          (awhen (and (eql addr root-start)
                      infolist
                      (get-vtable-class-name mirror (second (first infolist))))
            (consume (aif (resolve-class-in-context mirror (second (first infolist)))
                          (make-instance 'global-type-proxy :effective-main-type it)
                          (make-instance 'pointer :type-name (vtable-type-by-os mirror))))
            (align))
          (do ((tail infolist (rest tail)))
              ((null tail))
            (when (>= (caar tail) addr)
              (assert (= (caar tail) addr))
              (dolist (x (ensure-list (match-type-by-info mirror tail)))
                (consume x))
              (align))))
        (when (< addr cap)
          (when (logtest (- cap addr) 2)
            (consume (make-instance 'int16_t)))
          (when (logtest (- cap addr) 1)
            (consume (make-instance 'int8_t)))))
      (nreverse groups))))
