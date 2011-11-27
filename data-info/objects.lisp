;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(def (class* e) object-memory-mirror (memory-mirror type-context)
  ((malloc-chunks (make-malloc-chunk-map) :reader t)
   (malloc-chunk-reftbl nil :accessor t)
   (find-by-id-cache (make-hash-table :test #'equal) :reader t)
   (vtable-names (make-hash-table :test #'eql) :reader t)))

(def (class* e) malloc-chunk-range (address-chunk)
  ((chunk-id :reader t)
   (references :reader t)))

(def (class* ea) memory-object-info ()
  ((malloc-chunk-range nil :accessor t)
   (section nil :accessor t)
   (region nil :accessor t)
   (is-code? nil :accessor t)))

(defmethod refresh-memory-mirror :after ((mirror object-memory-mirror))
  (check-refresh-context mirror)
  (clrhash (find-by-id-cache-of mirror))
  (with-simple-restart (continue "Ignore malloc chunks")
    (collect-malloc-objects mirror (malloc-chunks-of mirror))
    (with-simple-restart (continue "Skip cross-referencing chunks")
      (setf (malloc-chunk-reftbl-of mirror)
            (collect-chunk-references mirror (malloc-chunks-of mirror))))))

(defmethod get-id-search-cache ((context object-memory-mirror) address type field)
  (let ((key (list address type field)))
    (aif (gethash key (find-by-id-cache-of context))
         (values it t)
         (values (setf (gethash key (find-by-id-cache-of context))
                       (make-hash-table :test #'equal)) nil))))

(defgeneric get-address-object-info (mirror address)
  (:method (mirror (address null)) nil)
  (:method (mirror (address memory-object-ref))
    (get-address-object-info mirror (start-address-of address)))
  (:method ((mirror object-memory-mirror) (address integer))
    (bind (((:values malloc-id malloc-min malloc-max malloc-ok?)
            (lookup-malloc-object (malloc-chunks-of mirror) address))
           (section (find-section-by-address (executable-of mirror) address))
           (region (find-region-by-address (executable-of mirror) address)))
      (make-instance 'memory-object-info
                     :malloc-chunk-range
                     (when malloc-ok?
                       (make-instance 'malloc-chunk-range
                                      :chunk-id malloc-id
                                      :start-address malloc-min :length (- malloc-max malloc-min)
                                      :references
                                      (get-references (malloc-chunk-reftbl-of mirror) malloc-id)))
                     :section section
                     :is-code? (and section (executable? (origin-of section)))
                     :region region))))

(defmethod describe-address-in-context append ((it loaded-section) addr)
  (list (format nil "~A~A in ~A"
                (section-name-of it)
                (format-hex-offset (- addr (start-address-of it)) :force-sign? t)
                (pathname-name (path-of (image-of (origin-of it)))))))

(defmethod describe-address-in-context append ((it malloc-chunk-range) addr)
  (list (let ((s (start-address-of it)))
          (format nil "heap ~A~@[~A~] (~A bytes~@[, ~A refs~])"
                  (format-hex-offset s)
                  (when (/= addr s)
                    (format-hex-offset (- addr s)  :force-sign?  t))
                  (format-hex-offset (length-of it))
                  (aif (references-of it) (length it))))))

(defmethod describe-address-in-context append ((it loaded-region) addr)
  (list (format nil "~A~@[~A~]"
                (or (symbol-name-of it)
                    (format-hex-offset (start-address-of it)))
                (when (/= addr (start-address-of it))
                  (format-hex-offset (- addr (start-address-of it)) :force-sign? t)))))

(defmethod describe-address-in-context append ((info memory-object-info) addr)
  (mapcan (lambda (x) (describe-address-in-context x addr))
          (list (region-of info)
                (malloc-chunk-range-of info)
                (section-of info))))

(defmethod describe-address-in-context append ((mirror object-memory-mirror) addr)
  (describe-address-in-context (get-address-object-info mirror addr) addr))

(defun get-address-info-range (memory addr)
  (bind ((info (get-address-object-info memory addr))
         ((:values r-start r-len)
          (awhen (and info
                      (or (malloc-chunk-range-of info)
                          (region-of info)))
            (values (start-address-of it) (length-of it)))))
    (values info r-start r-len)))

(defmethod get-vtable-class-name ((context object-memory-mirror) address)
  (let* ((vtbl (make-memory-ref context address $glibc:vtable))
         (tptr $vtbl.type_info)
         (tinfo (get-address-info-range context tptr))
         (vinfo (get-address-info-range context $vtbl.methods[0])))
    (when (and tptr tinfo vinfo
               (is-code? vinfo)
               (section-of tinfo)
               (not (is-code? tinfo)))
      $tptr.class_name)))

(defun match-type-by-info (mirror infolist)
  (destructuring-bind (addrv val info start end) (first infolist)
    (declare (ignore addrv end))
    (let* ((next (second infolist))
           (nnext (third infolist))
           (section (section-of info)))
      (cond
        ;; string
        ((eql (- val #xC) start)
         (make-instance 'stl-string))
        ;; garbage
        ((not (eql val start))
         (cond ((or (logtest val 3)
                    (not (or start section)))
                (let ((cnt 1))
                  (loop for v in (rest infolist)
                     while (= (second v) val)
                     do (incf cnt))
                  (if (> cnt 12)
                      (make-instance 'static-array :count cnt :type-name $int32_t
                                     :comment (format nil "Value is ~A" (signed val 32)))
                      (make-instance 'int32_t))))
               ((or start (executable? (origin-of section)))
                (make-instance 'pointer))
               (t
                (make-instance 'pointer))))
        ;; stl vector
        ((and (eql start (fourth next))
              (eql start (fourth nnext))
              (<= val (second next) (second nnext)))
         (bind ((ptr (get-memory-integer mirror val 4))
                ((:values info ds de)
                 (get-address-info-range mirror ptr))
                (size (- (second next) val))
                (elt-type (cond ((and ds de (= ds ptr))
                                  (make-instance 'pointer))
                                ((logtest size 1)
                                 (make-instance 'int8_t))
                                ((logtest size 2)
                                 (make-instance 'int16_t))
                                (t
                                 (make-instance 'int32_t)))))
           (declare (ignore info))
           (make-instance 'stl-vector :fields (list elt-type))))
        ;; random pointer
        (t
         (make-instance 'pointer))))))

(defun guess-types-by-data (mirror ref)
  (with-bytes-for-ref (vector offset ref (length-of ref))
    (bind ((groups)
           (cnt -1)
           (addr (start-address-of ref))
           (cap (+ addr (length-of ref)))
           ((:values root-info root-start root-let)
            (get-address-info-range mirror ref)))
      (declare (ignore root-info))
      (labels ((consume (type)
                 (let ((new (make-ad-hoc-memory-ref
                             mirror addr type :no-copy? t
                             :parent ref :key (incf cnt) :local? t)))
                   (push new groups)
                   (incf addr (length-of new))
                   (incf offset (length-of new))))
               (align ()
                 (when (logtest addr 1)
                   (consume (make-instance 'int8_t)))
                 (when (logtest addr 2)
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
                          (make-instance 'pointer :type-name $glibc:vtable)))
            (align))
          (do ((tail infolist (rest tail)))
              ((null tail))
            (when (>= (caar tail) addr)
              (assert (= (caar tail) addr))
              (consume (match-type-by-info mirror tail))
              (align))))
        (when (logtest (- cap addr) 2)
          (consume (make-instance 'int16_t)))
        (when (logtest (- cap addr) 1)
          (consume (make-instance 'int8_t))))
      (nreverse groups))))
