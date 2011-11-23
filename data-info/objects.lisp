;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(def (class* e) object-memory-mirror (memory-mirror type-context)
  ((malloc-chunks (make-malloc-chunk-map) :reader t)
   (vtable-names (make-hash-table :test #'eql) :reader t)))

(def (class* e) malloc-chunk-range (address-chunk)
  ())

(def (class* ea) memory-object-info ()
  ((malloc-chunk-range nil :accessor t)
   (section nil :accessor t)
   (region nil :accessor t)
   (is-code? nil :accessor t)))

(defmethod refresh-memory-mirror :after ((mirror object-memory-mirror))
  (check-refresh-context mirror)
  (with-simple-restart (continue "Ignore malloc chunks")
    (collect-malloc-objects mirror (malloc-chunks-of mirror))))

(defgeneric get-address-object-info (mirror address)
  (:method (mirror (address null)) nil)
  (:method (mirror (address memory-object-ref))
    (get-address-object-info mirror (start-address-of address)))
  (:method ((mirror object-memory-mirror) (address integer))
    (bind (((:values malloc-min malloc-max malloc-ok?)
            (lookup-malloc-object (malloc-chunks-of mirror) address))
           (section (find-section-by-address (executable-of mirror) address))
           (region (find-region-by-address (executable-of mirror) address)))
      (make-instance 'memory-object-info
                     :malloc-chunk-range
                     (if malloc-ok?
                         (make-instance 'malloc-chunk-range
                                        :start-address malloc-min :length (- malloc-max malloc-min)))
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
          (format nil "heap ~A~@[~A~] (~A bytes)"
                  (format-hex-offset s)
                  (when (/= addr s)
                    (format-hex-offset (- addr s)  :force-sign?  t))
                  (format-hex-offset (length-of it))))))

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
         (tinfo (get-address-info-range context tptr)))
    (when (and tptr
               (is-code? (get-address-info-range context $vtbl.methods[0]))
               (section-of tinfo)
               (not (is-code? tinfo)))
      $tptr.class_name)))

(defun match-type-by-info (infolist)
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
                (make-instance 'int32_t))
               ((or start (executable? (origin-of section)))
                (make-instance 'pointer))
               (t
                (make-instance 'pointer))))
        ;; stl vector
        ((and (eql start (fourth next))
              (eql start (fourth nnext))
              (<= val (second next) (second nnext)))
         (make-instance 'stl-vector :fields (list (make-instance 'pointer))))
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
              (consume (match-type-by-info tail))
              (align))))
        (when (logtest (- cap addr) 2)
          (consume (make-instance 'int16_t)))
        (when (logtest (- cap addr) 1)
          (consume (make-instance 'int8_t))))
      (nreverse groups))))
