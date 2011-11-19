;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(def (class* e) object-memory-mirror (memory-mirror type-context)
  ((malloc-chunks (make-binsearch-uint32-vec) :reader t)))

(def (class* ea) memory-object-info ()
  ((malloc-chunk-range nil :accessor t)
   (section nil :accessor t)
   (region nil :accessor t)))

(defmethod refresh-memory-mirror :after ((mirror object-memory-mirror))
  (check-refresh-context mirror)
  (with-simple-restart (continue "Ignore malloc chunks")
    (collect-malloc-objects mirror (malloc-chunks-of mirror))))

(defgeneric get-address-object-info (mirror address)
  (:method ((mirror object-memory-mirror) address)
    (bind (((:values malloc-min malloc-max malloc-ok?)
            (lookup-malloc-object (malloc-chunks-of mirror) address))
           (section (find-section-by-address (executable-of mirror) address))
           (region (find-region-by-address (executable-of mirror) address)))
      (make-instance 'memory-object-info
                     :malloc-chunk-range (if malloc-ok? (cons malloc-min malloc-max))
                     :section section
                     :region region))))

(defun get-address-info-region (memory addr)
  (bind ((info (get-address-object-info memory addr))
         ((:values r-start r-len)
          (acond ((malloc-chunk-range-of info)
                  (values (car it) (- (cdr it) (car it))))
                 ((region-of info)
                  (values (start-address-of it) (length-of it))))))
    (values info r-start r-len)))

(defun guess-types-by-data (mirror ref)
  (with-bytes-for-ref (vector offset ref (length-of ref))
    (let* ((groups)
           (cnt -1)
           (addr (start-address-of ref))
           (cap (+ addr (length-of ref))))
      (labels ((consume (type)
                 (let ((new (make-ad-hoc-memory-ref
                             mirror addr type :no-copy? t
                             :parent ref :key (incf cnt) :local? t)))
                   (push new groups)
                   (incf addr (length-of new))
                   (incf offset (length-of new)))))
        (when (logtest addr 1)
          (consume (make-instance 'int8_t)))
        (when (logtest addr 2)
          (consume (make-instance 'int16_t)))
        (let ((infolist (loop for addr from addr below (- cap 3) by 4
                           for off from offset by 4
                           for val = (parse-int vector off 4)
                           collect (list* addr val
                                          (multiple-value-list
                                           (get-address-info-region mirror val))))))
          (do ((tail infolist (rest tail)))
              ((null tail))
            (destructuring-bind (addrv val info start end) (first tail)
              (declare (ignore info end))
              (let* ((next (second tail))
                     (nnext (third tail)))
                (assert (= addr addrv))
                (cond
                  ;; string
                  ((eql (- val #xC) start)
                   (consume (make-instance 'stl-string)))
                  ;; garbage
                  ((not (eql val start))
                   (consume (make-instance 'int32_t)))
                  ;; stl vector
                  ((and (eql start (fourth next))
                        (eql start (fourth nnext))
                        (<= val (second next) (second nnext)))
                   (consume (make-instance 'stl-vector :fields (list (make-instance 'pointer))))
                   (setf tail (cddr tail)))
                  ;; random pointer
                  (t
                   (consume (make-instance 'pointer))))))))
        (when (logtest (- cap addr) 2)
          (consume (make-instance 'int16_t)))
        (when (logtest (- cap addr) 1)
          (consume (make-instance 'int8_t))))
      (nreverse groups))))
