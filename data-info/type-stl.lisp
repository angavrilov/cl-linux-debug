;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

;; Strings

(defmethod xml:xml-tag-name-symbol ((str stl-string)) 'stl-string)

(def (class* eas) stl-string/linux (stl-string ptr-string)
  ())

(def (class* eas) stl-string/windows (stl-string)
  ())

(defmethod layout-type-rec :before ((str stl-string))
  (case (os-type-of *type-context*)
    ($windows (change-class str 'stl-string/windows))
    (otherwise (change-class str 'stl-string/linux))))

(defmethod compute-effective-fields ((type stl-string/windows))
  (list (make-instance 'compound :is-union t
                       :fields (list
                                (make-instance 'static-string :name $buffer :size 16)
                                (make-instance 'pointer :name $ptr :type-name $static-string)))
        (make-instance 'int32_t :name $length)
        (make-instance 'int32_t :name $capacity)
        (make-instance 'padding :name $pad :size 4)))

(defmethod %memory-ref-$ ((type stl-string/windows) ref (key (eql t)))
  (if (< $ref.capacity 16) $ref.buffer $ref.ptr.value))

;; STL vector

(defmethod compute-effective-fields ((type stl-vector))
  (flatten (list
            (make-instance 'pointer :name $start)
            (make-instance 'pointer :name $end)
            (make-instance 'pointer :name $block-end)
            (when (eq (os-type-of *type-context*) $windows)
              (make-instance 'padding :name $pad :size 4 :alignment 4)))))

(defun stl-vector-dimensions (ref elt-size &key (size-bias 0) size-override)
  (let* ((s $ref.start)
         (e $ref.end)
         (a $ref.block-end)
         (saddr (ensure-ref-address s)))
    (awhen (or (and s e a
                    (not (logtest saddr 3))
                    (<= 0 saddr
                        (memory-object-ref-address e)
                        (memory-object-ref-address a)))
               (not (or s e a)))
      (if s
          (let ((size (/ (address- e s) elt-size)))
            (when (or (null size-override) (<= size-override size))
              (values saddr (+ (or size-override size) size-bias))))
          (values nil 0)))))

(defmethod array-base-dimensions ((type stl-vector) ref)
  (stl-vector-dimensions ref (effective-element-size-of type)))

(defmethod build-set-array-base-dimensions (context (node stl-vector) offset ctx ptr-var cnt-var)
  `(let* ((start ,(access-walker-int ctx offset 4))
          (end ,(access-walker-int ctx (+ offset 4) 4))
          (diff (logand (- end start) #xFFFFFFFF)))
     (declare (type uint32 start end diff))
     (when (and (<= start end)
                (not (logtest diff 3))
                (< diff most-positive-fixnum))
       (setf ,ptr-var start ,cnt-var (ash diff -2)))))

;; STL bit vector

(def (class* eas) stl-bit-vector/linux (stl-bit-vector)
  ())

(def (class* eas) stl-bit-vector/windows (stl-bit-vector)
  ())

(defmethod layout-type-rec :before ((str stl-bit-vector))
  (case (os-type-of *type-context*)
    ($windows (change-class str 'stl-bit-vector/windows))
    (otherwise (change-class str 'stl-bit-vector/linux))))

(defmethod compute-effective-fields ((type stl-bit-vector/linux))
  (flatten (list
            (make-instance 'pointer :name $start)
            (make-instance 'int32_t :name $start-bit)
            (make-instance 'pointer :name $end)
            (make-instance 'int32_t :name $end-bit)
            (make-instance 'pointer :name $block-end))))

(defmethod array-base-dimensions ((type stl-bit-vector/linux) ref)
  (stl-vector-dimensions ref 1/8 :size-bias $ref.end-bit))

(defmethod compute-effective-fields ((type stl-bit-vector/windows))
  (flatten (list
            (make-instance 'pointer :name $start)
            (make-instance 'pointer :name $end)
            (make-instance 'pointer :name $block-end)
            (make-instance 'padding :name $pad :size 4)
            (make-instance 'int32_t :name $size))))

(defmethod array-base-dimensions ((type stl-bit-vector/windows) ref)
  (stl-vector-dimensions ref 1/8 :size-override $ref.size))


