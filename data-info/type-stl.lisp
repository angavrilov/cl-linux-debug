;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

;; Strings

(defmethod xml:xml-tag-name-symbol ((str stl-string)) 'stl-string)

(def (class* eas) stl-string/linux (stl-string ptr-string)
  ())

(def (class* eas) stl-string/windows (stl-string)
  ())

(defmethod substitute-type-class ((context os-context/windows) (str stl-string))
  (change-class str 'stl-string/windows))

(defmethod substitute-type-class ((context os-context/linux) (str stl-string))
  (change-class str 'stl-string/linux))

(defmethod compute-effective-fields (context (type stl-string/windows))
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

(defmethod compute-effective-fields ((context os-context/linux) (type stl-vector))
  (list
   (make-instance 'pointer :name $start)
   (make-instance 'pointer :name $end)
   (make-instance 'pointer :name $block-end)))

(defmethod compute-effective-fields ((context os-context/windows) (type stl-vector))
  (list
   (make-instance 'pointer :name $start)
   (make-instance 'pointer :name $end)
   (make-instance 'pointer :name $block-end)
   (make-instance 'padding :name $pad :size 4 :alignment 4)))

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
  (with-walker-utils (u ctx offset)
    `(let* ((start ,(u/field-int node $start 4))
            (end ,(u/field-int node $end 4))
            (diff (uint32 (- end start))))
       (when (and (<= start end)
                  (not (logtest diff 3))
                  (< diff most-positive-fixnum))
         (setf ,ptr-var start ,cnt-var (ash diff -2))))))

;; STL bit vector

(def (class* eas) stl-bit-vector/linux (stl-bit-vector)
  ())

(def (class* eas) stl-bit-vector/windows (stl-bit-vector)
  ())

(defmethod substitute-type-class ((context os-context/windows) (str stl-bit-vector))
  (change-class str 'stl-bit-vector/windows))

(defmethod substitute-type-class ((context os-context/linux) (str stl-bit-vector))
  (change-class str 'stl-bit-vector/linux))

(defmethod compute-effective-fields (context (type stl-bit-vector/linux))
  (flatten (list
            (make-instance 'pointer :name $start)
            (make-instance 'int32_t :name $start-bit)
            (make-instance 'pointer :name $end)
            (make-instance 'int32_t :name $end-bit)
            (make-instance 'pointer :name $block-end))))

(defmethod array-base-dimensions ((type stl-bit-vector/linux) ref)
  (stl-vector-dimensions ref 1/8 :size-bias $ref.end-bit))

(defmethod compute-effective-fields (context (type stl-bit-vector/windows))
  (flatten (list
            (make-instance 'pointer :name $start)
            (make-instance 'pointer :name $end)
            (make-instance 'pointer :name $block-end)
            (make-instance 'padding :name $pad :size 4)
            (make-instance 'int32_t :name $size))))

(defmethod array-base-dimensions ((type stl-bit-vector/windows) ref)
  (stl-vector-dimensions ref 1/8 :size-override $ref.size))

;; STL deque

(def (class* eas) stl-deque/linux (stl-deque)
  ())

(def (class* eas) stl-deque/windows (stl-deque)
  ())

(defmethod substitute-type-class ((context os-context/linux) (str stl-deque))
  (change-class str 'stl-deque/linux))

(defmethod substitute-type-class ((context os-context/windows) (str stl-deque))
  (change-class str 'stl-deque/windows))

(defmethod compute-effective-fields (context (type stl-deque/linux))
  (flatten (list
            (make-instance 'pointer :name $map)
            (make-instance 'int32_t :name $map-size)
            (make-instance 'pointer :name $start-cur)
            (make-instance 'pointer :name $start-first)
            (make-instance 'pointer :name $start-last)
            (make-instance 'pointer :name $start-map)
            (make-instance 'pointer :name $end-cur)
            (make-instance 'pointer :name $end-first)
            (make-instance 'pointer :name $end-last)
            (make-instance 'pointer :name $end-map))))

(defun deque-block-size/linux (elt-size)
  (if (< elt-size 512)
      (floor 512 elt-size)
      1))

(defmethod sequence-content-items ((type stl-deque/linux) ref)
  (let* ((map $ref.map)
         (map-size $ref.map-size)
         (smap $ref.start-map)
         (scur $ref.start-cur)
         (emap $ref.end-map)
         (ecur $ref.end-cur))
    (when (and map map-size smap scur emap ecur (> map-size 0))
      (with-bytes-for-ref (vector offset map (* 4 map-size))
        (flet ((ptr-index (base target &optional (size 4))
                 (aprog1 (/ (- (start-address-of target) base) size)
                   (unless (and (typep it 'fixnum) (<= 0 it))
                     (return-from sequence-content-items nil)))))
          (let* ((mbase (start-address-of map))
                 (sidx (ptr-index mbase smap))
                 (eidx (ptr-index mbase emap))
                 (elt-size (effective-element-size-of type))
                 (block-size (deque-block-size/linux elt-size)))
            (when (<= 0 sidx eidx (1- map-size))
              (loop for i from sidx to eidx
                 for ptr = (parse-int vector (+ offset (* i 4)) 4)
                 for start = (if (= i sidx) (ptr-index ptr scur elt-size) 0)
                 and end = (if (= i eidx) (ptr-index ptr ecur elt-size) block-size)
                 collect (cons (+ ptr (* start elt-size)) (- end start))
                 into chunks
                 finally (return (wrap-chunked-array-item-seq type ref chunks))))))))))

(defmethod compute-effective-fields (context (type stl-deque/windows))
  (flatten (list
            (make-instance 'pointer :name $proxy)
            (make-instance 'pointer :name $map)
            (make-instance 'int32_t :name $map-size)
            (make-instance 'int32_t :name $off)
            (make-instance 'int32_t :name $size)
            (make-instance 'padding :size 4 :alignment 4))))

(defun deque-block-size/windows (elt-size)
  (cond ((<= elt-size 1) 16)
        ((<= elt-size 2) 8)
        ((<= elt-size 4) 4)
        ((<= elt-size 8) 2)
        (t 1)))

(defmethod sequence-content-items ((type stl-deque/windows) ref)
  (let* ((map $ref.map)
         (map-size $ref.map-size)
         (off $ref.off)
         (size $ref.size))
    (when (and map map-size off size
               (> map-size 0) (>= size 0) (>= off 0))
      (with-bytes-for-ref (vector offset map (* 4 map-size))
        (let* ((elt-size (effective-element-size-of type))
               (block-size (deque-block-size/windows elt-size))
               (sidx (floor off block-size))
               (eidx (floor (+ off size) block-size)))
          (loop for i from sidx to eidx
             for ptr = (parse-int vector (+ offset (* (mod i map-size) 4)) 4)
             for start = (if (= i sidx) (mod off block-size) 0)
             and end = (if (= i eidx) (mod (+ off size) block-size) block-size)
             collect (cons (+ ptr (* start elt-size)) (- end start))
             into chunks
             finally (return (wrap-chunked-array-item-seq type ref chunks))))))))
