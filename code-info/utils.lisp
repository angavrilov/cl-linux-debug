;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

;; Basic data types

(deftype uint8 () '(unsigned-byte 8))
(deftype uint16 () '(unsigned-byte 16))
(deftype uint32 () '(unsigned-byte 32))
(deftype uint64 () '(unsigned-byte 64))

(deftype machine-uword () #+x86-64 'uint64 #-x86-64 'uint32)

(deftype int8 () '(signed-byte 8))
(deftype int16 () '(signed-byte 16))
(deftype int32 () '(signed-byte 32))
(deftype int64 () '(signed-byte 64))

(deftype machine-word () #+x86-64 'int64 #-x86-64 'int32)

(deftype uint8-array () '(simple-array uint8 (*)))

(deftype index-fixnum (&optional (div 1) (bias 0))
  `(integer 0 ,(- (floor most-positive-fixnum div) bias)))

(defconstant +uint32-mask+ (1- (ash 1 32)))

(defmacro uint32 (arg)
  `(the uint32 (logand ,arg +uint32-mask+)))

(def (structure ea) memory-mapping
  (start-addr 0)
  (end-addr 0)
  (readable? nil)
  (writable? nil)
  (executable? nil)
  (shared? nil)
  (file-offset 0)
  (file-path nil))

;; Offsets

(deftype offset () '(or signed-byte ratio null))
(deftype address () '(or unsigned-byte ratio null))

(defun format-hex-offset (offset &key force-sign? (prefix "0x"))
  (multiple-value-bind (int rest) (floor offset 1)
    (string-downcase
     (format nil "~@[~A~]~A~X~@[.~A~]"
             (cond ((< int 0) "-")
                   (force-sign? "+"))
             prefix
             (abs int)
             (ecase rest
               (0 nil)
               ((1/8 2/8 3/8 4/8 5/8 6/8 7/8) (* 8 rest)))))))

(defun parse-hex-offset (offset)
  (or (cl-ppcre:register-groups-bind (sign? body tail)
          ("([-+])?0x([0-9a-fA-F]+)(?:.([0-7]))?" offset)
        (check-type body string)
        (let* ((iv (parse-integer body :radix 16))
               (siv (if (equal sign? "-") (- iv) iv)))
          (if tail
              (+ siv (/ (parse-integer tail) 8))
              siv)))
      (error "Invalid syntax for offset: '~A'" offset)))

;; Signed/unsigned conversion

(declaim (inline signed unsigned))

(defun unsigned (value &optional (bits 32))
  (logand value (1- (ash 1 bits))))

(defun signed (value &optional (bits 32))
  (if (logbitp (1- bits) value)
      (dpb value (byte bits 0) -1)
      value))

;; Abstract chunk table

(defgeneric start-address-of (object))
(defgeneric length-of (object))

(defun make-chunk-table ()
  (make-binary-tree :red-black #'<
                    :key #'start-address-of
                    :test #'=))

(defun lookup-chunk (table address)
  (awhen (lower-bound address table)
    (let ((offset (- address (start-address-of it))))
      (values (if (or (null (length-of it))
                      (< offset (length-of it)))
                  it nil)
              offset))))

(defun lookup-next-chunk (table address)
  (upper-bound (1+ address) table))

;; Binary data parsing

(defun make-byte-vector (size)
  (make-array size :element-type 'uint8))

(defun parse-int (vector start size-bytes &key signed? &aux (result 0))
  (declare (type (vector uint8) vector))
  (loop for i from 0 below size-bytes
     and j from start
     do (setf (ldb (byte 8 (* 8 i)) result)
              (aref vector j)))
  (values (if signed?
              (signed result (* size-bytes 8))
              result)
          size-bytes))

(defun (setf parse-int) (value vector start size-bytes)
  (loop for i from 0 below size-bytes
     and j from start
     do (setf (aref vector j)
              (ldb (byte 8 (* 8 i)) value)))
  value)

(define-compiler-macro parse-int (&whole whole vector start size-bytes &key signed?)
  (cond ((not (and (typep size-bytes '(integer 1 8))
                   (member signed? '(nil t))))
         whole)
        ((= size-bytes 1)
         `(values ,(if signed?
                       `(the (signed-byte 8) (signed (aref ,vector ,start) 8))
                       `(the (unsigned-byte 8) (aref ,vector ,start)))
                  1))
        (t
         (let ((size-bits (* size-bytes 8)))
           (with-unique-names (result vec base)
             `(let ((,result 0)
                    (,vec ,vector)
                    (,base ,start))
                (declare (type (unsigned-byte ,size-bits) ,result)
                         (type (vector uint8) ,vec)
                         (type fixnum ,base))
                (setf ,result
                      (logior ,@(loop for i from 0 below size-bytes
                                   collect `(the (unsigned-byte ,size-bits)
                                              (ash (aref ,vec (+ ,base ,i)) ,(* 8 i))))))
                (values ,(if signed?
                             `(the (signed-byte ,size-bits)
                                (signed ,result ,size-bits))
                             result)
                        ,size-bytes)))))))

(defmacro parsef (cmd vector pos &rest args)
  (with-unique-names (rv size)
    `(multiple-value-bind (,rv ,size) (,cmd ,vector ,pos ,@args)
       (incf ,pos ,size)
       ,rv)))

(defmacro parse-intf (vector pos size &rest flags)
  `(parsef parse-int ,vector ,pos ,size ,@flags))

(defun parse-string (vector start &key (limit (length vector)))
  (declare (type (vector uint8) vector))
  (let* ((length (loop for i from start
                    when (or (>= i limit)
                             (= (aref vector i) 0))
                    return (- i start)))
         (str (make-string length)))
    (loop for i from start and j from 0 below length
       do (setf (aref str j) (code-char (aref vector i))))
    (values str (1+ length))))

(defmacro parse-stringf (vector pos)
  `(parsef parse-string ,vector ,pos))

(defun parse-leb128 (vector start &key signed?)
  (declare (type (vector uint8) vector))
  (let ((value 0))
    (declare (type unsigned-byte value))
    (loop for i from start and j from 0
       for byte = (aref vector i)
       do (progn
            (setf (ldb (byte 7 (* 7 j)) value) (logand byte #x7F))
            (unless (logtest byte #x80)
              (return
                (values (if signed?
                            (signed value (* 7 (1+ j)))
                            value)
                        (1+ j))))))))

(defmacro parse-leb128f (vector pos &rest flags)
  `(parsef parse-leb128 ,vector ,pos ,@flags))

(defun parse-bytes (vector start size)
  (declare (type (vector uint8) vector))
  (let ((buf (make-array size :element-type 'uint8)))
    (loop for i from 0 below size and j from start
       do (setf (aref buf i) (aref vector j)))
    (values buf size)))

(defmacro parse-bytesf (vector pos size)
  `(parsef parse-bytes ,vector ,pos ,size))

;; Binary search

(defun make-binsearch-uint32-vec (&optional (size 0) &rest flags)
  (apply #'make-array size :element-type 'uint32 :fill-pointer 0 :adjustable t flags))

(defun get-vector-simple-array (vector)
  (sb-kernel:with-array-data ((dv vector) (sv) (ev))
    (assert (= sv 0))
    (values dv ev)))

(defmacro with-vector-array ((sv-var vector elt-type &key (size (gensym))) &body code)
  `(multiple-value-bind (,sv-var ,size)
       (get-vector-simple-array ,vector)
     (declare (type (simple-array ,elt-type (*)) ,sv-var)
              (type fixnum ,size)
              (ignorable ,size))
     ,@code))

(defmacro with-simple-vector-fill ((sv-var fill-vector elt-type) &body code)
  (let ((size-var (symbolicate sv-var '#:/size))
        (push-extend (symbolicate sv-var '#:/push-extend)))
    (with-unique-names (fill-ptr fill-vec real-push)
      `(let ((,fill-vec ,fill-vector))
         (declare (type (vector ,elt-type) ,fill-vec))
         (multiple-value-bind (,sv-var ,size-var)
             (get-vector-simple-array ,fill-vec)
           (declare (type (simple-array ,elt-type (*)) ,sv-var)
                    (type fixnum ,size-var))
           (let ((,fill-ptr (fill-pointer ,fill-vec)))
             (declare (type fixnum ,fill-ptr))
             (unwind-protect
                  (flet ((,real-push (value)
                           (declare (optimize (speed 1) (safety 3)))
                           (setf (fill-pointer ,fill-vec) ,fill-ptr)
                           (vector-push-extend value ,fill-vec)
                           (multiple-value-setq (,sv-var ,size-var)
                             (get-vector-simple-array ,fill-vec))
                           (setf ,fill-ptr (fill-pointer ,fill-vec))))
                    (declare (dynamic-extent #',real-push))
                    (flet ((,push-extend (value)
                             (declare (type ,elt-type value)
                                      (optimize (speed 3) (safety 0)))
                             (if (< ,fill-ptr ,size-var)
                                 (progn
                                   (setf (aref ,sv-var ,fill-ptr) value)
                                   (setf ,fill-ptr (the fixnum (1+ ,fill-ptr))))
                                 (locally
                                     (declare (optimize (speed 1) (safety 3)))
                                   (,real-push value)))))
                      (declare (inline ,push-extend))
                      ,@code))
               (setf (fill-pointer ,fill-vec) ,fill-ptr))))))))

(defmacro with-unsafe-int-read ((reader-name vector) &body code)
  (with-unique-names (vec ptr)
    `(let ((,vec ,vector))
       (declare (type (simple-array uint8 (*)) ,vec))
       (sb-sys:with-pinned-objects (,vec)
         (let ((,ptr (sb-sys:vector-sap ,vec)))
           (declare (ignorable ,ptr))
           (macrolet ((,reader-name (offset size &key signed?)
                        `(the (,(if signed? 'signed-byte 'unsigned-byte) ,(* size 8))
                           (cffi:mem-ref ,',ptr
                                         ,(nth-value (if signed? 1 0)
                                                     (ecase size
                                                       (1 (values :uint8 :int8))
                                                       (2 (values :uint16 :int16))
                                                       (4 (values :uint32 :int32))
                                                       (8 (values :uint64 :int64))))
                                         ,offset))))
             ,@code))))))

(defmacro with-binsearch-in-array ((name vector elt-type comparator
                                         &key array-var right-edge?) &body code)
  (with-unique-names (arr-size)
    (let ((arr (or array-var (gensym "ARR")))
          (key-tmp-type (unless (eq elt-type '*)
                          (if (subtypep elt-type 'integer)
                              (if (subtypep elt-type 'unsigned-byte)
                                  'machine-uword 'machine-word)
                              elt-type))))
      `(multiple-value-bind (,arr ,arr-size) (get-vector-simple-array ,vector)
         (declare (type (simple-array ,elt-type (*)) ,arr)
                  (type fixnum ,arr-size))
         (assert (<= ,arr-size #.(ash most-positive-fixnum -1))) ; ensure no overflow
         (flet ((,name (key &optional (min-bound 0) (max-bound (length ,vector)))
                  (declare ,@(when key-tmp-type `((type ,key-tmp-type key)))
                           (type fixnum min-bound max-bound)
                           (optimize (speed 3) (safety 0)))
                  (assert (and (<= 0 min-bound max-bound ,arr-size)))
                  (setf key key) ; A hack to force the optimizer to unbox
                  (let ((work-min (1- min-bound))
                        (work-max max-bound))
                    (declare (type (integer -1 #.(ash most-positive-fixnum -1)) work-min work-max))
                    (loop (let ((mid (floor (+ work-min work-max) 2)))
                            (declare (type fixnum mid))
                            ,(if right-edge?
                                 ;; Searching for leftmost right index that compares true to the key
                                 `(cond ((= mid work-min)
                                         (return (if (< work-max max-bound) work-max nil)))
                                        ((funcall ,comparator key (aref ,arr mid))
                                         (setf work-max mid))
                                        (t
                                         (setf work-min mid)))
                                 ;; Searching for rightmost left index that compares true to the key
                                 `(cond ((= mid work-min)
                                         (return (if (< work-min min-bound) nil work-min)))
                                        ((funcall ,comparator (aref ,arr mid) key)
                                         (setf work-min mid))
                                        (t
                                         (setf work-max mid)))))))))
           (declare (inline ,name)
                    (ftype (function (,(or key-tmp-type elt-type)
                                       &optional fixnum fixnum) (or fixnum null)) ,name))
           ,@code)))))

(defmacro def-binsearch-fun (name &key (elt-type '*) (comparator 'cmp cmp-p))
  `(defun ,name (bs-vector key ,@(unless cmp-p '(&key (cmp #'<))))
     (declare ,@(unless cmp-p `((type function cmp)))
              #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
     (with-binsearch-in-array (searcher bs-vector ,elt-type ,comparator)
       (searcher key))))

(def-binsearch-fun binsearch-generic)
(def-binsearch-fun binsearch-uint32-< :elt-type uint32 :comparator #'<)
(def-binsearch-fun binsearch-uint32-<= :elt-type uint32 :comparator #'<=)


