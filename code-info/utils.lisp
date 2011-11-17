;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

;; Basic data types

(deftype uint8 () '(unsigned-byte 8))
(deftype uint16 () '(unsigned-byte 16))
(deftype uint32 () '(unsigned-byte 32))

(deftype int8 () '(signed-byte 8))
(deftype int16 () '(signed-byte 16))
(deftype int32 () '(signed-byte 32))

(def (structure ea) memory-mapping
  (start-addr 0)
  (end-addr 0)
  (readable? nil)
  (writable? nil)
  (executable? nil)
  (shared? nil)
  (file-offset 0)
  (file-path nil))

;; Signed/unsigned conversion

(declaim (inline signed unsigned))

(defun unsigned (value &optional (bits 32))
  (logand value (1- (ash 1 bits))))

(defun signed (value &optional (bits 32))
  (if (logbitp (1- bits) value)
      (dpb value (byte bits 0) -1)
      value))

;; Binary data parsing

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

(defun get-vector-simple-array (vector)
  (sb-kernel:with-array-data ((dv vector) (sv) (ev))
    (assert (= sv 0))
    (values dv ev)))

(defmacro with-simple-vector-fill ((sv-var fill-vector elt-type) &body code)
  (let ((size-var (symbolicate sv-var '#:/size))
        (push-extend (symbolicate sv-var '#:/push-extend)))
    `(multiple-value-bind (,sv-var ,size-var)
         (get-vector-simple-array ,fill-vector)
       (declare (type (simple-array ,elt-type (*)) ,sv-var)
                (type fixnum ,size-var))
       (macrolet ((,push-extend (value)
                    `(let ((tmp ,value)
                           (fp (fill-pointer ,',fill-vector)))
                       (declare (type ,',elt-type tmp)
                                (type fixnum fp))
                       (if (< fp ,',size-var)
                           (progn
                             (setf (aref ,',sv-var fp) tmp)
                             (incf (fill-pointer ,',fill-vector)))
                           (locally
                               (declare (optimize (speed 0) (safety 3)))
                             (vector-push-extend tmp ,',fill-vector)
                             (multiple-value-setq (,',sv-var ,',size-var)
                               (get-vector-simple-array ,',fill-vector)))))))
         ,@code))))

(defmacro def-binsearch-fun (name &key (elt-type '*) (comparator 'cmp cmp-p))
  `(defun ,name (bs-vector key ,@(unless cmp-p '(&key (cmp #'<))))
     (declare ,@(unless (eq elt-type '*) `((type ,elt-type key)))
              ,@(unless cmp-p `((type function cmp)))
              #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
              (optimize (speed 3)))
     (let ((min-bound -1)
           (max-bound (length bs-vector)))
       (declare (type (integer -1 #.(ash most-positive-fixnum -1)) min-bound max-bound))
       (multiple-value-bind (arr arr-size)
           (get-vector-simple-array bs-vector)
         (declare (type (simple-array ,elt-type (*)) arr)
                  (type fixnum arr-size))
         (assert (<= 0 max-bound arr-size))
         (locally (declare (optimize (safety 0)))
           (loop (let ((mid (floor (+ min-bound max-bound) 2)))
                   (declare (type fixnum mid))
                   (cond ((= mid min-bound)
                          (return min-bound))
                         ((funcall ,comparator key (aref arr mid))
                          (setf max-bound mid))
                         (t
                          (setf min-bound mid))))))))))

(def-binsearch-fun binsearch-generic)
(def-binsearch-fun binsearch-uint32-< :elt-type uint32 :comparator #'<)

