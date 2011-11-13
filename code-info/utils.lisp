;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

(deftype uint8 () '(unsigned-byte 8))
(deftype uint16 () '(unsigned-byte 16))
(deftype uint32 () '(unsigned-byte 32))

(deftype int8 () '(signed-byte 8))
(deftype int16 () '(signed-byte 16))
(deftype int32 () '(signed-byte 32))

(defun unsigned (value &optional (bits 32))
  (logand value (1- (ash 1 bits))))

(defun signed (value &optional (bits 32))
  (if (logbitp (1- bits) value)
      (dpb value (byte bits 0) -1)
      value))

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

(defmacro parse-intf (vector start-var size-bytes &key signed?)
  (check-type size-bytes (integer 1 8))
  (if (= size-bytes 1)
      `(prog1 ,(if signed?
                   `(signed (aref ,vector ,start-var) 8)
                   `(aref ,vector ,start-var))
         (incf ,start-var))
      (with-unique-names (result)
        `(let ((,result 0))
           (declare (type (unsigned-byte ,(* size-bytes 8)) ,result)
                    (type (vector uint8) ,vector))
           ,@(loop for i from 0 below size-bytes
                collect `(setf (ldb (byte 8 ,(* 8 i)) ,result)
                               (aref ,vector (+ ,start-var ,i))))
           (incf ,start-var ,size-bytes)
           ,(if signed? `(signed ,result ,(* size-bytes 8)) result)))))

(defmacro parsef (cmd vector pos &rest args)
  (with-unique-names (rv size)
    `(multiple-value-bind (,rv ,size) (,cmd ,vector ,pos ,@args)
       (incf ,pos ,size)
       ,rv)))

(defun parse-string (vector start)
  (declare (type (vector uint8) vector))
  (let* ((length (loop for i from start
                    when (or (>= i (length vector))
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
