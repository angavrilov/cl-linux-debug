;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug)

(deftype uint8 () '(unsigned-byte 8))
(deftype uint16 () '(unsigned-byte 16))
(deftype uint32 () '(unsigned-byte 32))

(deftype int8 () '(signed-byte 8))
(deftype int16 () '(signed-byte 16))
(deftype int32 () '(signed-byte 32))

(defun unsigned (value &optional (bits 32))
  (logand value (1- (ash 1 bits))))

(defun signed (value &optional (bits 32))
  (if (logbitp value (1- bits))
      (dpb value (byte bits 0) -1)
      value))
