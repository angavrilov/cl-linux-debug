;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-xml)

(defparameter global nil)

(defmacro fmt (&rest args) `(format nil ,@args))

(defgeneric describe-obj (obj)
  (:method (obj) obj)
  (:method ((obj memory-object-ref))
    (describe-ref-value obj obj)))

(defun get-int (obj offset size &key signed?)
  (get-memory-integer obj offset size &key signed?))
