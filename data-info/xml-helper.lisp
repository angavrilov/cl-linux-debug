;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-xml)

(defparameter global nil)

(defmacro fmt (&rest args) `(format nil ,@args))

(defgeneric describe-obj (obj)
  (:method (obj) obj)
  (:method ((obj memory-object-ref))
    (describe-ref-value obj obj)))

(defun get-int (obj offset size &key signed?)
  (get-memory-integer obj offset size :signed? signed?))

(defun enum-to-int (enum value)
  (check-type value (or symbol integer))
  (if (integerp value) value
      ($ $global.enum[enum] value value)))

(defun enum-to-key (enum value)
  (check-type value (or symbol integer))
  (if (symbolp value) value
      ($ $global.enum[enum] value value)))
