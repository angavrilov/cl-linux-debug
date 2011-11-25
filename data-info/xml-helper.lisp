;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-xml)

(defparameter global nil)

(defmacro fmt (&rest args) `(format nil ,@args))

(defgeneric describe-obj (obj)
  (:method (obj) obj)
  (:method ((obj memory-object-ref))
    (describe-ref-value obj obj)))

(defgeneric get-int (obj offset size &key signed?)
  (:method ((obj null) offset size &key signed?)
    (declare (ignore signed?))
    0)
  (:method ((obj memory-object-ref) offset size &key signed?)
    (get-int (cl-linux-debug.data-info::memory-object-ref-memory obj)
             (+ (start-address-of obj) offset) size :signed? signed?))
  (:method ((obj memory-mirror) offset size &key signed?)
    (get-memory-integer obj offset size :signed? signed?))
  (:method ((obj memory-extent) offset size &key signed?)
    (get-memory-integer obj offset size :signed? signed?)))
