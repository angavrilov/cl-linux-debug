;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :common-lisp-user)

(defpackage :cl-linux-debug.test
  (:use :common-lisp
        :hu.dwim.def
        :hu.dwim.stefil
        :cl-linux-debug
        :cl-cont)
  (:export #:test))

(in-package :cl-linux-debug.test)

(defsuite* (test :in root-suite) ()
  (run-child-tests))

(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))
