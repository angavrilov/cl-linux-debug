;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-user)

(defpackage :cl-linux-debug.code-info
  (:use :common-lisp
        :cffi :alexandria
        :anaphora :metabang-bind
        :hu.dwim.def
        :hu.dwim.defclass-star
        :elf
        :cl-ppcre)
  (:shadowing-import-from #:common-lisp #:type)
  (:export #:uint8 #:uint16 #:uint32 #:uint64
           #:int8 #:int16 #:int32 #:int64
           #:signed #:unsigned))
