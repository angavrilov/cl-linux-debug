;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-user)

(defpackage :cl-linux-debug
  (:use :common-lisp
        :cffi :alexandria
        :anaphora :metabang-bind
        :hu.dwim.def
        :hu.dwim.defclass-star
        :bordeaux-threads
        :cont
        :chanl
        :cl-ppcre)
  (:shadowing-import-from #:bordeaux-threads
                          #:current-thread
                          #:thread-alive-p
                          #:threadp
                          #:thread-name
                          #:all-threads)
  (:export #:name-of
           #:task-finished?
           #:return-values-of
           #:signalled-condition-of
           #:exit-task #:abort-task #:yield-task
           #:with-exit-unwind
           #:make-debug-r/w-lock #:with-r/w-lock-held))
