;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-user)

(defpackage :cl-linux-debug
  (:use :common-lisp
        :cffi :alexandria
        :anaphora :metabang-bind
        :hu.dwim.def
        :hu.dwim.defclass-star
        :bordeaux-threads
        :cont :chanl
        :cl-ppcre
        :cl-linux-debug.code-info)
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
           #:ignored-signals-of
           #:exit-task #:abort-task #:yield-task
           #:with-exit-unwind
           #:make-debug-r/w-lock #:with-r/w-lock-held
           #:def-debug-task #:call-debug-task
           #:process-memory-maps
           #:with-thread-suspended
           #:with-threads-suspended
           #:with-any-thread-suspended
           #:read-process-data
           #:*debugged-processes*
           #:start-debug #:stop-debug
           #:resume-all-threads #:stop-all-threads))
