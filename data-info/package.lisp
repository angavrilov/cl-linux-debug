;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-user)

(defpackage :cl-linux-debug.field-names
  (:use)
  (:export #:@ #:$ #:$$))

(defpackage :cl-linux-debug.data-defs
  (:use :common-lisp :xml
        :alexandria :anaphora
        :hu.dwim.def
        :hu.dwim.defclass-star
        :cl-linux-debug.field-names)
  (:import-from :cl-linux-debug
                #:name-of)
  (:import-from :cl-linux-debug.code-info
                #:int8 #:uint8
                #:int16 #:uint16
                #:int32 #:uint32
                #:int64 #:uint64)
  (:export #:*known-types* #:*known-types-version*
           #:*known-globals* #:*known-globals-version*
           #:*type-context* #:size-in-context #:lookup-type-reference
           #:copy-data-definition #:layout-type-rec
           #:name-with-namespace #:align-up
           #:format-hex-offset #:parse-hex-offset
           #:name))

(pushnew (find-package :cl-linux-debug.data-defs)
         xml:*xmlisp-packages*)

(defpackage :cl-linux-debug.data-info
  (:use :common-lisp
        :alexandria
        :anaphora :metabang-bind
        :hu.dwim.def
        :hu.dwim.defclass-star
        :bordeaux-threads
        :cont :cl-ppcre :xml
        :cl-linux-debug.code-info
        :cl-linux-debug
        :cl-linux-debug.data-defs
        :cl-linux-debug.field-names)
  (:shadowing-import-from :cl-linux-debug.code-info
                          #:parse-int #:parse-bytes #:parse-string)
  (:export ))

(pushnew (find-package :cl-linux-debug.data-info)
         xml:*xmlisp-packages*)

