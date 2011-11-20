;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-user)

(defpackage :cl-linux-debug.field-names
  (:use)
  (:export #:@ #:$ #:$$
           #:|$-keyword| #:|$-keyword-namespace|
           #:|is-$-keyword?| #:|is-$-keyword-namespace?|))

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
                #:int64 #:uint64
                #:offset #:address
                #:format-hex-offset #:parse-hex-offset)
  (:export #:copy-data-definition #:name-with-namespace
           #:name ; to avoid conflict
           ))

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
  (:export *known-builtin-types*
           *known-types*
           *known-globals*
           #:align-up
           #:get-$-field-name
           #:type-field-sequence
           #:lookup-type-in-context
           #:lookup-global-in-context
           #:memory-object-ref-type
           #:memory-object-ref-parent
           #:memory-object-ref-parent-key
           #:effective-main-type-of
           #:address= #:address-
           #:format-field-seq
           #:format-ref-value
           #:describe-ref-value
           #:resolve-extent-for-addr
           #:get-bytes-for-addr
           #:make-memory-ref
           #:make-ad-hoc-memory-ref
           #:with-bytes-for-ref
           #:make-memory-mirror
           #:refresh-memory-mirror
           #:get-memory-bytes
           #:get-memory-integer
           #:get-memory-global
           #:get-address-object-info
           #:describe-address-in-context
           #:get-address-info-range
           #:guess-types-by-data
           ))

(pushnew (find-package :cl-linux-debug.data-info)
         xml:*xmlisp-packages*)

