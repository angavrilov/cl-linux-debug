;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-user)

(defpackage :cl-linux-debug.field-names
  (:use)
  (:export #:@ #:$ #:$$
           #:|$-keyword| #:|$-keyword-namespace| #:|$-keyword-namespace-or-obj|
           #:is-$-keyword?  #:is-$-keyword-namespace?))

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
           #:auto-code-helpers
           #:name #:value #:os-type #:os-type-of ; to avoid conflict
           #:public-type-name-of
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
           *known-symtables*
           #:align-up
           #:get-$-field-name
           #:type-field-sequence
           #:lookup-type-in-context
           #:lookup-global-in-context
           #:memory-object-ref-type
           #:memory-object-ref-parent
           #:memory-object-ref-parent-key
           #:memory-object-ref-parent-ref
           #:effective-main-type-of
           #:address= #:address- #:value=
           #:format-field-seq
           #:format-ref-value
           #:comment-string-of
           #:describe-ref-value
           #:get-ref-links
           #:resolve-extent-for-addr
           #:get-bytes-for-addr
           #:get-context-of-memory
           #:make-memory-ref
           #:make-ad-hoc-memory-ref
           #:with-bytes-for-ref
           #:with-bits-for-ref
           #:with-safe-deref
           #:find-by-id
           #:make-memory-mirror
           #:check-refresh-context
           #:refresh-memory-mirror
           #:refresh-memory-mirror-ranges
           #:get-memory-bytes
           #:get-memory-integer
           #:get-memory-global
           #:get-address-object-info #:obj-type-of
           #:get-address-info-ref
           #:get-chunk-range-refs
           #:compile-helper
           #:call-helper #:call-helper-if-found
           #:describe-address-in-context
           #:get-address-info-range
           #:guess-types-by-data
           #:load-data-definition
           #:register-data-definition
           #:find-stl-strings #:find-memory-strings
           #:begin-find-changes #:update-find-changes #:get-found-changes
           #:os-type-of #:garbage-word-of
           #:export-csv
           ))

(pushnew (find-package :cl-linux-debug.data-info)
         xml:*xmlisp-packages*)

(defpackage :cl-linux-debug.data-xml
  (:use :common-lisp :xml
        :alexandria :anaphora :metabang-bind
        :cl-linux-debug.code-info
        :cl-linux-debug.field-names
        :cl-linux-debug.data-defs
        :cl-linux-debug.data-info)
  (:import-from :cl-linux-debug.code-info
                #:int8 #:uint8
                #:int16 #:uint16
                #:int32 #:uint32
                #:int64 #:uint64
                #:offset #:address
                #:format-hex-offset #:parse-hex-offset)
  (:intern #:global))

(pushnew (find-package :cl-linux-debug.data-xml)
         xml:*xmlisp-packages*)

