;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-user)

(defpackage :cl-linux-debug.code-info
  (:use :common-lisp
        :cffi :alexandria
        :anaphora :metabang-bind
        :hu.dwim.def
        :hu.dwim.defclass-star
        :com.gigamonkeys.binary-data
        :elf :trees
        :cl-ppcre)
  (:shadow #:parse-string)
  (:shadowing-import-from #:common-lisp #:type
                          #:reduce #:position #:delete
                          #:find)
  (:shadowing-import-from #:trees #:size #:emptyp)
  (:export #:uint8 #:uint16 #:uint32 #:uint64 #:+uint32-mask+
           #:int8 #:int16 #:int32 #:int64
           #:machine-word #:machine-uword
           #:uint8-array #:index-fixnum
           #:address-int #:+min-address+ #:+max-address+
           #:offset #:address
           #:format-hex-offset #:parse-hex-offset
           #:signed #:unsigned
           #:start-address-of #:length-of
           #:start-offset-of #:data-bytes-of #:data-bytes
           #:image-of #:file-offset-of #:section-name-of
           #:loaded? #:executable? #:writable? #:is-64bit?
           #:find-section-by-address #:find-section-by-name
           #:entry-address-of #:path-of #:shared-lib?
           #:relocated? #:image-section-of #:mapping-of
           #:loaded-image-of #:relocation-offset-of
           #:unwind-info-of #:origin-of #:executable-of
           #:main-image-of #:all-images-of #:sections-of
           #:find-region-by-address #:find-regions-by-name
           #:symbol-name-of
           #:md5-hash-of #:binary-timestamp-of
           #:make-chunk-table #:lookup-chunk
           #:index-chunks #:lookup-indexed-chunk
           #:make-binsearch-uint32-vec
           #:with-simple-vector-fill #:with-vector-array
           #:with-unsafe-int-read
           #:with-binsearch-in-array
           #:binsearch-generic
           #:binsearch-uint32-< #:binsearch-uint32-<=
           #:binsearch-addr64-< #:binsearch-addr64-<=
           #:make-byte-vector
           #:disassemble-function
           #:region-unwind-table #:unwind-state-cfa
           ))
