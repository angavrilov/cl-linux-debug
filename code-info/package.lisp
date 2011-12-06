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
  (:export #:uint8 #:uint16 #:uint32 #:uint64
           #:int8 #:int16 #:int32 #:int64
           #:offset #:address
           #:format-hex-offset #:parse-hex-offset
           #:signed #:unsigned
           #:start-address-of #:length-of
           #:start-offset-of #:data-bytes-of #:data-bytes
           #:image-of #:file-offset-of #:section-name-of
           #:loaded? #:executable? #:writable?
           #:find-section-by-address #:find-section-by-name
           #:entry-address-of #:path-of #:shared-lib?
           #:relocated? #:image-section-of #:mapping-of
           #:loaded-image-of #:relocation-offset-of
           #:unwind-info-of #:origin-of #:executable-of
           #:main-image-of #:all-images-of #:sections-of
           #:find-region-by-address #:find-regions-by-name
           #:symbol-name-of
           #:make-chunk-table #:lookup-chunk
           #:index-chunks/uint32 #:lookup-indexed-chunk/uint32
           #:make-binsearch-uint32-vec
           #:with-simple-vector-fill
           #:with-unsafe-int-read
           #:with-binsearch-in-array
           #:binsearch-generic #:binsearch-uint32-< #:binsearch-uint32-<=
           #:make-byte-vector
           ))
