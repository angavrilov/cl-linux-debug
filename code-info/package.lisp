;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-user)

(defpackage :cl-linux-debug.code-info
  (:use :common-lisp
        :cffi :alexandria
        :anaphora :metabang-bind
        :hu.dwim.def
        :hu.dwim.defclass-star
        :elf :trees
        :cl-ppcre)
  (:shadow #:parse-string)
  (:shadowing-import-from #:common-lisp #:type
                          #:reduce #:position #:delete
                          #:find)
  (:shadowing-import-from #:trees #:size #:emptyp)
  (:export #:uint8 #:uint16 #:uint32 #:uint64
           #:int8 #:int16 #:int32 #:int64
           #:signed #:unsigned
           #:start-address-of #:length-of
           #:start-offset-of #:data-bytes-of
           #:image-of #:file-offset-of #:section-name-of
           #:path-of #:shared-lib?
           #:relocated? #:image-section-of #:mapping-of
           #:unwind-info-of
           #:main-image-of #:all-images-of #:sections-of))
