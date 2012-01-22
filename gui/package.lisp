;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :common-lisp-user)

(defpackage :cl-linux-debug.gui
  (:use :common-lisp
        :alexandria :anaphora :metabang-bind
        :hu.dwim.def
        :hu.dwim.defclass-star
        :cl-linux-debug.code-info
        :cl-linux-debug
        :cl-linux-debug.field-names
        :cl-linux-debug.data-defs
        :cl-linux-debug.data-info
        :sb-gray
        :gtk :gdk :gobject)
  (:shadow #:within-main-loop)
  (:shadowing-import-from :cl-linux-debug.data-defs
                          #:alignment #:pointer #:value #:enum-item)
  (:shadowing-import-from :cl-linux-debug.code-info
                          #:parse-int #:parse-bytes #:parse-string)
  (:import-from :cl-linux-debug.data-info
                #:is-$-keyword? #:get-$-field-name
                #:layout-ad-hoc-in-context
                #:memory-object-ref-memory)
  (:export #:browse-object-in-new-window
           #:enable-gui-debugger-hook))

(in-package :cl-linux-debug.gui)
