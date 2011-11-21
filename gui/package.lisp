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
        :gtk :gdk :gobject)
  (:shadowing-import-from :cl-linux-debug.data-defs
                          #:alignment #:pointer)
  (:export #:browse-object-in-new-window))

(in-package :cl-linux-debug.gui)
