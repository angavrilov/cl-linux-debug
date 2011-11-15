;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-user)

(defpackage :cl-linux-debug.data-defs
  (:use :common-lisp :xml
        :alexandria :anaphora :metabang-bind
        :hu.dwim.def
        :hu.dwim.defclass-star)
  (:export ))

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
        :cl-linux-debug.data-defs)
  (:export ))

(pushnew (find-package :cl-linux-debug.data-info)
         xml:*xmlisp-packages*)

