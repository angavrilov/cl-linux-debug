;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :cl-linux-debug.gui
  :author ("Alexander Gavrilov <angavrilov@gmail.com>")
  :depends-on (:cl-linux-debug
               :cl-gtk2-gtk)
  :components ((:module
                "gui"
                :components ((:file "package")
                             (:file "memory-browser" :depends-on ("package"))
                             ))))
