;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :cl-linux-debug.test
  :class hu.dwim.test-system
  :author ("Alexander Gavrilov <angavrilov@gmail.com>")
  :depends-on (:hu.dwim.stefil+hu.dwim.def
               :cl-linux-debug)
  :components ((:module
                "test"
                :components ((:file "package")
                             (:file "tasks" :depends-on ("package"))
                             ))))
