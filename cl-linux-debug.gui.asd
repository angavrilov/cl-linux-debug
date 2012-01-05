;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :cl-linux-debug.gui
  :author ("Alexander Gavrilov <angavrilov@gmail.com>")
  :depends-on (:cl-linux-debug
               :cl-gtk2-gtk)
  :components ((:file "patches.gui")
               (:module
                "gui"
                :depends-on ("patches.gui")
                :components ((:file "package")
                             (:file "debug-hook" :depends-on ("package"))
                             (:file "tree-model" :depends-on ("package" "debug-hook"))
                             (:file "memory-objects" :depends-on ("package" "tree-model"))
                             (:file "memory-browser" :depends-on ("package" "memory-objects"))
                             (:file "list-browser" :depends-on ("package" "memory-browser"))
                             ))))
