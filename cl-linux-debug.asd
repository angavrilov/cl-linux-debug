;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(load-system :cffi-grovel)

(defsystem :cl-linux-debug
  :author ("Alexander Gavrilov <angavrilov@gmail.com>")
  :depends-on (:anaphora
               :metabang-bind
               :cffi
               :hu.dwim.defclass-star
               :bordeaux-threads
               :chanl)
  :components ((:module
                "src"
                :components ((:file "package")
                             (:file "utils" :depends-on ("package"))
                             (cffi-grovel:grovel-file "bea-engine-grovel" :depends-on ("package"))
                             (:file "bea-engine" :depends-on ("bea-engine-grovel"))
                             (cffi-grovel:grovel-file "ptrace-grovel" :depends-on ("package"))
                             (:file "ptrace" :depends-on ("package" "ptrace-grovel"))))))

