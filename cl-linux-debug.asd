;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(load-system :cffi-grovel)

(defsystem :cl-linux-debug
  :author ("Alexander Gavrilov <angavrilov@gmail.com>")
  :depends-on (:anaphora
               :metabang-bind
               :cffi
               :hu.dwim.def
               :hu.dwim.defclass-star+hu.dwim.def
               :bordeaux-threads
               :chanl :cl-cont)
  :components ((:module
                "src"
                :components ((:file "package")
                             (:file "utils" :depends-on ("package"))
                             (cffi-grovel:grovel-file "bea-engine-grovel" :depends-on ("package"))
                             (:file "bea-engine" :depends-on ("bea-engine-grovel"))
                             (cffi-grovel:grovel-file "ptrace-grovel" :depends-on ("package"))
                             (:file "ptrace" :depends-on ("package" "utils" "ptrace-grovel"))
                             (:file "proc" :depends-on ("package" "utils"))
                             (:file "debug-process" :depends-on ("package" "ptrace" "proc"))))))

