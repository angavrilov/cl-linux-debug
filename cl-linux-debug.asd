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
               :chanl :cl-cont :elf)
  :components ((:module
                "code-info"
                :components ((:file "package")
                             (:file "utils" :depends-on ("package"))
                             (cffi-grovel:grovel-file "bea-engine-grovel" :depends-on ("package"))
                             (:file "bea-engine" :depends-on ("bea-engine-grovel"))
                             (:file "dwarf" :depends-on ("package" "utils"))))
               (:module
                "debugger"
                :depends-on ("code-info")
                :components ((:file "package")
                             (cffi-grovel:grovel-file "ptrace-grovel" :depends-on ("package"))
                             (:file "ptrace" :depends-on ("package" "ptrace-grovel"))
                             (:file "proc" :depends-on ("package"))
                             (:file "tasks" :depends-on ("package"))
                             (:file "classes" :depends-on ("package" "tasks"))
                             (:file "debug-process" :depends-on ("package" "classes" "ptrace" "proc"))
                             (:file "code-injection" :depends-on ("debug-process"))))))

