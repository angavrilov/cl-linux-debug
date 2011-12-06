;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

#-quicklisp
(load-system :cffi-grovel)
#+quicklisp
(ql:quickload :cffi-grovel)

(defsystem :cl-linux-debug
  :author ("Alexander Gavrilov <angavrilov@gmail.com>")
  :depends-on (:anaphora
               :metabang-bind
               :cffi
               :hu.dwim.def
               :hu.dwim.defclass-star+hu.dwim.def
               :bordeaux-threads
               :trees
               :chanl :cl-cont :elf)
  :components ((:module
                "xmlisp"
                :components ((:file "package")
                             (:file "XMLisp" :depends-on ("package"))))
               (:file "patches")
               (:module
                "code-info"
                :depends-on ("patches")
                :components ((:file "package")
                             (:file "utils" :depends-on ("package"))
                             (cffi-grovel:grovel-file "bea-engine-grovel" :depends-on ("package"))
                             (:file "bea-engine" :depends-on ("bea-engine-grovel"))
                             (:file "dwarf" :depends-on ("package" "utils"))
                             (:file "classes" :depends-on ("package" "dwarf"))
                             (:file "symbols" :depends-on ("classes" "bea-engine"))
                             (:file "elf" :depends-on ("classes" "symbols"))
                             (:file "exe" :depends-on ("classes" "symbols"))
                             (:file "executable" :depends-on ("classes" "elf" "exe"))))
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
                             (:file "debug-attach" :depends-on ("debug-process"))
                             (:file "code-injection" :depends-on ("debug-attach"))))
               (:module
                "data-info"
                :depends-on ("debugger" "xmlisp")
                :components ((:file "package")
                             (:file "fields" :depends-on ("package"))
                             (:file "types" :depends-on ("package" "fields"))
                             (:file "type-core" :depends-on ("package" "types"))
                             (:file "type-memory" :depends-on ("package" "type-core"))
                             (:file "type-misc" :depends-on ("package" "type-memory"))
                             (:file "type-context" :depends-on ("package" "type-core" "type-memory"))
                             (:file "xml-helper" :depends-on ("package" "type-core" "type-memory"))
                             (:file "memory" :depends-on ("package" "type-context"
                                                                    "type-memory" "xml-helper"))
                             (:file "malloc" :depends-on ("package" "memory"))
                             (:file "objects" :depends-on ("package" "memory" "malloc"))
                             (:file "search" :depends-on ("package" "objects"))))))

