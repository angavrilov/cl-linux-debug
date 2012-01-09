;; For SLIME
(require :sb-bsd-sockets)
(require :sb-introspect)
(require :sb-cltl2)

;; ASDF libs
(let* ((dist-dir (merge-pathnames #P"quicklisp/dists/quicklisp/software/"
                                  (user-homedir-pathname)))
       (asdf:*central-registry*
        (flet ((enum-dir (&rest path)
                 (mapcar (lambda (dpn)
                           (make-pathname
                            :host (pathname-host dpn)
                            :directory (pathname-directory dpn)))
                         (directory
                          (merge-pathnames
                           (make-pathname :directory (list* :relative path)
                                          :name :wild :type "asd")
                           dist-dir)))))
          (remove-duplicates
           (append
            (enum-dir :wild)
            (enum-dir :wild :wild))
           :test #'equal))))
  (print asdf:*central-registry*)
  (labels ((load-systems (&rest names)
             (dolist (name names)
               (asdf:load-system name))
             (dolist (name names)
               (let ((system (asdf:find-system name)))
                 (asdf::%set-system-source-file nil system)
                 (eval
                  `(defmethod asdf::do-traverse :around (op (system (eql ,system)) collect) nil))))))
    (load-systems :trivial-features
                  :trivial-garbage
                  :alexandria
                  :anaphora
                  :metabang-bind
                  :iterate
                  :split-sequence
                  :trivial-shell
                  :bordeaux-threads
                  :babel
                  :cffi
                  :cffi-grovel
                  :chanl
                  :closer-mop
                  :cl-cont
                  :cl-ppcre
                  :diff
                  :com.gigamonkeys.binary-data
                  :elf
                  :hu.dwim.asdf
                  :hu.dwim.def
                  :hu.dwim.defclass-star
                  :hu.dwim.defclass-star+hu.dwim.def
                  :hu.dwim.stefil
                  :trees
                  :cl-gtk2-gtk)))

;(inspect #'asdf::do-traverse)

(asdf:clear-configuration)
(save-lisp-and-die "sbcl-runtime" :executable t)
