;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(def (class* e) object-memory-mirror (memory-mirror type-context)
  ((malloc-chunks (make-binsearch-uint32-vec) :reader t)))

(defmethod refresh-memory-mirror :after ((mirror object-memory-mirror))
  (check-refresh-context mirror)
  (with-simple-restart (continue "Ignore malloc chunks")
    (collect-malloc-objects mirror (malloc-chunks-of mirror))))
