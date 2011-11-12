;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug)

(defun process-proc-dir (process)
  (make-pathname :directory (list :absolute "proc" (format nil "~A" process))))

(defun process-memory-file (process)
  (merge-pathnames #P"mem" (process-proc-dir process)))

(defun process-thread-ids (process)
  (mapcar (lambda (path)
            (parse-integer (car (last (pathname-directory path)))))
          (directory (merge-pathnames #P"task/*/mem"
                                      (process-proc-dir process)))))
