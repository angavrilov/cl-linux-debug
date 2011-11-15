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

(defun proc-memory-maps (process)
  (with-open-file (stream (merge-pathnames #P"maps"
                                           (process-proc-dir process)))
    (loop for line = (read-line stream nil nil)
       while line
       collect (register-groups-bind (start end r w x p offset path)
                   ("^([0-9a-f]+)-([0-9a-f]+)\\s+(r|-)(w|-)(x|-)(p|s)\\s+([0-9a-f]+)\\s+[0-9a-f]+:[0-9a-f]+\\s+[0-9]+\\s+(\\S.*\\S)?"
                    line)
                 (check-type start string)
                 (check-type end string)
                 (check-type offset string)
                 (make-memory-mapping :start-addr (parse-integer start :radix 16)
                                      :end-addr (parse-integer end :radix 16)
                                      :readable? (equal r "r")
                                      :writable? (equal w "w")
                                      :executable? (equal x "x")
                                      :shared? (equal p "s")
                                      :file-offset (parse-integer offset :radix 16)
                                      :file-path path)))))

(defparameter *default-linux-start-address* #x08048000)

(defun select-main-executable (mappings)
  (let ((execs (remove-if-not (lambda (map)
                                (and (memory-mapping-executable? map)
                                     (aand (memory-mapping-file-path map)
                                           (not (equal it "[vdso]")))))
                              mappings)))
    (or (find-if (lambda (map) (= (memory-mapping-start-addr map)
                             *default-linux-start-address*)) execs)
        (find-if (lambda (map) (not (ends-with-subseq ".so" (memory-mapping-file-path map)))) execs)
        (first execs))))

(defun proc-read-memory (mem-file address vector &key (start 0) end)
  (file-position mem-file address)
  (let ((rpos (read-sequence vector mem-file :start start :end end)))
    (unless (= rpos (or end (length vector)))
      (error "Could not read memory fully at ~A...~A: ~A" address end rpos))))

