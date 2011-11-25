;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(defun find-stl-strings (memory text &key any-prefix? any-suffix?)
  "Find STL strings that contain the given substring."
  (let* ((refs nil)
         (sbytes (aprog1 (make-array (length text) :element-type 'uint8)
                   (loop for c across text and i from 0
                      do (setf (aref it i) (char-code c)))))
         (text-size (length text))
         (min-size (+ #xC text-size 1)))
    (declare (type fixnum min-size text-size))
    ;; STL strings are always full malloc chunks.
    (do-malloc-chunks (bytes offset limit min-addr)
        (memory (malloc-chunks-of memory) :int-reader get-int)
      ;; Verify that the header looks sane; otherwise it isn't an STL string
      (when (>= (- limit offset) min-size)
        (let ((length (get-int offset 4))
              (capacity (get-int (+ offset 4) 4)))
          (when (and (typep length 'fixnum) (typep capacity 'fixnum)
                     (<= text-size length capacity (- limit offset #xC))
                     (or any-prefix? any-suffix? (= length text-size)))
            ;; Search for characters
            (let* ((sstart (+ offset #xC))
                   (send (+ offset #xC length))
                   (pos (search sbytes bytes :start2 sstart :end2 send)))
              (declare (optimize (speed 1)))
              (when (and pos
                         (if (not any-prefix?) (= pos sstart) t)
                         (if (not any-suffix?) (= (+ pos text-size) send)))
                (push (make-ad-hoc-memory-ref memory (+ min-addr #xC)
                                              (make-instance 'static-string :size length))
                      refs)))))))
    refs))
