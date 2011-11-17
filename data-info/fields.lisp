;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(defgeneric @ (obj key &optional default)
  (:method ((obj list) key &optional default)
    (mapcan (lambda (x) (ensure-list (@ x key default))) obj))
  (:method (obj (key list) &optional default)
    (mapcan (lambda (x) (ensure-list (@ obj x default))) key)))

(defgeneric $ (obj key &optional default)
  (:method ((obj list) key &optional default)
    (mapcan (lambda (x) (ensure-list ($ x key default))) obj))
  (:method (obj (key list) &optional default)
    (mapcan (lambda (x) (ensure-list ($ obj x default))) key)))

(defvar *old-readtable*
  (progn
    (when (get-macro-character #\$)
      (warn "A reader for $ is already present."))
    (when (get-macro-character #\@)
      (warn "A reader for @ is already present."))
    (copy-readtable *readtable*)))

(defun register-$-var (name)
  (let ((fpkg (load-time-value
               (find-package :cl-linux-debug.field-names))))
    (multiple-value-bind (sym status)
        (intern name fpkg)
      (export sym fpkg)
      (values sym (null status)))))

(defun register-$-field (name)
  (multiple-value-bind (sym new?)
      (register-$-var name)
    (when new?
      (setf (get sym 'is-$-var?) t)
      (eval `(progn
               (declaim (inline ,sym))
               (defun ,sym (arg &optional def)
                 ($ arg ',sym def))
               (define-symbol-macro ,sym ',sym))))
    sym))

(defun add-$-prefix (name)
  (concatenate 'string "$" name))

(defun get-$-field (name)
  (aif (position #\: name)
       (cons (register-$-field (add-$-prefix (subseq name 0 it)))
             (register-$-field (add-$-prefix (subseq name (1+ it)))))
       (register-$-field (add-$-prefix name))))

(defun get-$-field-name (field &key no-namespace?)
  (if (consp field)
      (progn
        (assert (and (symbolp (car field)) (symbolp (cdr field))))
        (if no-namespace?
            (get-$-field-name (cdr field))
            (concatenate 'string (get-$-field-name (car field))
                         ":" (get-$-field-name (cdr field)))))
      (progn
        (assert (and (typep field 'symbol) (get field 'is-$-var?)))
        (subseq (symbol-name field) 1))))

(defun main-$-reader (stream char)
  (let ((at? (char= char #\@))
        (real-dollars nil)
        (dollars nil)
        (name-list nil)
        (last-name nil)
        (namespace nil)
        (chars (list char)))
    (labels ((next ()
               (prog1 (setf char (read-char stream nil #.(code-char 0) t))
                 (push char chars)))
             (fail ()
               (error "Syntax error in $: ~A" (coerce (reverse chars) 'string)))
             (id-char? (char)
               (or (xml::digitp char)
                   (xml::letterp char)
                   (char= char #\_)
                   (char= char #\-)))
             (fallback (str)
               (let ((*readtable* *old-readtable*))
                 (read-from-string str)))
             (concat (&rest strs)
               (apply #'concatenate 'string (flatten strs)))
             (namespace-opt ()
               (if namespace (list namespace ":")))
             (register-$ (dstr nstr)
               (cond ((= (length dstr) 0)
                      (fallback (concat (namespace-opt) nstr)))
                     ((= (length dstr) 1)
                      (if namespace
                          `',(get-$-field (concat namespace ":" nstr))
                          (get-$-field nstr)))
                     (t
                      (when namespace (fail))
                      (register-$-var (concat dstr nstr)))))
             (push-name ()
               (unless last-name (fail))
               (push (if dollars
                         (let* ((dstr (subseq dollars 1)))
                           (setf dollars nil)
                           (register-$ dstr last-name))
                         last-name)
                     name-list)
               (setf last-name nil))
             (read-name ()
               (awhen (loop while (id-char? char) collect char do (next))
                 (coerce it 'string)))
             (read-$-name ()
               `(quote ,(get-$-field (or (read-name) (fail)))))
             (commit ()
               (unless (char= char #.(code-char 0))
                 (unread-char char stream))
               (return-from main-$-reader
                 (cond ((and (null name-list) (null last-name))
                        (if at?
                            (fallback real-dollars)
                            (register-$-var dollars)))
                       ((null last-name) (fail))
                       ((and dollars at?)
                        (fallback (concat real-dollars (namespace-opt) last-name)))
                       (dollars
                        (register-$ dollars last-name))
                       (t (let ((names (nreverse (list* last-name name-list))))
                            (loop while (cdr names)
                               for op = (if (and at? (null (cddr names))) '@ '$)
                               do (setf names `((,op ,(first names) ,(second names))
                                                ,@(cddr names))))
                            (first names)))))))
      (setf real-dollars (coerce (list* char (loop while (eql (next) #\$) collect char)) 'string))
      (setf dollars (concatenate 'string "$" (subseq real-dollars 1)))
      (if (id-char? char)
          (progn
            (setf last-name (read-name))
            (when (char= char #\:)
              (next)
              (setf namespace last-name
                    last-name (or (read-name) (fail)))))
          (when (char= char #\.)
            (setf last-name (register-$-var dollars)
                  dollars nil)))
      (loop
         (cond
           ((or (xml::white-space-p char)
                (char= char #\))
                (char= char #\])
                (char= char #.(code-char 0)))
            (commit))
           ;; .name
           ((char= char #\.)
            (push-name)
            (next)
            (if (char= char #\*)
                (progn
                  (setf last-name ''*)
                  (next))
                (setf last-name (read-$-name))))
           ;; [...]
           ((char= char #\[)
            (push-name)
            (let* ((lst (read-delimited-list #\] stream t))
                   (item (first lst)))
              (push #\] chars)
              (unless (= (length lst) 1) (fail))
              (setf last-name (if (eq item '*) ''* item)))
            (next))
           ;; error
           (t (fail)))))))

(defun conditional-$-reader (stream char)
  (if (member (load-time-value
               (find-package :cl-linux-debug.field-names))
              (package-use-list *package*))
      (main-$-reader stream char)
      ;; fallback
      (let ((*readtable* *old-readtable*))
        (unread-char char stream)
        (read stream))))

(set-macro-character #\$ #'conditional-$-reader t)
(set-macro-character #\@ #'conditional-$-reader t)
(set-macro-character #\] (get-macro-character #\) nil) nil)
