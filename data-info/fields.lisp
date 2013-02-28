;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

;; Access generics

(defparameter *in-@-funcall* nil)

(defgeneric @ (obj key) ; lvalue
  (:method ((obj function) key)
    (let ((*in-@-funcall* t))
      (funcall obj key)))
  (:method ((obj list) key) (assert nil))
  (:method (obj (key list)) (assert nil))
  (:method :around ((obj list) key)
    (mapcan (lambda (x) (ensure-list (@ x key))) obj))
  (:method :around (obj (key list))
    (mapcan (lambda (x) (ensure-list (@ obj x))) key)))

(defgeneric $ (obj key) ; rvalue
  (:method (obj key)
    ($ (@ obj key) t))
  (:method (obj (key (eql t)))
    obj)
  (:method ((obj function) key)
    (let ((*in-@-funcall* nil))
      (funcall obj key)))
  (:method ((obj list) key) (assert nil))
  (:method (obj (key list)) (assert nil))
  (:method :around ((obj list) key)
    (mapcan (lambda (x) (ensure-list ($ x key))) obj))
  (:method :around (obj (key list))
    (mapcan (lambda (x) (ensure-list ($ obj x))) key)))

(defgeneric (setf $) (value obj key)
  (:argument-precedence-order obj key value)
  (:method (value obj key)
    (setf ($ (@ obj key) t) value))
  (:method (value obj (key (eql t)))
    (error "Don't know how to set the value of: ~S" obj))
  (:method (value (obj list) key) (assert nil))
  (:method (value obj (key list)) (assert nil))
  (:method :around (value (obj list) key)
    (map nil (lambda (x) (setf ($ x key) value)) obj)
    value)
  (:method :around (value obj (key list))
    (map nil (lambda (x) (setf ($ obj x) value)) obj)
    value))

;; Save readtable

(defvar *old-readtable*
  (progn
    (when (get-macro-character #\$)
      (warn "A reader for $ is already present."))
    (when (get-macro-character #\@)
      (warn "A reader for @ is already present."))
    (copy-readtable *readtable*)))

;; Symbol management

(defun register-$-var (name)
  (let ((fpkg (load-time-value
               (find-package :cl-linux-debug.field-names))))
    (multiple-value-bind (sym status)
        (intern name fpkg)
      (export sym fpkg)
      (values sym (null status)))))

(defun init-$-field (sym)
  (unless (get sym 'is-$-var?)
    (setf (get sym 'is-$-var?) t)
    ;; Running eval for every identifier turns out to be very slow
    #+sbcl
    (progn
      (setf (fdefinition sym)
            (lambda (arg)
              ($ arg sym))
            (compiler-macro-function sym)
            (lambda (whole env)
              (declare (ignore env))
              (if (and (eq (first whole) sym)
                       (= (length whole) 2))
                  `($ ,(second whole) ',sym)
                  whole)))
      (sb-c::%define-symbol-macro sym `(quote ,sym) (sb-c:source-location)))
    #-sbcl
    (eval `(progn
             (declaim (inline ,sym))
             (defun ,sym (arg)
               ($ arg ',sym))
             (define-symbol-macro ,sym ',sym))))
  t)

(defun register-$-field (name)
  (multiple-value-bind (sym)
      (register-$-var name)
    (init-$-field sym)
    sym))

(defun add-$-prefix (name)
  (assert (not (equal name "")))
  (concatenate 'string "$" name))

(defun get-$-field (name)
  (aif (position #\: name)
       (cons (register-$-field (add-$-prefix (subseq name 0 it)))
             (register-$-field (add-$-prefix (subseq name (1+ it)))))
       (register-$-field (add-$-prefix name))))

(declaim (inline is-$-keyword? is-$-keyword-namespace?))

(defun is-$-keyword? (obj)
  (and (typep obj 'symbol)
       (eq (symbol-package obj)
           (load-time-value
            (find-package :cl-linux-debug.field-names)))
       (or (get obj 'is-$-var?)
           (init-$-field obj))))

(defun is-$-keyword-namespace? (obj)
  (or (is-$-keyword? obj)
      (and (consp obj) (is-$-keyword? (car obj)) (is-$-keyword? (cdr obj)))))

(defun get-$-field-name (field &key no-namespace?)
  (if (consp field)
      (progn
        (assert (is-$-keyword-namespace? field))
        (if no-namespace?
            (get-$-field-name (cdr field))
            (concatenate 'string (get-$-field-name (car field))
                         ":" (get-$-field-name (cdr field)))))
      (progn
        (assert (is-$-keyword? field))
        (subseq (symbol-name field) 1))))

(defun name-with-namespace (name namespace)
  (if (and namespace (symbolp name))
      (cons namespace name)
      name))

(defun namespace-by-name (name &optional default)
  (if (consp name) (car name) default))

;; Types

(deftype |$-keyword| () 'symbol)
(deftype |$-keyword-namespace| () '(or symbol cons))
(deftype |$-keyword-namespace-or-obj| () '(or symbol cons standard-object))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (value (type (eql '|$-keyword|)) stream)
  (format stream "\"~A\"" (get-$-field-name value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((value string) (Type (eql '|$-keyword|)))
  (aprog1 (get-$-field value)
    (assert (symbolp it))))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (value (type (eql '|$-keyword-namespace|)) stream)
  (format stream "\"~A\"" (get-$-field-name value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((value string) (Type (eql '|$-keyword-namespace|)))
  (get-$-field value))

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (value (type (eql '|$-keyword-namespace-or-obj|)) stream)
  (format stream "\"~A\"" (get-$-field-name value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((value string) (Type (eql '|$-keyword-namespace-or-obj|)))
  (get-$-field value))

;; Reader

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
                               for op = (if (or at? (cddr names)) '@ '$)
                               do (setf names `((,op ,(first names) ,(second names))
                                                ,@(cddr names))))
                            (first names)))))))
      (setf real-dollars (coerce (list* char (loop while (eql (next) #\$) collect char)) 'string))
      (setf dollars (concatenate 'string "$" (subseq real-dollars 1)))
      (cond ((id-char? char)
             (setf last-name (read-name))
             (when (char= char #\:)
               (next)
               (setf namespace last-name
                     last-name (or (read-name) (fail)))))
            ((or (char= char #\.) (char= char #\[))
             (setf last-name (register-$-var dollars)
                   dollars nil))
            ((char= char #\()
             (if (and at? (> (length dollars) 1))
                 (let ((body (read-delimited-list #\) stream t)))
                   (return-from main-$-reader
                     `(lambda (,(register-$-var (subseq dollars 1))) ,body)))
                 (progn
                   (unless (= (length dollars) 1) (fail))
                   (setf last-name (read-delimited-list #\) stream t)
                         dollars nil)
                   (push #\) chars)
                   (next)))))
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
            (aif (case char
                   (#\* ''*)
                   (#\@ ''@))
                 (progn
                   (setf last-name it)
                   (next))
                 (setf last-name (read-$-name))))
           ;; [...]
           ((char= char #\[)
            (push-name)
            (let* ((lst (read-delimited-list #\] stream t))
                   (item (first lst)))
              (push #\] chars)
              (unless (= (length lst) 1) (fail))
              (setf last-name (if (member item '(* @))
                                  `(quote ,item)
                                  item)))
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
