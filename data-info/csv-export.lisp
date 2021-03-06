;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(defparameter *csv-stream* nil)
(defparameter *anon-stem* nil)
(defparameter *anon-id* nil)
(defparameter *csv-merge-nested* nil)
(defparameter *csv-context* nil)

(defun subname (pname name)
  (if name
      (format nil "~@[~A.~]~A" pname (get-$-field-name name))
      pname))

(defun csv-comment-string (type)
  (remove-if (lambda (c) (case c ((#\Newline) t)))
             (or (comment-string-of type) "")))

(defun write-csv-entry (root level pname inc-offset type
                    &key item type-name name)
  (let* ((is-field? (typep type 'data-field))
         (offset (+ inc-offset (or (ignore-errors (effective-offset-of type)) 0)))
         (subname (subname pname (or name
                                     (cond ((typep type 'global-type-definition)
                                            (type-name-of type))
                                           (is-field? (name-of type)))))))
    (format *csv-stream* "\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\"~%"
            root level (format-hex-offset offset)
            (format-hex-offset (effective-size-of type))
            (or type-name (xml:xml-tag-name-string type))
            (or subname "") (or item "") (csv-comment-string type))))

(defun compute-container-entry (real-elt)
  (when (null real-elt)
    (return-from compute-container-entry (values nil (lambda () nil))))
  (let* ((elt (if (typep real-elt 'pointer-item)
                  (effective-contained-item-of real-elt)
                  real-elt))
         (anon? (not (or (typep elt 'global-type-proxy-base)
                         (typep elt 'primitive-field))))
         (elt-name (cond (anon?
                          (format nil "~A::anon~A" *anon-stem* (incf *anon-id*)))
                         ((typep elt 'global-type-proxy-base)
                          (get-$-field-name (type-name-of elt)))
                         (t
                          (xml:xml-tag-name-string elt)))))
    (values (format nil "~A~A" elt-name (if (eq elt real-elt) "" "*"))
            (lambda ()
              (when (and anon? (not *csv-merge-nested*))
                (export-csv-rec elt elt-name 0 nil 0))))))

(defgeneric export-csv-rec (type root level pname inc-offset &key)
  (:method ((type data-item) root level pname inc-offset &key name)
    (write-csv-entry root level pname inc-offset type :name name))
  (:method ((type virtual-compound-item) root level pname inc-offset &key name)
    (if (typep type 'primitive-field)
        (call-next-method)
        (let* ((is-field? (typep type 'data-field))
               (offset (+ inc-offset (or (ignore-errors (effective-offset-of type)) 0)))
               (subname (subname pname (or name (if is-field? (name-of type) nil)))))
          (when (or (= level 0) (not is-field?) (name-of type))
            (call-next-method))
          (dolist (sub (effective-fields-of type))
            (export-csv-rec sub root (1+ level) subname offset)))))
  (:method ((type global-type-proxy-base) root level pname inc-offset &key)
    (write-csv-entry root level pname inc-offset type
                     :type-name (get-$-field-name (type-name-of type))))
  (:method ((type global-type-proxy) root level pname inc-offset &key name)
    (if *csv-merge-nested*
        (export-csv-rec (effective-main-type-of type) root level pname
                        (+ inc-offset (or (ignore-errors (effective-offset-of type)) 0))
                        :name (or name (name-of type)))
        (call-next-method)))
  (:method ((type container-item) root level pname inc-offset &key)
    (multiple-value-bind (item list-cb)
        (compute-container-entry (effective-contained-item-of type))
      (write-csv-entry root level pname inc-offset type :item item)
      (funcall list-cb)))
  (:method ((type pointer) root level pname inc-offset &key)
    (if (and (eq (name-of type) $_vtable)
             (eq (type-name-of type)
                 (vtable-type-by-os (os-context-of *csv-context*))))
        (write-csv-entry root level pname inc-offset type
                         :item (format nil "~A::vtable" root))
        (call-next-method)))
  (:method ((type class-type) root level pname inc-offset &key)
    (call-next-method)
    (when (and (not *csv-merge-nested*)
               (null (inherits-from-of type))
               (virtual-methods-of type))
      (let* ((vroot (format nil "~A::vtable" root))
             (voffset 0)
             (dsize (destructor-vtbl-size-by-os (os-context-of *csv-context*)))
             (dargs (destructor-extra-args-by-os (os-context-of *csv-context*)))
             (margs (method-extra-args-by-os (os-context-of *csv-context*))))
        (loop for method in (methods-of (virtual-methods-of type)) and i from 0
           for size = (if (is-destructor-p method) dsize 4)
           and rtype = (awhen (ret-type-of method)
                         (if (typep it 'ret-type) (first (fields-of it)) it))
           for ritem = (when (typep rtype 'pointer)
                         (type-name-of rtype))
           and eitem = (cond ((typep rtype 'enum-field)
                              (type-name-of rtype))
                             ((typep rtype 'symbol) rtype))
           and asize = (loop for arg in (fields-of method)
                          sum (if (typep arg 'd-float) 8 4))
           do (progn
                ; multiple-value-bind (item list-cb)
                ;  (compute-container-entry nil #+() (when (typep ertype 'pointer)
                ;                             (effective-contained-item-of ertype)))
                (format *csv-stream* "\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\"~%"
                        vroot 1 (format-hex-offset voffset) (format-hex-offset size)
                        (format nil "vmethod(~A/~A)"
                                (length (fields-of method))
                                (+ asize (if (is-destructor-p method) dargs margs)))
                        (or (subname nil (name-of method))
                            (if (is-destructor-p method) (format nil "~~~A" root)
                                (format nil "vmethod~A" i)))
                        (cond (ritem (format nil "~A*" (get-$-field-name ritem)))
                              (eitem (format nil "~A" (get-$-field-name eitem)))
                              (t ""))
                        (csv-comment-string type))
               ; (funcall list-cb)
                (incf voffset size))))))
  (:method ((type static-array) root level pname inc-offset &key)
    (call-next-method)
    (let ((etype (car (effective-index-enum-tag-of type)))
          (elt (effective-contained-item-of type))
          (subname (subname pname (name-of type))))
      (unless (loop for ct = type then (effective-contained-item-of ct)
                 do (progn
                      (unless (typep ct 'static-array)
                        (return (or (typep ct 'primitive-field)
                                    (typep ct 'pointer-item))))
                      (when (effective-index-enum-tag-of ct)
                        (return nil))
                      (unless *csv-merge-nested*
                        (return t))))
        (loop
           with base-offset = (+ inc-offset (or (ignore-errors (effective-offset-of type)) 0))
           for i from 0 below (min 256 (count-of type))
           for key = (or $etype.keys[i] i)
           and offset = (* (effective-element-size-of type) i)
           do (export-csv-rec elt root (1+ level)
                              (format nil "~A[~A]" (or subname "")
                                      (if (is-$-keyword? key) (get-$-field-name key) key))
                              (+ base-offset offset))))))
  (:method ((type abstract-enum-item) root level pname inc-offset &key)
    (call-next-method)
    (dolist (field (effective-fields-of type))
      (format *csv-stream* "\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\"~%"
              root (1+ level) "" "" "enum-item"
              (aif (name-of field) (get-$-field-name it) "?") (effective-value-of field)
              (remove-if (lambda (c) (case c ((#\Newline) t)))
                         (or (comment-string-of field) ""))))))

(defun export-csv/types (stream context)
  (format stream "\"Type\",\"Level\",\"Offset\",\"Size\",\"Field Type\",\"Field Name\",\"Elt Type\",\"Comment\"~%")
  (let ((*csv-stream* stream)
        (*csv-context* context))
    (dolist (tn (sort (remove-if #'consp (mapcar #'car *known-types*))
                      (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
      (let ((type (lookup-type-in-context context tn)))
        (let ((*anon-id* 1)
              (*anon-stem* (get-$-field-name tn)))
          (export-csv-rec type *anon-stem* 0 nil 0))))))

(defun export-csv/globals (stream context)
  (format stream "\"Global\",\"Level\",\"Address\",\"Size\",\"Field Type\",\"Field Name\",\"Elt Type\",\"Comment\"~%")
  (let ((*csv-stream* stream)
        (*csv-context* context)
        (globals (mapcar (lambda (x)
                           (cons (lookup-global-in-context context (car x))
                                 (or (offset-of (cdr x))
                                     (gethash (car x)
                                              (global-address-table-of context)))))
                         *known-globals*)))
    (dolist (type (sort (remove-if (lambda (x) (null (cdr x))) globals)
                        (lambda (a b) (< (cdr a) (cdr b)))))
      (let ((*anon-id* 1)
            (*anon-stem* (get-$-field-name (name-of (car type))))
            (*csv-merge-nested* t))
        (export-csv-rec (effective-contained-item-of (car type))
                        *anon-stem* 0 nil (cdr type))))))

(defun export-csv (stream context &key globals?)
  (if globals?
      (export-csv/globals stream context)
      (export-csv/types stream context)))
