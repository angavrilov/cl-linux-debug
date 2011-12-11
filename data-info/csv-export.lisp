;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(defparameter *csv-stream* nil)
(defparameter *anon-stem* nil)
(defparameter *anon-id* nil)

(defun subname (pname name)
  (if name
      (format nil "~@[~A.~]~A" pname (get-$-field-name name))
      pname))

(defun write-csv-entry (root level pname inc-offset type
                    &key item type-name name)
  (let* ((is-field? (typep type 'data-field))
         (offset (+ inc-offset (if (and is-field? (> level 0))
                                   (effective-offset-of type) 0)))
         (subname (subname pname (or name
                                     (cond ((typep type 'global-type-definition)
                                            (type-name-of type))
                                           (is-field? (name-of type))))))
         (cstring (remove-if (lambda (c) (case c ((#\Newline) t)))
                             (or (comment-string-of type) ""))))
    (format *csv-stream* "\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\"~%"
            root level (format-hex-offset offset)
            (format-hex-offset (effective-size-of type))
            (or type-name (xml:xml-tag-name-string type))
            (or subname "") (or item "") cstring)))

(defgeneric export-csv-rec (type root level pname inc-offset)
  (:method ((type data-item) root level pname inc-offset)
    (write-csv-entry root level pname inc-offset type))
  (:method ((type virtual-compound-item) root level pname inc-offset)
    (if (typep type 'primitive-field)
        (call-next-method)
        (let* ((is-field? (typep type 'data-field))
               (offset (+ inc-offset (if (and is-field? (> level 0))
                                         (effective-offset-of type) 0)))
               (subname (subname pname (if is-field? (name-of type) nil))))
          (when (or (= level 0) (not is-field?) (name-of type))
            (call-next-method))
          (dolist (sub (effective-fields-of type))
            (export-csv-rec sub root (1+ level) subname offset)))))
  (:method ((type global-type-proxy) root level pname inc-offset)
    (write-csv-entry root level pname inc-offset type
                     :type-name (get-$-field-name (type-name-of type))))
  (:method ((type container-item) root level pname inc-offset)
    (let* ((real-elt (effective-contained-item-of type))
           (elt (if (typep real-elt 'pointer)
                    (effective-contained-item-of real-elt)
                    real-elt))
           (anon? (not (or (typep elt 'global-type-proxy)
                           (typep elt 'primitive-field))))
           (elt-name (cond (anon?
                            (format nil "~A::anon~A" *anon-stem* (incf *anon-id*)))
                           ((typep elt 'primitive-field)
                            (xml:xml-tag-name-string elt))
                           (t
                            (get-$-field-name (type-name-of elt))))))
      (write-csv-entry root level pname inc-offset type
                       :item (format nil "~A~A" elt-name (if (eq elt real-elt) "" "*")))
      (when anon?
        (export-csv-rec elt elt-name 0 nil 0))))
  (:method ((type abstract-enum-item) root level pname inc-offset)
    (call-next-method)
    (dolist (field (effective-fields-of type))
      (format *csv-stream* "\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\",\"~A\"~%"
              root (1+ level) "" ""
              (xml:xml-tag-name-string (effective-base-type-of type))
              (aif (name-of field) (get-$-field-name it) "?") (effective-value-of field)
              (remove-if (lambda (c) (case c ((#\Newline) t)))
                         (or (comment-string-of field) ""))))))

(defun export-csv (stream context)
  (format stream "\"Type\",\"Level\",\"Offset\",\"Size\",\"Field Type\",\"Field Name\",\"Elt Type\",\"Comment\"~%")
  (let ((*csv-stream* stream))
    (dolist (tn (sort (remove-if #'consp (mapcar #'car *known-types*))
                      (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
      (let ((type (lookup-type-in-context context tn)))
        (let ((*anon-id* 1)
              (*anon-stem* (get-$-field-name tn)))
          (export-csv-rec type *anon-stem* 0 nil 0))))))
