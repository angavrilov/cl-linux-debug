;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(defparameter *csv-stream* nil)
(defparameter *anon-stem* nil)
(defparameter *anon-id* nil)
(defparameter *csv-merge-nested* nil)

(defun subname (pname name)
  (if name
      (format nil "~@[~A.~]~A" pname (get-$-field-name name))
      pname))

(defun write-csv-entry (root level pname inc-offset type
                    &key item type-name name)
  (let* ((is-field? (typep type 'data-field))
         (offset (+ inc-offset (or (ignore-errors (effective-offset-of type)) 0)))
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
    (let* ((real-elt (effective-contained-item-of type))
           (elt (if (typep real-elt 'pointer)
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
      (write-csv-entry root level pname inc-offset type
                       :item (format nil "~A~A" elt-name (if (eq elt real-elt) "" "*")))
      (when (and anon? (not *csv-merge-nested*))
        (export-csv-rec elt elt-name 0 nil 0))))
  (:method ((type static-array) root level pname inc-offset &key)
    (call-next-method)
    (let ((etype (car (effective-index-enum-tag-of type)))
          (elt (effective-contained-item-of type))
          (subname (subname pname (name-of type))))
      (unless (loop for ct = type then (effective-contained-item-of ct)
                 do (progn
                      (unless (typep ct 'static-array)
                        (return (or (typep ct 'primitive-field)
                                    (typep ct 'pointer))))
                      (when (effective-index-enum-tag-of ct)
                        (return nil))
                      (unless *csv-merge-nested*
                        (return t))))
        (loop
           for i from 0 below (min 256 (count-of type))
           for key = (or $etype.keys[i] i)
           and offset = (* (effective-element-size-of type) i)
           do (export-csv-rec elt root (1+ level)
                              (format nil "~A[~A]" (or subname "")
                                      (if (is-$-keyword? key) (get-$-field-name key) key))
                              (+ inc-offset offset))))))
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
  (let ((*csv-stream* stream))
    (dolist (tn (sort (remove-if #'consp (mapcar #'car *known-types*))
                      (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
      (let ((type (lookup-type-in-context context tn)))
        (let ((*anon-id* 1)
              (*anon-stem* (get-$-field-name tn)))
          (export-csv-rec type *anon-stem* 0 nil 0))))))

(defun export-csv/globals (stream context)
  (format stream "\"Global\",\"Level\",\"Address\",\"Size\",\"Field Type\",\"Field Name\",\"Elt Type\",\"Comment\"~%")
  (let ((*csv-stream* stream))
    (dolist (type (sort (remove-if (lambda (x) (null (offset-of x)))
                                   (mapcar (lambda (x) (lookup-global-in-context context (car x)))
                                           *known-globals*))
                        (lambda (a b) (< (offset-of a) (offset-of b)))))
      (let ((*anon-id* 1)
            (*anon-stem* (get-$-field-name (name-of type)))
            (*csv-merge-nested* t))
        (export-csv-rec type *anon-stem* 0 nil (offset-of type))))))

(defun export-csv (stream context &key globals?)
  (if globals?
      (export-csv/globals stream context)
      (export-csv/types stream context)))
