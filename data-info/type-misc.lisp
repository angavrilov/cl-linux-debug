;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

;; Primitives

(macrolet ((primitives (&rest names)
             `(progn
                ,@(loop for name in names
                     for kwd = (get-$-field (string-downcase (symbol-name name)))
                     collect `(setf (assoc-value *known-builtin-types* ',kwd) ',name)))))
  (primitives int8_t uint8_t int16_t uint16_t
              int32_t uint32_t int64_t uint64_t
              bool static-string ptr-string stl-string
              pointer))

;; User-readable output

(defgeneric format-ref-value-by-type (type ref value)
  (:method (type ref (value memory-object-ref))
    (if (eq value ref) "" (format-hex-offset (start-address-of value))))
  (:method (type ref (value null))
    "?")
  (:method (type ref value)
    value))

;; Integers

(defmethod %memory-ref-$ ((type integer-field) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (parse-int vector offset size :signed? (effective-int-signed? type)))))

(defmethod %memory-ref-$ ((type bool) ref (key (eql t)))
  (awhen (call-next-method)
    (/= it 0)))

(defmethod format-ref-value-by-type ((type bool) ref value)
  (if value "true" "false"))

;; Pointer

(defmethod %memory-ref-$ ((type pointer) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (let ((ptr (parse-int vector offset size)))
        (when (/= ptr 0)
          (resolve-offset-ref ref ptr (effective-contained-item-of type) t))))))

(defmethod %memory-ref-$ ((type pointer) ref key)
  ($ (%memory-ref-$ type ref t) key))

(defmethod format-ref-value-by-type ((type pointer) ref (value null))
  "NULL")

;; Strings

(defmethod %memory-ref-$ ((type static-string) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (if (/= size 0)
          (parse-string vector offset
                        :limit (min (length vector) (+ size offset)))
          (parse-string vector offset)))))

(defmethod compute-effective-fields ((type ptr-string))
  (list (make-instance 'pointer :name $ptr :type-name $static-string)))

(defmethod %memory-ref-$ ((type ptr-string) ref (key (eql t)))
  $ref.ptr[t])

;; Abstract array

(defgeneric array-base-dimensions (type ref))

(defun array-item-ref (type ref index &key (check-bounds? t))
  (multiple-value-bind (base size)
      (array-base-dimensions type ref)
    (when (and base
               (or (< -1 index size)
                   (not check-bounds?)))
      (let ((elt-size (effective-element-size-of type)))
        (values (resolve-offset-ref ref (+ base (* index elt-size))
                                    (effective-contained-item-of type)
                                    index)
                size
                elt-size)))))

(defmethod %memory-ref-$ ((type array-item) ref (key integer))
  ($ (array-item-ref type ref key) t))

(defun all-array-item-refs (type ref)
  (multiple-value-bind (first size step)
      (array-item-ref type ref 0)
    (when first
      (loop
         for i from 0 below size
         collect (offset-memory-reference first i step)))))

(defmethod %memory-ref-$ ((type array-item) ref (key (eql '@)))
  (all-array-item-refs type ref))

(defmethod %memory-ref-$ ((type array-item) ref (key (eql '*)))
  ($ (all-array-item-refs type ref) t))

(defmethod %memory-ref-$ ((type array-item) ref (key (eql $item)))
  (array-item-ref type ref 0 :check-bounds? nil))

(defmethod %memory-ref-$ ((type array-item) ref (key (eql $count)))
  (or (nth-value 1 (array-base-dimensions type ref)) 0))

(defmethod format-ref-value-by-type ((type array-item) ref value)
  (format nil "[~A]" $ref.count))

;; Static array

(defmethod compute-effective-size (context (obj static-array))
  (* (effective-element-size-of obj) (count-of obj)))

(defmethod compute-effective-alignment (context (obj static-array))
  (effective-alignment-of (effective-contained-item-of obj)))

(defmethod array-base-dimensions ((type static-array) ref)
  (values (memory-object-ref-address ref) (count-of type)))

;; STL vector

(defun copy-item-def (type)
  (copy-data-definition (effective-contained-item-of type)))

(defmethod compute-effective-fields ((type stl-vector))
  (list (make-instance 'pointer :name $start :fields (list (copy-item-def type)))
        (make-instance 'pointer :name $end)
        (make-instance 'pointer :name $block-end)))

(defmethod array-base-dimensions ((type stl-vector) ref)
  (let ((s $ref.start) (e $ref.end))
    (awhen (and s e)
      (values (start-address-of s)
              (/ (address- e s) (effective-element-size-of type))))))

;; Generic structure

(defmethod %memory-ref-@ ((type virtual-compound-item) ref (key symbol))
  (aif (find key (effective-fields-of type) :key #'name-of)
       (resolve-offset-ref ref (+ (memory-object-ref-address ref)
                                  (effective-offset-of it))
                           it key :local? t)
       (call-next-method)))

(defmethod %memory-ref-@ ((type virtual-compound-item) ref (key (eql '*)))
  (mapcar (lambda (it)
            (resolve-offset-ref ref (+ (memory-object-ref-address ref)
                                       (effective-offset-of it))
                                it (or (name-of it) (effective-tag-of it))
                                :local? t))
          (effective-fields-of type)))

