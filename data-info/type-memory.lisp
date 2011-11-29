;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

;; Memory reference object

(def (structure e) memory-object-ref
  (memory nil)
  (address 0)
  (tag nil :type cons)
  (parent-ref nil)
  (parent-key nil))

(declaim (inline memory-object-ref-type))
(defun memory-object-ref-type (obj)
  (car (memory-object-ref-tag obj)))

(defmethod effective-main-type-of ((obj memory-object-ref))
  (effective-main-type-of (memory-object-ref-type obj)))

(defmethod start-address-of ((ref memory-object-ref))
  (memory-object-ref-address ref))

(defmethod length-of ((ref memory-object-ref))
  (effective-size-of (memory-object-ref-type ref)))

(defun address= (ref1 ref2)
  (= (start-address-of ref1) (start-address-of ref2)))

(defun address- (ref1 ref2)
  (- (start-address-of ref1) (start-address-of ref2)))

(defgeneric adjust-mem-ref-type (type ref)
  (:method (type ref) ref))

(defun format-field-seq (type)
  (format nil "~{~A~^.~}"
          (mapcar #'get-$-field-name (nreverse (type-field-sequence type)))))

(defmethod print-object ((ref memory-object-ref) stream)
  (print-unreadable-object (ref stream :identity t)
    (let ((type (memory-object-ref-type ref)))
      (format stream "REF ~A @~X: ~A"
              (xml-tag-name-string (effective-main-type-of type))
              (memory-object-ref-address ref)
              (ignore-errors (format-field-seq type))))))

(defgeneric lookup-type-in-context (context type-name)
  (:method :around (context type-name)
    (let ((*type-context* context))
      (call-next-method)))
  (:method (context (type-name null))
    nil))

(defgeneric lookup-global-in-context (context type-name)
  (:method :around (context type-name)
    (let ((*type-context* context))
      (call-next-method))))

(defgeneric layout-ad-hoc-in-context (context type-tree)
  (:method :around (context type-tree)
    (let ((*type-context* context))
      (call-next-method))
    type-tree))

(defgeneric get-id-search-cache (context address type field)
  (:method (context address type field)
    (values (make-hash-table :test #'equal) nil)))

(defgeneric resolve-class-in-context (context address)
  (:method (context address) nil))

(defgeneric describe-address-in-context (context address)
  (:method-combination append)
  (:method append (context address) nil))

(defgeneric get-context-of-memory (memory)
  (:method ((ref memory-object-ref))
    (get-context-of-memory (memory-object-ref-memory ref))))

(defgeneric resolve-extent-for-addr (extent address))

(defgeneric get-bytes-for-addr (extent address size)
  (:method ((ref memory-object-ref) address size)
    (get-bytes-for-addr (memory-object-ref-memory ref)
                        (+ (memory-object-ref-address ref) address)
                        size)))

(defmacro with-bytes-for-ref ((vector-var offset-var ref size &optional (offset 0)) &body code)
  `(multiple-value-bind (,vector-var ,offset-var)
       (get-bytes-for-addr ,ref ,offset ,size)
     (when ,vector-var
       ,@code)))

(defun get-memory-bytes (memory addr size)
  (with-bytes-for-ref (bytes offset memory size addr)
    (parse-bytes bytes offset size)))

(defun get-memory-integer (memory addr size &key signed?)
  (with-bytes-for-ref (bytes offset memory size addr)
    (parse-int bytes offset size :signed? signed?)))

(defun make-memory-ref (memory address type &key parent key local?)
  (declare (optimize (speed 3)))
  (let* ((extent (if local? memory (resolve-extent-for-addr memory address)))
         (tag (effective-tag-of type)))
    (assert extent)
    (make-memory-object-ref :memory extent
                            :address address
                            :tag tag
                            :parent-ref parent
                            :parent-key (or key tag))))

(defun resolve-offset-ref (base address type key &key local?)
  (make-memory-ref (memory-object-ref-memory base) address type
                   :parent base :key key :local? local?))

(defun make-ad-hoc-memory-ref (memory address type-tree &key no-copy? parent key local?)
  (assert (not (effective-finalized? type-tree)))
  (let ((type (layout-ad-hoc-in-context
               memory (if no-copy? type-tree
                          (copy-data-definition type-tree)))))
    (make-memory-ref memory address type :parent parent :key key :local? local?)))

(declaim (inline get-bytes-for-ref))
(defun get-bytes-for-ref (ref size)
  (get-bytes-for-addr (memory-object-ref-memory ref)
                      (memory-object-ref-address ref)
                      size))

(defparameter *safe-dereference* nil)

;;
;; Basic dereferencing
;;

(defun identity-key? (key)
  (case key (($this $it $me $value :value) t)))

(defun offset-memory-reference (ref shift-count shift-step)
  (if (= shift-count 0)
      ref
      (aprog1 (copy-memory-object-ref ref)
        (incf (memory-object-ref-address it) (* shift-count shift-step))
        (when (integerp (memory-object-ref-parent-key it))
          (incf (memory-object-ref-parent-key it) shift-count)))))

(defgeneric %memory-ref-@ (type ref key)
  (:method :around ((type global-type-proxy) ref key)
    (%memory-ref-@ (effective-main-type-of type) ref key))
  (:method (type ref key)
    (if (identity-key? key)
        (%memory-ref-@ type ref t)
        (cerror "ignore" "Invalid field access: ~S in ~S" key ref)))
  (:method (type ref (key integer))
    (offset-memory-reference ref key (effective-size-of type)))
  (:method (type ref (key (eql '*)))
    (declare (ignore type ref))
    nil)
  (:method (type ref (key (eql t)))
    (declare (ignore type))
    ref))

(defgeneric %memory-ref-$ (type ref key)
  (:method :around ((type global-type-proxy) ref key)
    (%memory-ref-$ (effective-main-type-of type) ref key))
  (:method (type ref key)
    (let ((refs (%memory-ref-@ type ref key)))
      (if (eq key '*)
          refs
          ($ refs t))))
  (:method (type ref (key (eql t)))
    (declare (ignore type))
    ref)
  (:method (type ref (key (eql $_address)))
    (memory-object-ref-address ref))
  (:method (type ref (key (eql $_size)))
    (length-of ref))
  (:method (type ref (key (eql $_parent)))
    (memory-object-ref-parent-ref ref))
  (:method (type ref (key (eql $_key)))
    (memory-object-ref-parent-key ref))
  (:method (type  ref (key (eql $_type)))
    nil)
  (:method ((type global-type-definition) ref (key (eql $_type)))
    (type-name-of type)))

(defmethod @ ((ref memory-object-ref) key &optional default)
  (or (%memory-ref-@ (memory-object-ref-type ref) ref key) default))

(defmethod $ ((ref memory-object-ref) key &optional default)
  (or (%memory-ref-$ (memory-object-ref-type ref) ref key) default))

;; Ptr walker

(defgeneric walk-reference-by-type (type ref report-cb)
  (:method :around ((type data-item) ref report-cb)
    (when (effective-has-pointers? type)
      (call-next-method)))
  (:method :around ((type global-type-proxy) ref report-cb)
    (walk-reference-by-type (effective-main-type-of type) ref report-cb))
  (:method ((type data-item) ref report-cb)
    nil)
  (:method ((type primitive-field) ref report-cb)
    nil))

(declaim (inline walk-reference))
(defun walk-reference (ref report-cb)
  (let ((type (memory-object-ref-type ref)))
    (when (effective-has-pointers? type)
      (walk-reference-by-type type ref report-cb))))

(defstruct ptr-walker-ctx
  memory root-node root-ptr report-cb
  base (min-offset 0) (max-offset 0) vector
  (get-int-cb (lambda (v b o s s?)
                `(parse-int ,v (+ ,b ,o) ,s :signed? ,s?)))
  (make-ref-cb (lambda (c n o)
                 `(make-memory-ref ,(ptr-walker-ctx-memory c)
                                   (+ ,(ptr-walker-ctx-root-ptr c) ,o)
                                   ,n))))

(defun access-walker-int (ctx offset size &key signed?)
  (maxf (ptr-walker-ctx-max-offset ctx) (+ offset size))
  (minf (ptr-walker-ctx-min-offset ctx) offset)
  (funcall (ptr-walker-ctx-get-int-cb ctx)
           (ptr-walker-ctx-vector ctx)
           (ptr-walker-ctx-base ctx) offset size signed?))

(defun make-walker-temp-ref (ctx node offset)
  (funcall (ptr-walker-ctx-make-ref-cb ctx) ctx node offset))

(defun report-walker-ptr (ctx ptr-value type)
  `(funcall ,(ptr-walker-ctx-report-cb ctx)
            (make-memory-ref ,(ptr-walker-ctx-memory ctx)
                             ,ptr-value ,type)))

(defgeneric build-effective-pointer-walker (context node offset walker-ctx)
  (:method (context (node data-item) offset walker-ctx)
    nil)
  (:method :around (context (node global-type-proxy) offset walker-ctx)
    (build-effective-pointer-walker context (effective-main-type-of node) offset walker-ctx))
  (:method (context (node virtual-compound-item) offset walker-ctx)
    `(progn
       ,@(loop for item in (effective-fields-of node)
            for code = (build-effective-pointer-walker context item
                                                       (+ offset (effective-offset-of item))
                                                       walker-ctx)
            when code collect code)))
  (:method (context (node container-item) offset ctx)
    (assert nil)))

(defun compile-effective-pointer-walker (context node)
  (with-unique-names (base-ref report-cb)
    (with-unique-names (n-memory n-ptr n-off n-vec)
      (let* ((n-ctx (make-ptr-walker-ctx :memory n-memory
                                         :root-ptr n-ptr
                                         :root-node node
                                         :base n-off
                                         :vector n-vec
                                         :report-cb report-cb))
             (n-code (build-effective-pointer-walker context node 0 n-ctx))
             (n-size (ptr-walker-ctx-max-offset n-ctx)))
        (if n-code
            `(lambda (,base-ref ,report-cb)
               (declare (optimize (debug 3)))
               (let ((,n-memory (get-context-of-memory ,base-ref))
                     (,n-ptr (memory-object-ref-address ,base-ref)))
                 (declare (type (integer 0 ,(- (ash 1 32) (ptr-walker-ctx-max-offset n-ctx) 1)) ,n-ptr))
                 (with-bytes-for-ref (,n-vec ,n-off (memory-object-ref-memory ,base-ref) ,n-size ,n-ptr)
                   (when (>= ,n-off ,(- (ptr-walker-ctx-min-offset n-ctx)))
                     (locally
                         (declare (type (integer 0 ,(- most-positive-fixnum n-size)) ,n-off)
                                  (type (simple-array uint8 (*)) ,n-vec))
                       ,n-code)))))
            `(lambda (x y) (declare (ignore x y))))))))

(defun get-effective-pointer-walker (context node)
  (or (effective-pointer-walker-of node)
      (setf (effective-pointer-walker-of node)
            (compile nil (compile-effective-pointer-walker context node)))))
