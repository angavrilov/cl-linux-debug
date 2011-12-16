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

(defgeneric valid-ref? (ref)
  (:method ((ref t)) nil)
  (:method ((ref memory-object-ref))
    (and (/= (memory-object-ref-address ref) 0)
         (valid-ref? (memory-object-ref-memory ref)))))

(defun ensure-ref-address (ref)
  (etypecase ref
    (null 0)
    (memory-object-ref (memory-object-ref-address ref))))

(defun address= (ref1 ref2)
  (= (start-address-of ref1) (start-address-of ref2)))

(defun address- (ref1 ref2)
  (- (start-address-of ref1) (start-address-of ref2)))

(defgeneric value= (ref1 ref2)
  (:method (a b)
    (equal a b))
  (:method ((a null) b) nil)
  (:method (a (b null)) nil)
  (:method ((a cons) (b list))
    (and (= (length a) (length b))
         (every #'value= a b)))
  (:method ((a cons) b)
    (every (lambda (x) (value= x b)) a))
  (:method (a (b cons))
    (every (lambda (x) (value= a x)) b))
  (:method ((a memory-object-ref) (b memory-object-ref))
    (address= a b)))

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

(defgeneric %get-bytes-for-addr/fast-cb (extent)
  (:method ((ref memory-object-ref))
    (%get-bytes-for-addr/fast-cb (memory-object-ref-memory ref))))

(defmacro with-bytes-for-ref ((vector-var offset-var ref size &optional (offset 0)) &body code)
  `(multiple-value-bind (,vector-var ,offset-var)
       (get-bytes-for-addr ,ref ,offset ,size)
     (when ,vector-var
       (locally (declare (type (simple-array uint8 (*)) ,vector-var)
                         (type fixnum ,offset-var))
         ,@code))))

(defun get-memory-bytes (memory addr size)
  (with-bytes-for-ref (bytes offset memory size addr)
    (parse-bytes bytes offset size)))

(defun get-memory-integer (memory addr size &key signed?)
  (with-bytes-for-ref (bytes offset memory size addr)
    (parse-int bytes offset size :signed? signed?)))

(defgeneric request-memory-write (memory addr size))

(defun (setf get-memory-integer) (value memory addr size)
  (with-bytes-for-ref (bytes offset memory size addr)
    (setf (parse-int bytes offset size) value)
    (request-memory-write memory addr size))
  value)

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

(defun make-ad-hoc-type-reference (type)
  (if (typep type 'global-type-definition)
      (make-proxy-field nil type)
      (copy-data-definition (or (copy-origin-of type) type))))

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
  (:method (type ref (key null))
    (declare (ignore type))
    nil)
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

(defstruct ptr-walker-ctx
  memory root-ptr report-cb
  vector base size (min-offset 0) (max-offset 0))

(defun access-walker-int (ctx offset size &key signed?)
  (assert (<= (ptr-walker-ctx-min-offset ctx) offset
              (- (ptr-walker-ctx-max-offset ctx) size)))
  `(,(ptr-walker-ctx-vector ctx) (+ ,(ptr-walker-ctx-base ctx) ,offset) ,size :signed? ,signed?))

(defgeneric build-effective-pointer-walker (context node offset walker-ctx)
  (:method (context (node abstract-item) offset walker-ctx)
    nil)
  (:method :around (context (node data-item) offset walker-ctx)
    (when (effective-has-pointers? node)
      (call-next-method)))
  (:method :around (context (node global-type-proxy) offset walker-ctx)
    (build-effective-pointer-walker context (effective-main-type-of node) offset walker-ctx))
  (:method (context (node virtual-compound-item) offset walker-ctx)
    (loop for item in (effective-fields-of node)
       for code = (build-effective-pointer-walker context item
                                                  (+ offset (effective-offset-of item))
                                                  walker-ctx)
       when code collect code into items
       finally (when items
                 (return `(progn ,@items))))))

(defmacro with-walker-ctx ((memory ptr report-cb node ctx-var &key (size-gap 0)) &body code)
  (with-unique-names (vector base min-o max-o)
    `(with-unique-names (,vector ,base)
       (let* ((,min-o (effective-min-offset-of ,node))
              (,max-o (effective-max-offset-of ,node))
              (,ctx-var (make-ptr-walker-ctx :memory ,memory
                                             :root-ptr ,ptr
                                             :report-cb ,report-cb
                                             :vector ,vector :base ,base
                                             :min-offset ,min-o :max-offset ,max-o)))
         `(multiple-value-bind (,,vector ,,base)
              (funcall ,,memory ,,ptr (+ ,,size-gap ,,max-o))
            (declare (type fixnum ,,base))
            (when (and ,,vector (>= ,,base ,(- ,min-o)))
              (let ((,,base ,,base)
                    (,,ptr ,,ptr))
                (declare (type (integer 0 ,(- most-positive-fixnum ,max-o)) ,,base)
                         (type (integer 0 ,(- (ash 1 32) ,max-o 1)) ,,ptr)
                         (type (simple-array uint8 (*)) ,,vector))
                (setf ,,base ,,base ,,ptr ,,ptr)
                (with-unsafe-int-read (,,vector ,,vector)
                  ,(progn ,@code)))))))))

(defun compile-effective-pointer-walker (context node)
  (if (not (effective-has-pointers? node))
      nil
      (with-unique-names (memory ptr report-cb)
        (let ((n-code
               (with-walker-ctx (memory ptr report-cb node ctx)
                 (build-effective-pointer-walker context node 0 ctx))))
          `(lambda (,memory ,ptr ,report-cb)
             (declare (optimize (speed 3))
                      (type function ,memory ,report-cb)
                      (type uint32 ,ptr)
                      #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
             ,n-code)))))

(defun call-pointer-walker (context memory addr tag report-cb)
  (declare (optimize (speed 3)))
  (let* ((none '#:none)
         (walker (tag-attr tag 'pointer-walker none)))
    (when (eq walker none)
      (setf walker (awhen (compile-effective-pointer-walker context (car tag))
                     (compile nil it))
            (tag-attr tag 'pointer-walker) walker))
    (when walker
      (funcall (the function walker) memory addr report-cb))))
