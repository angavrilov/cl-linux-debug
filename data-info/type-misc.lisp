;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

;; Primitives

(macrolet ((primitives (&rest names)
             `(progn
                ,@(loop for name in names
                     for kwd = (get-$-field (string-downcase (symbol-name name)))
                     collect `(setf (assoc-value *known-builtin-types* ',kwd) ',name)
                     collect `(defmethod %memory-ref-$ ((type ,name) ref (key (eql $_type))) ,kwd)))))
  (primitives int8_t uint8_t int16_t uint16_t
              int32_t uint32_t int64_t uint64_t
              bool static-string ptr-string stl-string
              s-float d-float
              flag-bit
              pointer))

;; User-readable output

(defgeneric format-ref-value-by-type (type ref value)
  (:method (type ref (value memory-object-ref))
    (if (eq value ref) "" (format-hex-offset (start-address-of value))))
  (:method (type ref (value null))
    "?")
  (:method (type ref value)
    value))

(defun format-ref-value (ref value)
  (format-ref-value-by-type (memory-object-ref-type ref) ref value))

(defun get-array-enum-value (type key)
  (awhen (car (effective-index-enum-tag-of type))
    (gethash key (gethash $values (lookup-tables-of it)))))

(defgeneric get-ref-child-links-by-type (type ref child key)
  (:method-combination append :most-specific-first)
  (:method append (type ref value key) nil)
  (:method append ((type sequence-item) ref value (key integer))
    (awhen (call-helper-if-found type $index-refers-to key ref :context-ref ref)
      (list (list $index-refers-to it)))))

(defmethod %memory-ref-@ ((type sequence-item) ref (key (eql $index-refers-to)))
  (lambda (index)
    (call-helper-if-found type $index-refers-to index ref :context-ref ref)))

(defun get-ref-child-links (ref child)
  (when (typep ref 'memory-object-ref)
    (get-ref-child-links-by-type (memory-object-ref-type ref) ref child
                                 (memory-object-ref-parent-key child))))

(defgeneric get-ref-links-by-type (type ref value)
  (:method-combination append :most-specific-first)
  (:method append (type ref value)
    (get-ref-child-links (memory-object-ref-parent-ref ref) ref))
  (:method append ((type primitive-field) ref value)
    (append
     (awhen (call-helper-if-found type $refers-to value ref :context-ref ref)
       (list (list $refers-to it)))
     (awhen (call-helper-if-found type $ref-target value ref :context-ref ref)
       (list (list $ref-target it))))))

(defmethod %memory-ref-@ ((type primitive-field) ref (key (eql $refers-to)))
  (call-helper-if-found type $refers-to (%memory-ref-$ type ref t) ref :context-ref ref))

(defmethod %memory-ref-@ ((type primitive-field) ref (key (eql $ref-target)))
  (call-helper-if-found type $ref-target (%memory-ref-$ type ref t) ref :context-ref ref))

(defun get-ref-links (ref value)
  (get-ref-links-by-type (memory-object-ref-type ref) ref value))

(defgeneric describe-ref-child-by-type (type ref child key)
  (:method-combination append :most-specific-first)
  (:method append (type ref value key) nil)
  (:method append ((type sequence-item) ref value (key integer))
    (append
     (ensure-list (call-helper-if-found type $describe-item value ref :context-ref ref)))))

(defun describe-ref-child (ref child)
  (when (typep ref 'memory-object-ref)
    (describe-ref-child-by-type (memory-object-ref-type ref) ref child
                                (memory-object-ref-parent-key child))))

(defun comment-string-of (node)
  (atypecase (comment-of node)
    (comment (xml::content it))
    (t it)))

(defgeneric describe-ref-value-by-type (type ref value)
  (:method-combination append :most-specific-first)
  (:method append (type ref value)
    (append
     (describe-ref-child (memory-object-ref-parent-ref ref) ref)
     (loop for (nil v) in (get-ref-links ref value)
        append (ensure-list (describe-value v)))))
  (:method append ((type code-helper-mixin) ref value)
    (flatten (ensure-list (call-helper-if-found type $describe
                                                value ref :context-ref ref))))
  (:method append ((type struct-compound-item) ref value)
    (awhen (aand (slot-value type 'key-field) (@ ref it))
      (let ((fname (get-$-field-name (slot-value type 'key-field)))
            (kfval ($ it t)))
        (list*
         (format nil "~A=~A" fname (format-ref-value it kfval))
         (awhen (and (typep kfval 'memory-object-ref)
                     (describe-ref-value kfval kfval))
           it)))))
  (:method append ((type inheriting-type) ref value)
    (awhen (effective-inherited-child-of type)
      (flatten (list
                (let ((rr (@ value (or (name-of it) it))))
                  (describe-ref-value-by-type (memory-object-ref-type rr) ref rr))
                (when (eq ref value)
                  (list (get-$-field-name (type-name-of type)))))))))

(defun describe-ref-value (ref value)
  (describe-ref-value-by-type (memory-object-ref-type ref) ref value))

(defgeneric describe-value (obj)
  (:method (obj) obj)
  (:method ((obj memory-object-ref))
    (describe-ref-value obj obj)))

;; Primitives

(defmethod special-code-helpers append ((type primitive-field))
  (awhen (ref-target-of type)
    (let ((tag (effective-tag-of (lookup-type-in-context *type-context* it)))
          (aux-code (awhen (aux-value-of type)
                      `(let (($ $$) ($$ $$._parent))
                         (declare (ignorable $ $$))
                         ,@(parse-helper it)))))
      (flet ((find-target (value aux ref)
               (call-helper-if-found (car tag) $find-instance value aux :context-ref ref)))
        (list (cons $ref-target
                    (compile-helper `((funcall ,#'find-target $ ,aux-code $$)))))))))

;; Integers

(defmethod %memory-ref-$ ((type integer-item) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (parse-int vector offset size :signed? (effective-int-signed? type)))))

(defmethod (setf %memory-ref-$) ((value integer) (type integer-item) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (setf (parse-int vector offset size) value)
      (request-memory-write ref 0 size))))

(defmethod describe-ref-value-by-type append ((type integer-item) ref (value integer))
  (list (format-hex-offset (unsigned value (* 8 (effective-size-of type))))))

;; Boolean

(defmethod %memory-ref-$ ((type bool) ref (key (eql t)))
  (awhen (call-next-method)
    (if (/= it 0) it nil)))

(defmethod (setf %memory-ref-$) ((value symbol) (type bool) ref (key (eql t)))
  (setf (%memory-ref-$ type ref key) (if value 1 0)))

(defmethod format-ref-value-by-type ((type bool) ref value)
  (if value "Y" "N"))

;; Float

(defmethod %memory-ref-$ ((type s-float) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (cffi:with-foreign-object (tmp :uint32)
        (setf (cffi:mem-ref tmp :uint32) (parse-int vector offset 4))
        (cffi:mem-ref tmp :float)))))

(defmethod (setf %memory-ref-$) ((value real) (type s-float) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (cffi:with-foreign-object (tmp :uint32)
        (setf (cffi:mem-ref tmp :float) (float value))
        (setf (parse-int vector offset 4) (cffi:mem-ref tmp :uint32)))
      (request-memory-write ref 0 size))))

(defmethod %memory-ref-$ ((type d-float) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (cffi:with-foreign-object (tmp :uint64)
        (setf (cffi:mem-ref tmp :uint64) (parse-int vector offset 8))
        (cffi:mem-ref tmp :double)))))

(defmethod (setf %memory-ref-$) ((value real) (type d-float) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (cffi:with-foreign-object (tmp :uint64)
        (setf (cffi:mem-ref tmp :double) (float value 1.0d0))
        (setf (parse-int vector offset 8) (cffi:mem-ref tmp :uint64)))
      (request-memory-write ref 0 size))))

;; Enum

(defmethod format-ref-value-by-type ((type abstract-enum-item) ref (value symbol))
  (if (is-$-keyword? value)
      (get-$-field-name value)
      (call-next-method)))

(defmethod describe-ref-value-by-type append ((type abstract-enum-item) ref (value symbol))
  (append (awhen $type.values[value]
            (list it))
          (loop for attr in (enum-attrs-of type)
             collect (format nil "~A=~A" (name-of attr) $type[(name-of attr)][value]))))

(defmethod @ ((type abstract-enum-item) (key symbol))
  (let* ((tables (lookup-tables-of type))
         (vtable (gethash $values tables))
         (ktable (gethash key tables)))
    (assert vtable)
    (when ktable
      (if (eq vtable ktable)
          (lambda (key)
            (if (integerp key) key
                (gethash key vtable)))
          (lambda (key)
            (gethash (gethash key vtable key) ktable
                     (gethash :default ktable)))))))

(defgeneric parse-type-value (type value)
  (:method (type value) value)
  (:method ((type abstract-enum-item) value)
    (get-$-field value))
  (:method ((type integer-item) value)
    (read-from-string value))
  (:method ((type bool) value)
    (cond ((numberp value) (/= value 0))
          ((member value '("true") :test #'equalp) t)
          ((member value '("false") :test #'equalp) nil)
          (t (error "Invalid boolean value: ")))))

(defmethod layout-type (context (type enum-attr))
  (setf (effective-table-of type)
        (make-hash-table)
        (effective-base-type-of type)
        (awhen (type-name-of type)
          (lookup-type-in-context *type-context* it)))
  (awhen (default-value-of type)
    (setf (gethash :default (effective-table-of type))
          (parse-type-value (effective-base-type-of type) it))))

(defmethod layout-fields (context (type abstract-enum-item) fields)
  (let ((val -1)
        (ftable (make-hash-table))
        (ktable (make-hash-table))
        (vtable (make-hash-table)))
    (dolist (attr (enum-attrs-of type))
      (layout-type-rec context attr)
      (setf (gethash (name-of attr) ftable) (effective-table-of attr)))
    (setf (gethash $keys ftable) ktable)
    (setf (gethash $values ftable) vtable)
    (dolist (field fields)
      (setf (effective-value-of field)
            (setf val (or (value-of field) (1+ val))))
      (awhen (name-of field)
        (when (gethash it vtable)
          (error "Duplicate key ~A in enum ~A" it (public-type-name-of type)))
        (setf (gethash it vtable) val
              (gethash val ktable) it))
      (dolist (attr (item-attrs-of field))
        (let* ((table (find (name-of attr) (enum-attrs-of type) :key #'name-of))
               (evalue (parse-type-value (effective-base-type-of table) (value-of attr) )))
          (unless table
            (error "Unknown enum attribute: ~A" (name-of attr)))
          (if (is-list-p table)
              (push evalue (gethash val (effective-table-of table)))
              (setf (gethash val (effective-table-of table)) evalue)))))
    (setf (lookup-tables-of type) ftable)))

(defmethod compute-effective-size (context (type enum-type)) 4)
(defmethod compute-effective-alignment (context (type enum-type)) 4)

(defmethod lookup-tables-of ((type global-type-proxy-base))
  (lookup-tables-of (effective-main-type-of type)))

(defmethod enum-attrs-of ((type global-type-proxy-base))
  (enum-attrs-of (effective-main-type-of type)))

(defmethod %memory-ref-$ ((type abstract-enum-item) ref (key (eql t)))
  (let ((iv (call-next-method)))
    (gethash iv (gethash $keys (lookup-tables-of type)) iv)))

(defmethod %memory-ref-$ ((type abstract-enum-item) ref (key symbol))
  (or $type[key][(%memory-ref-$ type ref t)] (call-next-method)))

(defmethod (setf %memory-ref-$) ((value symbol) (type abstract-enum-item) ref (key (eql t)))
  (aif $type.values[value]
       (setf (%memory-ref-$ type ref t) it)
       (call-next-method)))

(defmethod base-type-of ((type global-type-proxy-base))
  (or (call-next-method) (base-type-of (effective-main-type-of type))))

(defmethod layout-type :before (context (type base-type-item))
  (let ((btype (lookup-type-in-context *type-context* (or (base-type-of type) $int32_t))))
    (unless (typep btype 'integer-field)
      (error "Base type must be an integer: ~A" (base-type-of type)))
    (layout-type-rec context btype)
    (setf (effective-base-type-of type) btype
          (effective-int-signed? type) (effective-int-signed? btype))))

(defmethod compute-effective-size (context (type base-type-item))
  (effective-size-of (effective-base-type-of type)))

(defmethod compute-effective-alignment (context (type base-type-item))
  (effective-alignment-of (effective-base-type-of type)))

;; Bit

(defmacro with-bits-for-ref ((vector offset byte-size bit-shift bit-size)
                             (ref &optional size)
                             &body code)
  (with-unique-names (base shift size-var)
    (once-only (ref)
      `(bind (((:values ,base ,shift) (floor (memory-object-ref-address ,ref) 1))
              (,size-var ,(or size `(length-of ,ref)))
              (,byte-size (ceiling (+ ,shift ,size-var)))
              ((:values ,vector ,offset)
               (get-bytes-for-addr (memory-object-ref-memory ,ref) ,base ,byte-size))
              (,bit-shift (* ,shift 8))
              (,bit-size (* ,size-var 8)))
         (when ,vector
           ,@code)))))

(defmethod %memory-ref-$ ((type bit-item) ref (key (eql t)))
  (with-bits-for-ref (vector offset byte-size bit-shift bit-size)
      (ref (effective-size-of type))
    (ldb (byte bit-size bit-shift)
         (parse-int vector offset byte-size))))

(defmethod (setf %memory-ref-$) ((value integer) (type bit-item) ref (key (eql t)))
  (with-bits-for-ref (vector offset byte-size bit-shift bit-size)
      (ref (effective-size-of type))
    (let ((tmp (parse-int vector offset byte-size)))
      (setf (ldb (byte bit-size bit-shift) tmp) value)
      (setf (parse-int vector offset byte-size) tmp))
    (request-memory-write ref 0 (effective-size-of type))))

(defmethod %memory-ref-$ ((type flag-bit) ref (key (eql t)))
  (let ((iv (call-next-method)))
    (if (or (/= iv 0) (/= (effective-size-of type) 1/8))
        iv nil)))

(defmethod (setf %memory-ref-$) ((value symbol) (type flag-bit) ref (key (eql t)))
  (setf (%memory-ref-$ type ref t) (if value 1 0)))

(defmethod compute-effective-size (context (obj bit-item)) (* 1/8 (count-of obj)))
(defmethod compute-effective-alignment (context (obj bit-item)) 1/8)

(defmethod format-ref-value-by-type ((type flag-bit) ref value)
  (if (= (effective-size-of type) 1/8)
      (if value "Y" "N")
      (call-next-method)))

;; Pointer

(defun make-pointer-ref (memory ptr type parent key)
  (aprog1 (make-memory-ref memory ptr type :parent parent :key key)
    (adjust-mem-ref-type (memory-object-ref-type it) it)))

(defmethod %memory-ref-@ ((type pointer-item) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (let ((ptr (with-bytes-for-ref (vector offset ref size)
                 (parse-int vector offset size))))
      (unless (eql ptr 0)
        (make-pointer-ref (memory-object-ref-memory ref)
                          (or ptr 0) (effective-contained-item-of type) ref t)))))

(defmethod %memory-ref-@ ((type pointer-item) ref (key (eql $_target)))
  (%memory-ref-@ type ref t))

(defmethod %memory-ref-$ ((type pointer-item) ref (key (eql $_target)))
  (%memory-ref-@ type ref t))

(defmethod (setf %memory-ref-$) ((value integer) (type pointer-item) ref (key (eql $_target)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (setf (parse-int vector offset size) value)
      (request-memory-write ref 0 size))))

(defmethod (setf %memory-ref-$) ((value memory-object-ref) (type pointer-item) ref (key (eql $_target)))
  (setf (%memory-ref-$ type ref $_target) (memory-object-ref-address value)))

(defmethod (setf %memory-ref-$) ((value null) (type pointer-item) ref (key (eql $_target)))
  (setf (%memory-ref-$ type ref $_target) 0))

(defmethod %memory-ref-@ ((type pointer) ref key)
  (@ (%memory-ref-@ type ref t) key))

(defmethod %memory-ref-$ ((type pointer) ref key)
  ($ (%memory-ref-@ type ref t) key))

(defmethod %memory-ref-$ ((type pointer) ref (key (eql t)))
  (%memory-ref-@ type ref t))

(defmethod (setf %memory-ref-$) (value (type pointer) ref (key (eql t)))
  (setf (%memory-ref-$ type ref $_target) value))

(defmethod format-ref-value-by-type ((type pointer-item) ref (value null))
  "NULL")

(defmethod describe-ref-value-by-type append ((type pointer-item) ref (value memory-object-ref))
  (when (not (eq ref value))
    (append (describe-ref-value value value)
            (or (describe-address-in-context
                 (get-context-of-memory ref) (start-address-of value))
                (list "unknown area")))))

(defmethod build-effective-pointer-walker (context (node pointer) offset ctx)
  `(let ((ptr ,(access-walker-int ctx offset 4)))
     (when (/= ptr 0)
       (funcall ,(ptr-walker-ctx-report-cb ctx) ptr
                ',(effective-tag-of (effective-contained-item-of node))))))

;; Strings

(defmethod %memory-ref-$ ((type static-string) ref (key (eql t)))
  (let ((size (effective-size-of type)))
    (with-bytes-for-ref (vector offset ref size)
      (if (/= size 0)
          (parse-string vector offset
                        :limit (min (length vector) (+ size offset)))
          (parse-string vector offset)))))

(defmethod compute-effective-fields (context (type ptr-string))
  (list (make-instance 'pointer :name $ptr :type-name $static-string)))

(defmethod %memory-ref-$ ((type ptr-string) ref (key (eql t)))
  $ref.ptr.value)

(defmethod format-ref-value-by-type ((type string-field) ref (value string))
  (format nil "~S" value))

;; Abstract sequence

(defstruct lazy-seq
  (base-ref nil :read-only t)
  (count 0 :type fixnum)
  (offset 0 :type fixnum))

(defgeneric seq-item-count (seq)
  (:method ((seq null)) 0)
  (:method ((seq vector)) (length seq))
  (:method ((seq lazy-seq)) (lazy-seq-count seq)))

(defmethod $ ((seq vector) (key (eql $count)))
  (seq-item-count seq))
(defmethod $ ((seq lazy-seq) (key (eql $count)))
  (seq-item-count seq))

(defgeneric seq-item (seq index)
  (:method ((seq null) index) nil)
  (:method ((seq vector) index)
    (when (<= 0 index (length seq))
      (aref seq index)))
  (:method :around ((seq lazy-seq) index)
    (when (<= 0 index (1- (lazy-seq-count seq)))
      (call-next-method))))

(defmethod @ ((seq vector) (key integer))
  (seq-item seq key))
(defmethod @ ((seq lazy-seq) (key integer))
  (seq-item seq key))

(defgeneric %seq-slice (seq start end)
  (:method ((seq null) start end)
    nil)
  (:method ((seq vector) start end)
    (make-array (- end start)
                :displaced-to seq :displaced-index-offset start))
  (:method :around ((seq lazy-seq) start end)
    (aprog1 (call-next-method)
      (setf (lazy-seq-count it) (- end start)
            (lazy-seq-offset it) (+ (lazy-seq-offset seq) start)))))

(defun seq-slice (seq start &optional end)
  (let* ((cnt (seq-item-count seq))
         (rend (min (or end cnt) cnt)))
    (when (<= 0 start cnt)
      (%seq-slice seq start rend))))

(defgeneric seq-all-items (seq)
  (:method ((seq null)) #())
  (:method ((seq vector)) seq)
  (:method (seq)
    (loop
       with cnt = (seq-item-count seq)
       with vec = (make-array cnt)
       for i from 0 below cnt
       do (setf (aref vec i) (seq-item seq i))
       finally (return vec))))

(defgeneric seq-address-range (seq)
  (:method (seq) (values nil nil))
  (:method ((seq vector))
    (if (= (length seq) 0)
        (values nil nil)
        (let* ((saddr (start-address-of (aref seq 0)))
               (top saddr))
          (loop
             for item across seq
             do (when item
                  (let ((addr (start-address-of item)))
                    (when (or (< addr saddr) (> addr (+ top 1024)))
                      (return-from seq-address-range (values nil nil)))
                    (setf top (+ addr (length-of item))))))
          (values saddr (- top saddr))))))

(defparameter *array-item-internal* nil)

(defgeneric sequence-content-items (type ref)
  (:method ((type sequence-item) ref) nil)
  (:method :around (type ref)
    (let ((*array-item-internal* t))
      (call-next-method))))

(defmethod %memory-ref-@ ((type sequence-item) ref (key (eql $_items)))
  (sequence-content-items type ref))

(defmethod %memory-ref-@ ((type sequence-item) ref (key integer))
  (seq-item (sequence-content-items type ref) key))

(defmethod %memory-ref-@ ((type sequence-item) ref (key symbol))
  (aif (and (null *array-item-internal*)
            (get-array-enum-value type key))
       (%memory-ref-@ type ref it)
       (call-next-method)))

(defmethod %memory-ref-@ ((type sequence-item) ref (key (eql '*)))
  (coerce (seq-all-items (sequence-content-items type ref)) 'list))

(defmethod %memory-ref-$ ((type sequence-item) ref (key (eql $count)))
  (seq-item-count (sequence-content-items type ref)))

(defmethod %memory-ref-$ ((type sequence-item) ref (key (eql $has-items)))
  (let ((items (sequence-content-items type ref)))
    (and items (aand (seq-item-count items) (not (eql it 0))))))

(defmethod format-ref-value-by-type ((type sequence-item) ref value)
  (format nil "[~A]"
          (aif (sequence-content-items type ref)
               (seq-item-count it) "???")))

(defmethod describe-ref-value-by-type append ((type sequence-item) ref value)
  (awhen (comment-string-of type)
    (list it)))

(defmethod layout-type :before (context (type sequence-item))
  (setf (effective-index-enum-tag-of type)
        (awhen (index-enum-of type)
          (effective-tag-of (lookup-type-in-context *type-context* it)))))

(defun find-by-id (array id-field key)
  (when array
    (multiple-value-bind (cache found)
        (get-id-search-cache (get-context-of-memory array)
                             (memory-object-ref-address array)
                             (memory-object-ref-type array)
                             id-field)
      (unless found
        (dolist (item $array.*)
          (let ((kv (if (functionp id-field)
                        (funcall id-field item)
                        ($ item id-field))))
            (setf (gethash kv cache) item))))
      (gethash key cache))))

;; Abstract array

(defstruct (array-item-seq (:include lazy-seq))
  (extent nil)
  (elt-size 0 :type real)
  (elt-type nil)
  (start-ptr 0))

(defmethod seq-item ((seq array-item-seq) index)
  (make-memory-ref (array-item-seq-extent seq)
                   (+ (array-item-seq-start-ptr seq)
                      (* index (array-item-seq-elt-size seq)))
                   (array-item-seq-elt-type seq)
                   :parent (array-item-seq-base-ref seq)
                   :key (+ index (array-item-seq-offset seq))
                   :local? t))

(defmethod %seq-slice ((seq array-item-seq) start end)
  (aprog1 (copy-array-item-seq seq)
    (setf (array-item-seq-start-ptr it)
          (+ (array-item-seq-start-ptr seq)
             (* start (array-item-seq-elt-size seq))))))

(defmethod seq-address-range ((seq array-item-seq))
  (values (array-item-seq-start-ptr seq)
          (* (array-item-seq-count seq) (array-item-seq-elt-size seq))))

(defun wrap-array-item-seq (type ref base size &key (offset 0))
  (when size
    (let* ((elt-type (effective-contained-item-of type))
           (elt-size (effective-element-size-of type))
           (byte-size (ceiling (* elt-size size))))
      (when (and (typep size 'fixnum) (>= size 0) (or base (= size 0)))
        (if (= size 0)
            (make-array-item-seq :base-ref ref :count 0 :offset offset
                                 :elt-type elt-type :elt-size elt-size)
            (let ((extent (resolve-extent-for-addr (memory-object-ref-memory ref) base)))
              (when (and extent
                         (<= (- base (start-address-of extent))
                             (- (length-of extent) byte-size)))
                (make-array-item-seq :base-ref ref :count size :offset offset
                                     :elt-type elt-type :elt-size elt-size
                                     :extent extent :start-ptr base))))))))

(defstruct (chunked-array-item-seq (:include lazy-seq))
  (chunks (trees:make-binary-tree :red-black #'< :key #'array-item-seq-offset :test #'=))
  (chunk-list nil))

(defmethod seq-item ((seq chunked-array-item-seq) index)
  (let ((offidx (+ index (chunked-array-item-seq-offset seq))))
    (awhen (trees:lower-bound offidx (chunked-array-item-seq-chunks seq))
      (seq-item it (- offidx (array-item-seq-offset it))))))

(defmethod %seq-slice ((seq chunked-array-item-seq) start end)
  (copy-chunked-array-item-seq seq))

(defun wrap-chunked-array-item-seq (type ref chunk-list)
  (let* ((seq (make-chunked-array-item-seq :base-ref ref))
         (table (chunked-array-item-seq-chunks seq))
         (clist nil))
    (loop with off = 0
       for (base . size) in chunk-list
       do (when (> size 0)
            (awhen (wrap-array-item-seq type ref base size :offset off)
              (trees:insert it table)
              (push it clist))
            (incf off size))
       finally (setf (chunked-array-item-seq-count seq) off
                     (chunked-array-item-seq-chunk-list seq) (nreverse clist)))
    seq))

(defgeneric array-base-dimensions (type ref))

(defmethod sequence-content-items ((type array-item) ref)
  (multiple-value-bind (base size)
      (array-base-dimensions type ref)
    (wrap-array-item-seq type ref base size)))

(defgeneric build-set-array-base-dimensions (context node offset ctx ptr-var cnt-var))

(declaim (ftype (function (function uint32 (index-fixnum 4) t function) (values))
                %walk-pointer-array))

(defun %walk-pointer-array (memory ptr cnt elt-tag report-cb)
  (declare (type fixnum cnt)
           (type function memory report-cb)
           (optimize (speed 3)))
  (when (> cnt 0)
    (multiple-value-bind (vec off)
        (funcall memory ptr (* 4 cnt))
      (when vec
        (with-unsafe-int-read (get-int vec)
          (declare (type fixnum off))
          (let ((limit (+ off (* 4 cnt))))
            (declare (type (index-fixnum 1 4) off limit))
            (loop for p fixnum from off by 4 below limit
               and i fixnum from 0 below cnt
               for pv of-type uint32 = (get-int p 4)
               when (/= pv 0)
               do (locally (declare (optimize (speed 1)))
                    (funcall report-cb pv elt-tag)))))))))

(defun %build-array-pointer-walker (context node a-ptr a-cnt ctx)
  (with-unique-names (a-idx)
    (let* ((elt-type (effective-contained-item-of node))
           (e-size (effective-element-size-of node))
           (memory (ptr-walker-ctx-memory ctx))
           (report-cb (ptr-walker-ctx-report-cb ctx))
           (core
            (if (typep elt-type 'pointer)
                `(%walk-pointer-array ,memory ,a-ptr ,a-cnt
                                      ',(effective-tag-of (effective-contained-item-of elt-type))
                                      ,report-cb)
                (with-walker-ctx (memory a-ptr report-cb elt-type a-ctx
                                         :size-gap `(* ,e-size (1- ,a-cnt)))
                  `(loop for ,a-idx from 0 below ,a-cnt
                      do ,(build-effective-pointer-walker context elt-type 0 a-ctx)
                      do (setf ,(ptr-walker-ctx-base a-ctx)
                               (sb-ext:truly-the fixnum (+ ,(ptr-walker-ctx-base a-ctx) ,e-size))))))))
      `(when (< 0 ,a-cnt ,(floor most-positive-fixnum e-size))
         ,core))))

(defmethod build-effective-pointer-walker (context (node array-item) offset ctx)
  (with-unique-names (a-ptr a-cnt)
    `(let ((,a-ptr 0) (,a-cnt 0))
       (declare (type fixnum ,a-cnt)
                (type uint32 ,a-ptr))
       ,(build-set-array-base-dimensions context node offset ctx a-ptr a-cnt)
       ,(%build-array-pointer-walker context node a-ptr a-cnt ctx))))

;; Static array

(defmethod compute-effective-size (context (obj static-array))
  (* (effective-element-size-of obj) (count-of obj)))

(defmethod compute-effective-alignment (context (obj static-array))
  (effective-alignment-of (effective-contained-item-of obj)))

(defmethod array-base-dimensions ((type static-array) ref)
  (values (memory-object-ref-address ref) (count-of type)))

(defmethod compute-effective-has-pointers? (context (obj static-array))
  (effective-has-pointers? (effective-contained-item-of obj)))

(defmethod build-effective-pointer-walker (context (node static-array) offset ctx)
  (with-unique-names (a-base a-limit)
    (let* ((elt-type (effective-contained-item-of node))
           (e-size (effective-element-size-of node))
           (gap (* e-size (1- (count-of node))))
           (a-ctx (copy-ptr-walker-ctx ctx)))
      (assert (<= (ptr-walker-ctx-min-offset ctx)
                  (+ offset (effective-min-offset-of elt-type))
                  (+ offset gap (effective-max-offset-of elt-type))
                  (ptr-walker-ctx-max-offset ctx)))
      (setf (ptr-walker-ctx-base a-ctx) a-base)
      `(let* ((,a-base (+ ,(ptr-walker-ctx-base ctx) ,offset))
              (,a-limit (+ ,a-base ,gap)))
         (declare (type (index-fixnum 1 ,(effective-max-offset-of elt-type))
                        ,a-base ,a-limit))
         (loop while (<= ,a-base ,a-limit)
            do ,(build-effective-pointer-walker context elt-type 0 a-ctx)
            do (incf ,a-base ,e-size))))))

(defun copy-item-def (type)
  (copy-data-definition (effective-contained-item-of type)))

;; Array pointer

(defmethod substitute-type-class (context (type pointer))
  (if (is-array-p type)
      (change-class type 'pointer/array)
      (call-next-method)))

(declaim (ftype (function (* uint32 fixnum) fixnum) %array-pointer-dim))

(defun %array-pointer-dim (memory addr elt-size)
  (declare (optimize (speed 3)))
  (multiple-value-bind (size gap)
      (get-heap-chunk-size memory addr)
    (when (and size gap)
      (locally (declare (type uint32 size gap elt-size))
        (let* ((msz (floor size elt-size))
               (real-size (or (when (= gap 4)
                                (with-bytes-for-ref (vector offset memory 4 (- addr 4))
                                  (parse-int vector offset 4)))
                              msz)))
          (return-from %array-pointer-dim
            (values (min msz real-size)))))))
  (values -1))

(defmethod array-base-dimensions ((type pointer/array) ref)
  (awhen (%memory-ref-@ type ref t)
    (let ((sz (%array-pointer-dim (memory-object-ref-memory it)
                                  (memory-object-ref-address it)
                                  (effective-element-size-of type))))
      (when (>= sz 0)
        (values (memory-object-ref-address it) sz)))))

(defmethod build-set-array-base-dimensions (context (node pointer/array) offset ctx ptr-var cnt-var)
  `(let* ((addr ,(access-walker-int ctx offset 4)))
     (when (and (/= addr 0) (not (logtest addr 3)))
       (setf ,ptr-var addr ,cnt-var 1)
       (let ((sz (%array-pointer-dim *type-context* addr ,(effective-element-size-of node))))
         (when (<= 0 sz)
           (setf ,cnt-var sz))))))

;; Generic structure

(defun find-field-by-name (compound name &optional (offset 0))
  (dolist (field (if (typep compound 'virtual-compound-item)
                     (effective-fields-of compound)))
    (let ((field-offset (+ offset (effective-offset-of field))))
      (when (or (eq field name)
                (eql field-offset name))
        (return (cons field-offset field)))
      (if (name-of field)
          (when (eq (name-of field) name)
            (return (cons field-offset field)))
          (awhen (find-field-by-name (effective-main-type-of field)
                                     name field-offset)
            (return it))))))

(defun field-offset-by-name (compound name)
  (car (find-field-by-name compound name)))

(defun make-field-ref (ref field &optional (offset (effective-offset-of field)))
  (resolve-offset-ref ref (+ (memory-object-ref-address ref) offset)
                      field (or (name-of field) field)
                      :local? t))

(defmethod %memory-ref-@ ((type virtual-compound-item) ref key)
  (aif (find-field-by-name type key)
       (make-field-ref ref (cdr it) (car it))
       (call-next-method)))

(defmethod %memory-ref-@ ((type virtual-compound-item) ref (key (eql $_fields)))
  (lambda (key)
    (awhen (find-field-by-name type key)
      (let ((rref (make-field-ref ref (cdr it) (car it))))
        (if *in-@-funcall* rref ($ rref t))))))

(defmethod %memory-ref-@ ((type virtual-compound-item) ref (key (eql '*)))
  (mapcar (lambda (it) (make-field-ref ref it)) (effective-fields-of type)))

(defmethod %memory-ref-@ ((type virtual-compound-item) ref (key (eql '@)))
  (mapcar (lambda (it) (make-field-ref ref it)) (effective-fields-of type)))

;; Class

(defgeneric vtable-type-by-os (context)
  (:method ((context os-context)) nil)
  (:method ((context os-context/linux)) $glibc:vtable)
  (:method ((context os-context/windows)) $wine:vtable))

(defgeneric destructor-vtbl-size-by-os (context)
  (:method ((context os-context)) 4)
  (:method ((context os-context/linux)) 8))

(defmethod compute-effective-fields (context (type inheriting-type))
  (nconc (awhen (inherits-from-of type)
           (list (setf (effective-inherited-child-of type)
                       (make-instance 'compound :type-name it))))
         (call-next-method)))

(defmethod compute-effective-fields (context (type class-type))
  (nconc (when (null (inherits-from-of type))
           (list (make-instance 'pointer :name $_vtable
                                :type-name (vtable-type-by-os context))))
         (call-next-method)))

(defun %fast-adjust-class (context mem-cb addr)
  (with-fast-memory (vec base mem-cb addr)
    (resolve-class-in-context context (vec base 4))))

(defmethod adjust-mem-ref-type ((type class-type) ref)
  (with-bytes-for-ref (vector offset ref 4)
    (let ((context (get-context-of-memory ref)))
      (awhen (resolve-class-in-context context (parse-int vector offset 4))
        (setf (memory-object-ref-tag ref) (effective-tag-of it))))))
