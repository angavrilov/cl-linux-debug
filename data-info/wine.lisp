;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

<data-definition namespace='wine'>
  <struct-type type-name='list'>
    <pointer name='next' type-name='list'/>
    <pointer name='prev' type-name='list'/>
  </struct-type>

  <struct-type type-name='heap'>
    <pointer name='unknown1a'/>
    <pointer name='unknown1b'/>
    <uint32_t name='unknown2'/>
    <uint32_t name='flags'/>
    <uint32_t name='force_flags'/>
    <compound name='subheap' type-name='subheap'/>
    <compound name='entry' type-name='list'/>
    <compound name='subheap_list' type-name='list'/>
    <compound name='large_list' type-name='list'/>
    <uint32_t name='grow_size'/>
    <uint32_t name='magic'/>
    <uint32_t name='pending_pos'/>
    <pointer name='pending_free'/>
    RTL_CRITICAL_SECTION critSection
    FREE_LIST_ENTRY *freeList
  </struct-type>

  <struct-type type-name='subheap'>
    <uint32_t name='base'/>
    <uint32_t name='size'/>
    <uint32_t name='min_commit'/>
    <uint32_t name='commitSize'/>
    <compound name='entry' type-name='list'/>
    <pointer name='heap' type-name='heap'/>
    <uint32_t name='headerSize'/>
    <uint32_t name='magic'/>
  </struct-type>

  <struct-type type-name='arena_large'>
    <compound name='entry' type-name='list'/>
    <uint32_t name='data_size'/>
    <uint32_t name='block_size'/>
    <uint32_t name='pad1'/>
    <uint32_t name='pad2'/>
    <uint32_t name='size'/>
    <uint32_t name='magic'/>
  </struct-type>
</data-definition>

(defconstant +wine-arena-large-size+ #xfedcba90)

(defconstant +wine-arena-inuse-magic+ #x455355)
(defconstant +wine-arena-pending-magic+ #xbedead)
(defconstant +wine-arena-free-magic+ #x45455246)
(defconstant +wine-arena-large-magic+ #x6752614c)

(defun get-wine-process-heap-address (rgn)
  (let ((val (load-time-value (list nil))))
    (or (car val)
        (setf (car val)
              (bind ((orgn (origin-of rgn))
                     (image (image-of (cl-linux-debug.code-info::section-of orgn)))
                     (got (find-section-by-name image ".got.plt"))
                     (start (start-offset-of orgn))
                     (length (length-of orgn))
                     (insns (disassemble-all (data-bytes-of orgn)
                                             :start start :end (+ start length)
                                             :base-address (start-address-of orgn)))
                     (offset nil))
                (dolist (insn insns)
                  (let ((arg (x86-instruction-argument2 insn)))
                    (when (and (eq (x86-instruction-mnemonic insn) :mov)
                               (typep arg 'x86-argument-memory)
                               (eq (x86-argument-memory-base-reg arg) :ebx)
                               (eq (x86-argument-memory-index-reg arg) nil))
                      (let ((xoffset (x86-argument-memory-displacement arg)))
                        (if offset
                            (assert (= offset xoffset))
                            (setf offset xoffset))))))
                (assert (and offset got))
                (+ (start-address-of got) offset))))))

(defun get-wine-main-heap (memory)
  (awhen (find-regions-by-name (executable-of (process-of memory))
                               "RtlGetProcessHeaps")
    (bind ((rgn (first it))
           (addr (+ (relocation-offset-of rgn)
                    (get-wine-process-heap-address rgn)))
           (heap (get-memory-integer memory addr 4))
           (type (lookup-type-in-context memory $wine:heap)))
      (when heap
        (let ((ref (make-memory-ref memory heap type)))
          (when (eql $ref.magic #x50414548)
            ref))))))

(defun walk-wine-list (list-ref type field)
  (bind ((rtype (lookup-type-in-context (get-context-of-memory list-ref) type))
         ((bias . rfield) (find-field-by-name rtype field))
         (offset (+ bias (effective-offset-of rfield))))
    (loop for ref = $list-ref.next then $ref.next
       and i from 0
       until (or (address= ref list-ref)
                 (null ref)
                 (= (start-address-of ref) 0))
       collect (resolve-offset-ref list-ref
                                   (- (start-address-of ref) offset)
                                   rtype i))))

(defun enumerate-wine-malloc-chunks (memory result-vector start-address limit)
  (multiple-value-bind (bytes offset base-in)
      (get-bytes-for-addr memory start-address 4)
    (when (and bytes (< start-address (- limit 4)))
      ;; Highly optimized section:
      (let ((offset-limit (min (length bytes) (- limit base-in))))
        (declare (type (vector uint32) result-vector)
                 (type (simple-array uint8 (*)) bytes)
                 (type uint32 base-in)
                 (type (integer 0 #.(- array-dimension-limit 8)) offset offset-limit)
                 (optimize (speed 3)))
        ;; Unsafe section:
        (with-simple-vector-fill (result-sv result-vector uint32)
          (with-unsafe-int-read (get-bytes-int bytes)
            (let ((addr start-address))
              (declare (type (unsigned-byte 32) addr)
                       (optimize (speed 3) (safety 0)))
              (loop while (< offset offset-limit)
                 do (let* ((size-word (get-bytes-int offset 4))
                           (size (logand size-word -4))
                           (gap (- most-positive-fixnum 8 offset)))
                      (when (>= size gap)
                        (return))
                      (incf size 8)
                      (let ((next-pos (+ offset size)))
                        (declare (type fixnum next-pos))
                        (when (> next-pos offset-limit)
                          (return))
                        (let* ((next-addr (+ addr size))
                               (free-bit (logand size-word 1))
                               (item (logand (logior (+ addr 4) free-bit) +uint32-mask+)))
                          (declare (type uint32 next-addr))
                          (result-sv/push-extend item)
                          (setf offset next-pos
                                addr next-addr)))))))))
      ;; End optimized section
      (let ((end-addr (+ base-in offset)))
        (vector-push-extend (logior end-addr 1) result-vector)
        (values (+ start-address 4) end-addr)))))

(defun enumerate-wine-large-chunk (memory result-vector start-address limit)
  (declare (ignore memory))
  (vector-push-extend start-address result-vector)
  (vector-push-extend (logior limit 1) result-vector)
  (values start-address limit))

(defun %adjust-wine-chunk-size (memory start max)
  (awhen (get-memory-integer memory start 4)
    (let* ((magic (logand it #xFFFFFF))
           (size (logand (ash it -24) #xFF)))
      (cond ((= magic +wine-arena-inuse-magic+)
             (- max 4 size))
            ((= magic +wine-arena-pending-magic+)
             start)))))

(defun get-wine-heap-ranges (heap)
  (append
   (loop for subheap in (walk-wine-list $heap.subheap_list $wine:subheap $entry)
      for base = $subheap.base
      collect (list (+ base $subheap.headerSize) (+ base $subheap.commitSize)
                    #'enumerate-wine-malloc-chunks #'%adjust-wine-chunk-size))
   (loop for bigchunk in (walk-wine-list $heap.large_list $wine:arena_large $entry)
      for base = (+ (start-address-of bigchunk) (length-of bigchunk))
      collect (list (- base 4) (+ base $bigchunk.data_size) #'enumerate-wine-large-chunk))))

(defun get-wine-heaps (memory)
  (awhen (get-wine-main-heap memory)
    (list* it (walk-wine-list $it.entry $wine:heap $entry))))

(defun find-wine-heap-ranges (memory)
  (mapcan #'get-wine-heap-ranges (get-wine-heaps memory)))
