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

  <struct-type type-name='vtable'>
    <pointer name='type_info' offset='-0x4' type-name='RTTICompleteObjectLocator'/>
    <static-array name='methods' count='16' type-name='pointer'/>

    <code-helper name='describe'>
      (fmt "Class: ~A" $.type_info.pTypeDescriptor.name)
    </code-helper>
  </struct-type>

  <struct-type type-name='RTTICompleteObjectLocator'>
    <uint32_t name='signature'/>
    <uint32_t name='offset'/>
    <uint32_t name='cdOffset'/>
    <pointer name='pTypeDescriptor' type-name='TypeDescriptor'/>
    <pointer name='pClassDescriptor' type-name='RTTIClassHierarchyDescriptor'/>
  </struct-type>

  <struct-type type-name='TypeDescriptor' key-field='name'>
    <pointer name='pVFTable'/>
    <pointer name='spare'/>
    <static-string name='name' size='0'/>
  </struct-type>

  <struct-type type-name='RTTIClassHierarchyDescriptor'>
    <uint32_t name='signature'/>
    <uint32_t name='attributes'/>
    <uint32_t name='numBaseClasses'/>
    <pointer name='pBaseClassArray'>
      <static-array count='100'>
        <pointer key-field='pTypeDescriptor'>
          <pointer name='pTypeDescriptor' type-name='TypeDescriptor'/>
          <uint32_t name='numContainedBases'/>
          etc
        </pointer>
      </static-array>
    </pointer>
  </struct-type>
</data-definition>

(defconstant +wine-arena-large-size+ #xfedcba90)

(defconstant +wine-arena-inuse-magic+ #x455355)
(defconstant +wine-arena-pending-magic+ #xbedead)
(defconstant +wine-arena-free-magic+ #x45455246)
(defconstant +wine-arena-large-magic+ #x6752614c)

(defun get-wine-process-heap-address (memory)
  (let ((val (load-time-value (list nil))))
    (or (car val)
        (setf (car val)
              (multiple-value-bind (insns got reloc)
                  (disassemble-function (executable-of (process-of memory)) "RtlGetProcessHeaps")
                (when insns
                  (bind ((offset nil))
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
                    (+ reloc got offset))))))))

(defun get-wine-main-heap (memory)
  (awhen (get-wine-process-heap-address memory)
    (bind ((heap (get-memory-integer memory it 4))
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
  (with-memory-area-walk (bytes get-bytes-int pos)
      (memory start-address limit 16)
    (with-simple-vector-fill (result-sv result-vector uint32)
      ;; Optimized loop
      (locally
          (declare (optimize (speed 3) (safety 0)))
        (loop while (pos/valid?)
           do (let* ((size-word (get-bytes-int pos/offset 4))
                     (magic-word (get-bytes-int (+ pos/offset 4) 4))
                     (size (logand size-word -4))
                     (free-bit (logand size-word 1))
                     (cur-addr pos/addr))
                (if (not (pos/advance size :may-eq? t
                                      :bias (if (/= free-bit 0) 16 8)))
                    (return)
                    (let* ((magic (logand magic-word #xFFFFFF))
                           (magic-addr (logand (+ cur-addr 4) +uint32-mask+))
                           (out-free-bit (if (= magic +wine-arena-pending-magic+) 1 free-bit))
                           (item (logior magic-addr out-free-bit)))
                      (result-sv/push-extend item)
                      ;; Add a cutoff at the end
                      (when (= out-free-bit 0)
                        (let* ((gap (logand (ash magic-word -24) #xFF)))
                          (when (and (= magic +wine-arena-inuse-magic+) (>= gap 4))
                            (let ((eaddr (logand (- pos/addr (logand gap #xFC)) +uint32-mask+)))
                              (assert (< cur-addr eaddr))
                              (result-sv/push-extend (logior eaddr 1)))))))))))
      ;; End optimized section
      (result-sv/push-extend (logior pos/addr 1))
      (values (+ start-address 4) pos/addr))))

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
             (if (>= size 4)
                 (- max (logand size 3))
                 (- max 4 size)))
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
