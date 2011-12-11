;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

#|
#define request2size(req)                                         \
  (((req) + SIZE_SZ + MALLOC_ALIGN_MASK < MINSIZE)  ?             \
   MINSIZE :                                                      \
   ((req) + SIZE_SZ + MALLOC_ALIGN_MASK) & ~MALLOC_ALIGN_MASK)

#define fastbin_index(sz) \
  ((((unsigned int)(sz)) >> (SIZE_SZ == 8 ? 4 : 3)) - 2)
#define MAX_FAST_SIZE     (80 * SIZE_SZ / 4)
#define NFASTBINS  (fastbin_index(request2size(MAX_FAST_SIZE))+1)

#define NBINS             128

#define BINMAPSHIFT      5
#define BITSPERMAP       (1U << BINMAPSHIFT)
#define BINMAPSIZE       (NBINS / BITSPERMAP)
|#

<data-definition namespace='glibc'>
  <struct-type type-name='heap_info'>
    <pointer name='ar_ptr' type-name='malloc_state'/>
    <pointer name='prev' type-name='heap_info'/>
    <uint32_t name='size'/>
    <uint32_t name='mprotect_size'/>
  </struct-type>

  <struct-type type-name='malloc_state'>
    <comment>A GLibc malloc arena descriptor</comment>
    <int32_t name='mutex'/>
    <int32_t name='flags'/>
    <static-array name='fastbinsY' count='10' comment='NFASTBINS' type-name='pointer'/>
    <uint32_t name='top' comment='pointer'/>
    <pointer name='last_remainder'/>
    <static-array name='bins' count='254' comment='NBINS*2-2'>
      <pointer/>
    </static-array>
    <static-array name='binmap' count='4' comment='BINMAPSIZE'>
      <uint32_t/>
    </static-array>
    <pointer name='next' type-name='malloc_state'/>
    <pointer name='next_free' type-name='malloc_state'/>
    <int32_t name='system_mem'/>
    <int32_t name='max_system_mem'/>
  </struct-type>

  <struct-type type-name='vtable'>
    <pointer name='type_info' offset='-0x4' type-name='type_info'/>
    <static-array name='methods' count='16' type-name='pointer'/>

    <code-helper name='describe'>
      (fmt "Class: ~A" $.type_info.class_name)
    </code-helper>
  </struct-type>

  <class-type type-name='type_info'>
    <ptr-string name='class_name'/>
    <pointer name='base_class' type-name='type_info'/>
  </class-type>

  <global-object name='main_arena' type-name='malloc_state'/>
</data-definition>

(defconstant +page-size+ 4096)
(defconstant +max-glibc-heap-size+ (* 1024 1024))
(defconstant +chunk-header-size+ 8)

(defconstant +ptr-alignment-mask* 3)

(defun verify-glibc-heap (memory address)
  "Try walking chunks starting at address, and check if they make sense."
  (loop
     for current = address then (+ current size) and count from 0
     for prev-size = '#:none then size
     for size-word = (get-memory-integer memory (+ current 4) 4)
     for size = (logand (or size-word 0) (lognot 7))
     do (if (or (null size-word) (= size 0))
            (return nil)
            (case (logand size-word 7)
              (0 (when (not (eql (get-memory-integer memory current 4) prev-size))
                   (return nil))
                 (when (> count 60)
                   (return count)))
              (1 (when (> count 1000)
                   (return count)))
              (otherwise (return nil))))))

(defun get-bss-end (memory)
  (let* ((main-image (main-image-of (executable-of memory)))
         (bss (find-section-by-name main-image ".bss")))
    (+ (start-address-of bss) (length-of bss))))

(defun get-heap-start (memory)
  (dolist (ext (extents-of memory))
    (when (equal (memory-mapping-file-path (mapping-of ext)) "[heap]")
      (return-from get-heap-start (start-address-of ext)))))

(defun find-main-glibc-heap (memory)
  (let* ((base (max (get-bss-end memory)
                    (get-heap-start memory))))
    (loop for i from 0 below 4096
       and p from base by +chunk-header-size+
       when (verify-glibc-heap memory p)
       return p)))

(defmacro with-memory-area-walk ((bytes getter pos)
                                 (memory start-address limit size)
                                 &body code)
  (let* ((offset (symbolicate pos '#:/offset))
         (addr (symbolicate pos '#:/addr)))
    (with-unique-names (base-in offset-limit)
      `(multiple-value-bind (,bytes ,offset ,base-in)
           (get-bytes-for-addr ,memory ,start-address ,size)
         (when (and bytes (< ,start-address (- ,limit ,size)))
           (let ((,offset-limit (- (min (length ,bytes) (- ,limit ,base-in)) ,size))
                 (,addr ,start-address))
             (declare (type (simple-array uint8 (*)) ,bytes)
                      (type uint32 ,base-in ,addr)
                      (type (integer 0 ,(- array-dimension-limit size)) ,offset ,offset-limit))
             (setf ,addr ,addr ,offset ,offset)
             (with-unsafe-int-read (,getter ,bytes)
               (macrolet ((,(symbolicate pos '#:/advance) (delta &key (bias 0) (may-eq? nil))
                            (with-unique-names (gap dsize next-pos next-addr)
                              `(let* ((,gap (- ,,(- most-positive-fixnum size) ,',offset)))
                                 (when (<= ,delta ,gap)
                                   (let* ((,dsize (+ ,delta ,bias))
                                          (,next-pos (the fixnum (+ ,',offset ,dsize)))
                                          (,next-addr (logand (+ ,',addr ,dsize) +uint32-mask+)))
                                     (when (,(if may-eq? '<= '<) ,next-pos ,',offset-limit)
                                       (setf ,',addr ,next-addr ,',offset ,next-pos)
                                       t))))))
                          (,(symbolicate pos '#:/valid?) () '(< ,offset ,offset-limit)))
                 ,@code))))))))

(defun enumerate-glibc-malloc-chunks (memory result-vector start-address limit)
  "Scan a chain of malloc chunks, collecting their start addresses. Unused ones have the lowest bit set."
  (incf start-address (- +chunk-header-size+ 4))
  (with-memory-area-walk (bytes get-bytes-int pos)
      (memory start-address limit 4)
    (with-simple-vector-fill (result-sv result-vector uint32)
      ;; Optimized loop
      (let ((last-size-word (get-bytes-int pos/offset 4)))
        (declare (type uint32 last-size-word)
                 (optimize (speed 3) (safety 0)))
        (loop while (pos/valid?)
           do (let* ((size (logand last-size-word (lognot 7)))
                     (cur-addr pos/addr))
                (if (not (pos/advance size))
                    (return)
                    (let* ((next-size-word (get-bytes-int pos/offset 4))
                           (used-bit (logand (logxor 1 next-size-word) 1))
                           (item (logior cur-addr used-bit)))
                      (result-sv/push-extend item)
                      (setf last-size-word next-size-word))))))
      ;; End optimized section
      (result-sv/push-extend (logior pos/addr 1))
      (values start-address pos/addr))))

(defun enumerate-glibc-malloc-arenas (memory)
  (let ((main-arena (get-memory-global memory $glibc:main_arena)))
    (values main-arena
            (loop for arena = $main-arena.next then $arena.next
               until (or (address= arena main-arena)
                         (null arena)
                         (= (start-address-of arena) 0))
               collect arena))))

(defun find-aux-glibc-heap-chain (memory arena)
  (let* ((base-ptr (logand $arena.top (lognot (1- +max-glibc-heap-size+))))
         (heap-info-type (lookup-type-in-context memory $glibc:heap_info))
         (base-struct (make-memory-ref memory base-ptr heap-info-type)))
    (if (and (address= $base-struct.ar_ptr arena)
             (< $arena.top (+ base-ptr $base-struct.mprotect_size)))
        (list* base-struct
               (loop for struct = $base-struct.prev then $struct.prev
                  while struct collect struct))
        (cerror "ignore" "Couldn't find heap info at ~X" base-ptr))))

(defun find-aux-glibc-heap-range (heap-info)
  (let* ((arena $heap-info.ar_ptr)
         (start (if (address= arena @heap-info[1])
                    (align-up (start-address-of @arena[1]) +chunk-header-size+)
                    (start-address-of @heap-info[1])))
         (end (+ (start-address-of heap-info) $heap-info.mprotect_size))
         (top $arena.top))
    (list start (if (< top start) end (min (+ top +chunk-header-size+) end))
          #'enumerate-glibc-malloc-chunks)))

(defun find-glibc-heap-ranges (memory)
  (multiple-value-bind (main-arena aux-arenas)
      (enumerate-glibc-malloc-arenas memory)
    (list* (list (find-main-glibc-heap memory)
                 (+ $main-arena.top +chunk-header-size+)
                 #'enumerate-glibc-malloc-chunks)
           (loop for arena in aux-arenas
              for heaps = (find-aux-glibc-heap-chain memory arena)
              append (mapcar #'find-aux-glibc-heap-range heaps)))))
