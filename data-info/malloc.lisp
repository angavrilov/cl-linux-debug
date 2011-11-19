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
  </struct-type>

  <struct-type type-name='type_info'>
    <pointer name='_vtable' type-name='vtable'/>
    <ptr-string name='class_name'/>
    <pointer name='base_class' type-name='type_info'/>
  </struct-type>

  <global-object name='main_arena' type-name='malloc_state'/>
</data-definition>

(defconstant +page-size+ 4096)
(defconstant +max-malloc-heap-size+ (* 1024 1024))
(defconstant +chunk-header-size+ 8)

(defun verify-malloc-heap (memory address)
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

(defun find-main-malloc-heap (memory)
  (let* ((base (max (get-bss-end memory)
                    (get-heap-start memory))))
    (loop for i from 0 below 4096
       and p from base by +chunk-header-size+
       when (verify-malloc-heap memory p)
       return p)))

(defun enumerate-malloc-chunks (memory result-vector start-address limit)
  "Scan a chain of malloc chunks, collecting their start addresses. Unused ones have the lowest bit set."
  (multiple-value-bind (bytes offset base-in)
      (get-bytes-for-addr memory start-address +chunk-header-size+)
    (when (and bytes (< start-address (- limit +chunk-header-size+)))
      ;; Highly optimized section:
      (let ((offset-limit (- (min (length bytes) (- limit base-in))
                             +chunk-header-size+)))
        (declare (type (vector uint32) result-vector)
                 (type (simple-array uint8 (*)) bytes)
                 (type uint32 base-in)
                 (type (integer 0 #.(- array-dimension-limit 8)) offset offset-limit)
                 #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
                 (optimize (speed 3)))
        ;; Unsafe section:
        (with-simple-vector-fill (result-sv result-vector uint32)
          (let ((last-size-word (parse-int bytes (+ offset 4) 4))
                (addr (+ base-in offset)))
            (declare (type uint32 last-size-word)
                     (type (unsigned-byte 32) addr)
                     (optimize (speed 3) (safety 0)))
            (loop while (< offset offset-limit)
               do (let* ((size (logand last-size-word (lognot 7)))
                         (gap (- most-positive-fixnum offset)))
                    (when (or (<= size 8)
                              (>= size gap))
                      (return))
                    (let ((next-pos (+ offset size)))
                      (declare (type fixnum next-pos))
                      (when (>= next-pos offset-limit)
                        (return))
                      (let* ((next-addr (+ addr size))
                             (next-size-word (parse-int bytes (+ next-pos 4) 4))
                             (used-bit (logxor 1 (logand next-size-word 1)))
                             (item (logior addr used-bit)))
                        (declare (type uint32 next-addr))
                        (result-sv/push-extend item)
                        (setf last-size-word next-size-word
                              offset next-pos
                              addr next-addr))))))))
      ;; End optimized section
      (vector-push-extend (logior (+ base-in offset) 1) result-vector))
    result-vector))

(defun enumerate-malloc-arenas (memory)
  (let ((main-arena (get-memory-global memory $glibc:main_arena)))
    (values main-arena
            (loop for arena = $main-arena.next then $arena.next
               until (address= arena main-arena)
               collect arena))))

(defun find-aux-heap-chain (memory arena)
  (let* ((base-ptr (logand $arena.top (lognot (1- +max-malloc-heap-size+))))
         (base-struct (make-memory-ref memory base-ptr $glibc:heap_info)))
    (if (and (address= $base-struct.ar_ptr arena)
             (< $arena.top (+ base-ptr $base-struct.mprotect_size)))
        (list* base-struct
               (loop for struct = $base-struct.prev then $struct.prev
                  while struct collect struct))
        (cerror "ignore" "Couldn't find heap info at ~X" base-ptr))))

(defun find-aux-heap-range (heap-info)
  (let* ((arena $heap-info.ar_ptr)
         (start (if (address= arena @heap-info[1])
                    (align-up (start-address-of @arena[1]) +chunk-header-size+)
                    (start-address-of @heap-info[1])))
         (end (+ (start-address-of heap-info) $heap-info.mprotect_size))
         (top $arena.top))
    (cons start (if (< top start) end (min (+ top +chunk-header-size+) end)))))

(defun find-heap-ranges (memory)
  (multiple-value-bind (main-arena aux-arenas)
      (enumerate-malloc-arenas memory)
    (list* (cons (find-main-malloc-heap memory)
                 (+ $main-arena.top +chunk-header-size+))
           (loop for arena in aux-arenas
              for heaps = (find-aux-heap-chain memory arena)
              append (mapcar #'find-aux-heap-range heaps)))))

(defun collect-malloc-objects (memory &optional (result-vector (make-binsearch-uint32-vec)))
  (setf (fill-pointer result-vector) 0)
  (dolist (range (sort (find-heap-ranges memory) #'< :key #'car))
    (enumerate-malloc-chunks memory result-vector (car range) (cdr range)))
  result-vector)

(defun lookup-malloc-object (obj-vector address)
  (when (> address 0)
    (let ((idx (binsearch-uint32-< obj-vector (1- address))))
      (if (< idx 0) nil
          (let ((start (aref obj-vector idx)))
            (if (logtest start 1) nil
                (let ((min (+ start +chunk-header-size+))
                      (max (logand (aref obj-vector (1+ idx)) (lognot 1))))
                  (values min max (<= min address max)))))))))
