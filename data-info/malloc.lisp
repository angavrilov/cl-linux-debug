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
(defconstant +max-malloc-heap-size+ (* 1024 1024))
(defconstant +chunk-header-size+ 8)

(defconstant +ptr-alignment-mask* 3)

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
  (incf start-address (- +chunk-header-size+ 4))
  (multiple-value-bind (bytes offset base-in)
      (get-bytes-for-addr memory start-address 4)
    (when (and bytes (< start-address (- limit 4)))
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
          (with-unsafe-int-read (get-bytes-int bytes)
            (let ((last-size-word (get-bytes-int offset 4))
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
                               (next-size-word (get-bytes-int next-pos 4))
                               (used-bit (logxor 1 (logand next-size-word 1)))
                               (item (logior addr used-bit)))
                          (declare (type uint32 next-addr))
                          (result-sv/push-extend item)
                          (setf last-size-word next-size-word
                                offset next-pos
                                addr next-addr)))))))))
      ;; End optimized section
      (let ((end-addr (+ base-in offset)))
        (vector-push-extend (logior end-addr 1) result-vector)
        (values start-address end-addr)))))

(defun enumerate-malloc-arenas (memory)
  (let ((main-arena (get-memory-global memory $glibc:main_arena)))
    (values main-arena
            (loop for arena = $main-arena.next then $arena.next
               until (or (address= arena main-arena)
                         (null arena)
                         (= (start-address-of arena) 0))
               collect arena))))

(defun find-aux-heap-chain (memory arena)
  (let* ((base-ptr (logand $arena.top (lognot (1- +max-malloc-heap-size+))))
         (heap-info-type (lookup-type-in-context memory $glibc:heap_info))
         (base-struct (make-memory-ref memory base-ptr heap-info-type)))
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

(defstruct malloc-chunk-map
  (range-vector (make-binsearch-uint32-vec 1048576))
  (extent-list nil)
  (extent-tbl (make-array (/ (ash 1 32) +max-malloc-heap-size+) :initial-element nil)))

(defun malloc-chunk-count (chunk-map)
  (length (malloc-chunk-map-range-vector chunk-map)))

(defun collect-malloc-objects (memory &optional (chunk-map (make-malloc-chunk-map)))
  (let ((vector (malloc-chunk-map-range-vector chunk-map))
        (ranges nil)
        (last-pos 1)
        (etbl (malloc-chunk-map-extent-tbl chunk-map)))
    ;; Wipe the range vector, and push the left guard
    (setf (fill-pointer vector) 0)
    (vector-push-extend 1 vector)
    (fill etbl nil)
    ;; Enumerate chunks
    (dolist (range (sort (find-heap-ranges memory) #'< :key #'car))
      (multiple-value-bind (start-addr end-addr)
          (enumerate-malloc-chunks memory vector (car range) (cdr range))
        ;; If added, register
        (let ((cur-pos (fill-pointer vector)))
          (when (> cur-pos last-pos)
            (push (list start-addr end-addr last-pos cur-pos) ranges)
            (let ((irange (cons last-pos cur-pos)))
              (setf last-pos cur-pos)
              (loop for i from (floor start-addr +max-malloc-heap-size+)
                 to (floor end-addr +max-malloc-heap-size+)
                 do (assert (null (aref etbl i)))
                 do (setf (aref etbl i) irange)))))))
    ;; Finish
    (setf (malloc-chunk-map-extent-list chunk-map) (nreverse ranges))
    chunk-map))

(defmacro with-malloc-chunk-lookup ((name chunk-map &key (range-vec (gensym))) &body code)
  (with-unique-names (etbl csearch)
    (once-only (chunk-map)
      `(let ((,etbl (malloc-chunk-map-extent-tbl ,chunk-map)))
         (declare (type simple-vector ,etbl))
         (with-binsearch-in-array (,csearch (malloc-chunk-map-range-vector ,chunk-map)
                                            uint32 #'< :array-var ,range-vec)
           (flet ((,name (addr)
                    (declare (type uint32 addr)
                             (optimize (speed 3) (safety 0)))
                    (let ((rvi nil) (rvo 0))
                      (declare (type (or fixnum null) rvi)
                               (type fixnum rvo))
                      (awhen (svref ,etbl (floor addr +max-malloc-heap-size+))
                        (awhen (,csearch addr (car it) (cdr it))
                          (let ((start (aref ,range-vec it)))
                            (when (not (logtest start 1))
                              (let ((offset (sb-ext:truly-the uint32 (- addr start))))
                                (declare (optimize (speed 1)))
                                (when (>= offset 4)
                                  (setf rvi it rvo (the fixnum (- offset 4)))))))))
                      (values rvi rvo))))
             (declare (inline ,name)
                      (ftype (function (uint32) (values (or null fixnum) fixnum)) ,name))
             ,@code))))))

(defun malloc-chunk-range (chunk-map id &optional address)
  (when id
    (let ((vec (malloc-chunk-map-range-vector chunk-map)))
      (let ((min (+ (aref vec id) 4))
            (max (logand (aref vec (1+ id)) (lognot 1))))
        (values min max (if address (<= min address max)))))))

(defun lookup-malloc-object (chunk-map address)
  (with-malloc-chunk-lookup (lookup chunk-map :range-vec vec)
    (awhen (lookup address)
      (let ((min (+ (aref vec it) 4))
            (max (logand (aref vec (1+ it)) (lognot 1))))
        (values it min max (<= address max))))))

(defmacro do-malloc-chunks ((bytes offset limit &optional (min-addr (gensym)) (max-addr (gensym)))
                            (memory chunk-map &key int-reader (index (gensym))) &body code)
  (with-unique-names (range-vec heap start-addr end-addr min-idx max-idx start-offset base)
    (once-only (chunk-map)
      `(with-simple-vector-fill (,range-vec (malloc-chunk-map-range-vector ,chunk-map) uint32)
         (dolist (,heap (malloc-chunk-map-extent-list ,chunk-map))
           (destructuring-bind (,start-addr ,end-addr ,min-idx ,max-idx) ,heap
             (declare (type uint32 ,start-addr ,end-addr)
                      (type fixnum ,min-idx ,max-idx))
             (multiple-value-bind (,bytes ,start-offset ,base)
                 (get-bytes-for-addr ,memory ,start-addr (- ,end-addr ,start-addr))
               (declare (ignore ,start-offset))
               (when ,bytes
                 (,@(if int-reader `(with-unsafe-int-read (,int-reader ,bytes)) `(progn))
                    (let ((,base ,base))
                      (declare (type uint32 ,base)
                               (optimize (speed 3)))
                      (setf ,base ,base)
                      (loop for ,index fixnum from ,min-idx below ,max-idx
                         do (let ((,min-addr (aref ,range-vec ,index)))
                              (unless (logtest ,min-addr 1)
                                (let ((,min-addr (sb-ext:truly-the uint32 (+ ,min-addr 4)))
                                      (,max-addr (logand (aref ,range-vec (1+ ,index)) -2)))
                                  (declare (type uint32 ,min-addr ,max-addr))
                                  (assert (<= ,base ,min-addr ,max-addr ,end-addr))
                                  (let ((,offset (sb-ext:truly-the uint32 (- ,min-addr ,base)))
                                        (,limit (sb-ext:truly-the uint32 (- ,max-addr ,base))))
                                    (declare (type (integer 0 #.(- most-positive-fixnum 8))
                                                   ,offset ,limit))
                                    ,@code)))))))))))))))

(defun enum-malloc-chunk-pointers (memory chunk-map callback)
  (declare (type function callback))
  (with-malloc-chunk-lookup (lookup chunk-map)
    (do-malloc-chunks (bytes offset limit min-addr)
        (memory chunk-map :int-reader get-int :index cur-idx)
      (loop for pos fixnum from offset below limit by 4
         do (let ((addr (get-int pos 4)))
              (unless (logtest addr 3)
                (multiple-value-bind (idx tgt-offset) (lookup addr)
                  (when idx
                    (funcall callback cur-idx (- pos offset) idx tgt-offset)))))))))

(defun enum-malloc-area-pointers (memory chunk-map start-addr length callback)
  (declare (type function callback))
  (multiple-value-bind (bytes offset)
      (get-bytes-for-addr memory start-addr length)
    (when bytes
      (with-malloc-chunk-lookup (lookup chunk-map)
        (with-unsafe-int-read (get-int bytes)
          (let ((limit (+ offset length -3)))
            (declare (type (integer 0 #.(- most-positive-fixnum 8)) offset limit)
                     (optimize (speed 3)))
            (loop for pos fixnum from offset below limit by 4
               do (let ((addr (get-int pos 4)))
                    (unless (logtest addr 3)
                      (multiple-value-bind (idx tgt-offset) (lookup addr)
                        (when idx
                          (funcall callback (- pos offset) idx tgt-offset))))))))))))

(defun push-reference (reftbl idx refobj)
  (declare (optimize (speed 3)))
  (check-type refobj atom)
  (let ((cv (svref reftbl idx)))
    (if (consp cv)
        (unless (eq (car cv) refobj)
          (setf (svref reftbl idx) (cons refobj cv)))
        (unless (eq cv refobj)
          (setf (svref reftbl idx)
                (if (null cv) refobj (list refobj cv)))))))

(defun get-references (reftbl idx)
  (ensure-list (svref reftbl idx)))

(defstruct static-chunk-ref
  addr section region)

(defun enum-malloc-section-pointers (mirror chunk-map section reftbl)
  (let* ((last-target nil)
         (last-cutoff 0)
         (image (loaded-image-of section))
         (last-rgn (find-region-by-address image (start-address-of section)))
         (next-rgn (find-region-by-address image (start-address-of section) :next? t))
         (next-rgn-offset 0)
         (extents (memory-extents-for-range
                   mirror (start-address-of section) (length-of section))))
    (labels ((init-rgn (base addr)
               (setf last-rgn (find-region-by-address image addr)
                     next-rgn (find-region-by-address image addr :next? t)
                     next-rgn-offset
                     (if next-rgn (- (start-address-of next-rgn) base) most-positive-fixnum)))
             (index (base s-off dst d-off)
               (unless (or (not (or (= d-off 0) (= d-off #xC)))
                           (and (eql dst last-target)
                                (<= s-off last-cutoff)))
                 (setf last-target dst
                       last-cutoff (min next-rgn-offset (+ s-off 32)))
                 (when (>= s-off next-rgn-offset)
                   (init-rgn base (+ base s-off)))
                 (push-reference reftbl dst
                                 (make-static-chunk-ref :addr (+ base s-off) :section section
                                                        :region last-rgn)))))
      (loop
         for (ext emin emax)
         in (stable-sort extents #'< :key #'second)
         do (setf last-target nil last-cutoff 0)
         do (init-rgn emin emin)
         do (enum-malloc-area-pointers ext chunk-map emin (- emax emin)
                                       (lambda (s-off dst d-off)
                                         (index emin s-off dst d-off)))))))

(defun collect-chunk-references (mirror chunk-map)
  (let ((reftbl (make-array (malloc-chunk-count chunk-map) :initial-element nil)))
    (flet ((push-if-ok (src dst d-off)
             (case d-off
               ((#xC #x0) (push-reference reftbl dst src)))))
      (declare (inline push-if-ok))
      (enum-malloc-chunk-pointers mirror chunk-map
                                  (lambda (src s-off dst d-off)
                                    (declare (ignore s-off) (type fixnum d-off))
                                    (push-if-ok src dst d-off))))
    (dolist (section (sections-of (executable-of mirror)))
      (when (writable? (origin-of section))
        (enum-malloc-section-pointers mirror chunk-map section reftbl)))
    reftbl))

(defun list-chunk-refs-for-area (mirror chunk-map start length target)
  (let ((refs nil))
    (enum-malloc-area-pointers mirror chunk-map start length
                               (lambda (s-off dst d-off)
                                 (when (and (eql dst target)
                                            (member d-off '(0 #xC)))
                                   (push s-off refs))))
    (nreverse refs)))

(defgeneric decode-chunk-reference (memory context ref target)
  (:method (memory (context malloc-chunk-map) (ref cons) target)
    (let ((min (car ref))
          (size (cdr ref)))
      (values min
              (make-instance 'padding :size size)
              (list-chunk-refs-for-area memory context min size target))))
  (:method (memory (context malloc-chunk-map) (ref fixnum) target)
    (multiple-value-bind (min max)
        (malloc-chunk-range context ref)
      (decode-chunk-reference memory context (cons min (- max min)) target)))
  (:method (memory (context malloc-chunk-map) (ref static-chunk-ref) target)
    (let ((rgn (static-chunk-ref-region ref)))
      (if (and rgn
               (< -1 (- (static-chunk-ref-addr ref) (start-address-of rgn)) (length-of rgn)))
          (decode-chunk-reference memory context (cons (start-address-of rgn) (length-of rgn)) target)
          (values (static-chunk-ref-addr ref)
                  (make-instance 'compound :fields
                                 (list (make-instance 'pointer)))
                  (list 0))))))

(defun collect-known-objects (memory chunk-map)
  (with-recursive-lock-held ((lock-of memory))
    (let ((reftbl (make-array (malloc-chunk-count chunk-map) :initial-element nil))
          #+nil(null-extent (null-extent-of memory))
          (queue-head nil) (queue-tail nil)
          (hash (make-hash-table))
          (memory-cb (%get-bytes-for-addr/fast-cb memory)))
      (declare (optimize (speed 3)))
      (with-malloc-chunk-lookup (lookup chunk-map)
        (labels ((queue-ref (addr tag &aux ref)
                   (declare (type uint32 addr))
                   (multiple-value-bind (id offset)
                       (lookup addr)
                     (if id
                         (unless (or (/= 0 offset) (aref reftbl id))
                           (setf ref (cons addr tag))
                           (setf (aref reftbl id) tag))
                         (unless (gethash addr hash)
                           (setf ref (cons addr tag))
                           (setf (gethash addr hash) tag)))
                     (when (and ref (cdr ref))
                       (chanl::pushend ref queue-head queue-tail))
                     nil))
                 (walk-ref (item)
                   (call-pointer-walker memory memory-cb (car item) (cdr item) #'queue-ref)))
          (dolist (global *known-globals*)
            (let ((ref ($ memory (car global))))
              (queue-ref (memory-object-ref-address ref)
                         (memory-object-ref-tag ref))))
          (loop while queue-head
             do (walk-ref (pop queue-head)))
          (values reftbl hash))))))
