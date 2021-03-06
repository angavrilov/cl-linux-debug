;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.data-info)

(defconstant +num-extents+ (/ (ash 1 32) +max-glibc-heap-size+))

(defstruct malloc-chunk-map
  "A table of malloc chunks as a sorted address vector for binsearch.
Fields:
  range-vector - ordered address vector; value = (start-4)|(1 if unused)
  extent-list - areas as (start-addr end-addr start-idx end-idx)
  extent-tbl - table of (start-idx end-idx) by addr/+max-glibc-heap-size+
  extent-handler-tbl - table of lists of extra info handlers"
  (range-vector (make-binsearch-uint32-vec 1048576))
  (extent-list nil)
  (extent-tbl (make-array +num-extents+ :initial-element nil))
  (extent-handler-tbl (make-array +num-extents+ :initial-element nil)))

(defun malloc-chunk-count (chunk-map)
  (length (malloc-chunk-map-range-vector chunk-map)))

(defun collect-malloc-objects (memory &optional (chunk-map (make-malloc-chunk-map)))
  (let ((vector (malloc-chunk-map-range-vector chunk-map))
        (ranges nil)
        (heaps (append (find-glibc-heap-ranges memory)
                       (find-wine-heap-ranges memory)))
        (last-pos 1)
        (etbl (malloc-chunk-map-extent-tbl chunk-map))
        (htbl (malloc-chunk-map-extent-handler-tbl chunk-map)))
    ;; Wipe the range vector, and push the left guard
    (setf (fill-pointer vector) 0)
    (vector-push-extend 1 vector)
    (fill etbl nil)
    (fill htbl nil)
    ;; Enumerate chunks
    (dolist (range (sort heaps #'< :key #'first))
      (multiple-value-bind (start-addr end-addr)
          (funcall (third range) memory vector (first range) (second range))
        ;; If added, register
        (let ((cur-pos (fill-pointer vector))
              (handler (fourth range)))
          (when (> cur-pos last-pos)
            (push (list start-addr end-addr last-pos cur-pos) ranges)
            (loop for i from (floor start-addr +max-glibc-heap-size+)
               to (floor end-addr +max-glibc-heap-size+)
               do (aif (svref etbl i)
                       (setf (cdr it) cur-pos)
                       (setf (svref etbl i) (cons last-pos cur-pos)))
               do (when handler
                    (pushnew handler (svref htbl i))))
            (setf last-pos cur-pos)))))
    ;; Finish
    (setf (malloc-chunk-map-extent-list chunk-map) (nreverse ranges))
    chunk-map))

(defmacro with-malloc-chunk-lookup ((name chunk-map &key (range-vec (gensym))) &body code)
  "Defines lookup function for search by address in CHUNK-MAP.
RANGE-VEC may be used to access the address vector as simple-array.
Function:
  (NAME addr) returns: chunk id or NIL, offset within chunk"
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
                      (awhen (svref ,etbl (floor addr +max-glibc-heap-size+))
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

(defun malloc-chunk-range (memory chunk-map id &optional address)
  "Retrieves the address range of malloc chunk ID from the map.
Returns: min-addr, max-addr, address is in range?"
  (when id
    (let ((vec (malloc-chunk-map-range-vector chunk-map)))
      (let* ((start (aref vec id))
             (min (+ start 4))
             (max (logand (aref vec (1+ id)) -2))
             (handlers (malloc-chunk-map-extent-handler-tbl chunk-map)))
        (loop for handler in (svref handlers (floor start +max-glibc-heap-size+))
           do (awhen (funcall handler memory start max)
                (setf max it)
                (return)))
        (values min max (if address (<= min address max)))))))

(defun lookup-malloc-object (memory chunk-map address)
  "Looks up address in malloc chunk map.
Returns: chunk id, min addr, max addr, address is within range?"
  (with-malloc-chunk-lookup (lookup chunk-map)
    (awhen (lookup address)
      (multiple-value-bind (min max ok?)
          (malloc-chunk-range memory chunk-map it address)
        (values it min max ok?)))))

(defmacro do-malloc-chunks ((bytes offset limit &optional (min-addr (gensym)) (max-addr (gensym)))
                            (memory chunk-map &key int-reader (index (gensym))) &body code)
  (with-unique-names (range-vec heap start-addr end-addr min-idx max-idx start-offset base)
    (once-only (chunk-map)
      `(with-vector-array (,range-vec (malloc-chunk-map-range-vector ,chunk-map) uint32)
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
         do (locally (declare (optimize (safety 0)))
              ;; If cl-simd is available, try to control cache pollution
              ;; by using a non-temporal prefetch instruction.
              #+sse2
              (sse:aref-prefetch-nta bytes (the fixnum (+ pos 64))))
         do (let* ((addr (get-int pos 4)))
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
               (unless (or (not (or (= d-off 0) (= d-off 4) (= d-off #xC)))
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
               ((#xC #x4 #x0) (push-reference reftbl dst src)))))
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
      (or (mapcar (lambda (x) (+ min x))
                  (list-chunk-refs-for-area memory context min size target))
          (list min))))
  (:method (memory (context malloc-chunk-map) (ref fixnum) target)
    (multiple-value-bind (min max)
        (malloc-chunk-range memory context ref)
      (decode-chunk-reference memory context (cons min (- max min)) target)))
  (:method (memory (context malloc-chunk-map) (ref static-chunk-ref) target)
    (let ((rgn (static-chunk-ref-region ref)))
      (if (and rgn
               (< -1 (- (static-chunk-ref-addr ref) (start-address-of rgn)) (length-of rgn)))
          (decode-chunk-reference memory context (cons (start-address-of rgn) (length-of rgn)) target)
          (list (static-chunk-ref-addr ref))))))

(defstruct array-queue
  (read-index 0 :type fixnum)
  (insert-index 0 :type fixnum)
  (pages (make-array 32 :fill-pointer 0 :adjustable t) :type (vector (or null simple-vector)))
  (page-cache nil))

(defun array-queue-empty? (queue)
  (>= (array-queue-read-index queue) (array-queue-insert-index queue)))

(defun array-queue-push (item queue)
  (declare (optimize (speed 1)))
  (let* ((idx (array-queue-insert-index queue))
         (page-idx (floor idx 1024))
         (item-idx (mod idx 1024))
         (pages (array-queue-pages queue)))
    (when (= item-idx 0)
      (vector-push-extend (or (pop (array-queue-page-cache queue))
                              (make-array 1024))
                          pages))
    (setf (svref (aref pages page-idx) item-idx) item)
    (setf (array-queue-insert-index queue) (the fixnum (1+ idx)))))

(defun array-queue-pop (queue)
  (declare (optimize (speed 1)))
  (let* ((idx (array-queue-read-index queue))
         (page-idx (floor idx 1024))
         (item-idx (mod idx 1024))
         (page (aref (array-queue-pages queue) page-idx)))
    (assert (< idx (array-queue-insert-index queue)))
    (prog1 (svref page item-idx)
      (setf (array-queue-read-index queue) (1+ idx))
      (when (= item-idx 1023)
        (push page (array-queue-page-cache queue))
        (setf (aref (array-queue-pages queue) page-idx) nil)))))

(defun collect-known-objects (memory chunk-map)
  (with-recursive-lock-held ((lock-of memory))
    (let ((reftbl (make-array (malloc-chunk-count chunk-map) :initial-element nil))
          #+nil(null-extent (null-extent-of memory))
          (addr-queue (make-array-queue))
          (tag-queue (make-array-queue))
          (hash (make-hash-table))
          (nonheap? t)
          (memory-cb (%get-bytes-for-addr/fast-cb memory)))
      (declare (optimize (speed 3)))
      (with-malloc-chunk-lookup (lookup chunk-map)
        (labels ((adjust-class (tag addr)
                   (aif (and (typep (car tag) 'class-type)
                             (%fast-adjust-class memory memory-cb addr))
                        (values (effective-tag-of it))
                        tag))
                 (queue-ref (addr tag &aux real-tag)
                   (declare (type uint32 addr))
                   (multiple-value-bind (id offset)
                       (lookup addr)
                     (if id
                         (unless (or (/= 0 offset) (aref reftbl id))
                           (setf real-tag (adjust-class tag addr))
                           (setf (aref reftbl id) real-tag))
                         (unless (or (not nonheap?) (gethash addr hash))
                           (setf real-tag (adjust-class tag addr))
                           (setf (gethash addr hash) real-tag)))
                     (when real-tag
                       (array-queue-push addr addr-queue)
                       (array-queue-push real-tag tag-queue))
                     nil))
                 (walk-ref (addr tag)
                   (call-pointer-walker memory memory-cb addr tag #'queue-ref)))
          (dolist (ref (@ memory '*))
            (queue-ref (memory-object-ref-address ref)
                       (memory-object-ref-tag ref)))
          (setf nonheap? nil)
          (loop while (not (array-queue-empty? addr-queue))
             do (walk-ref (array-queue-pop addr-queue) (array-queue-pop tag-queue)) )
          (values reftbl hash))))))
