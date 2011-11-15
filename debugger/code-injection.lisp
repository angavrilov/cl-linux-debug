;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug)

(defparameter *syscall-bytes*
  (make-array 3 :element-type 'uint8 :initial-contents '(#xCD #x80 #xCC)))

(defparameter *area-alloc-size* (/ 1048576 2))

(def-debug-task %exec-injected-syscall (thread address id &rest args)
  (apply #'%set-registers thread
         :eip address :orig-eax -1 :eax id
         (mapcan #'list '(:ebx :ecx :edx :esi :edi :ebp) args))
  (%resume-thread thread)
  (let ((state (wait-for-state thread :not-type 'debug-thread-state-running))
        (regs (ptrace-get-registers (thread-id-of thread))))
    (unless (and (typep state 'debug-thread-state-trapped)
                 (= (getf regs :eip) (+ address 3)))
      (abort-task "Could not execute injected syscall, state ~S, regs ~S" state regs))
    (getf regs :eax)))

(def-debug-task inject-syscall (thread id &rest args)
  (let ((addr (%get-injected-syscall-address thread)))
    (with-thread-excursion (thread)
      (apply #'%exec-injected-syscall thread addr id args))))

(def-debug-task inject-mmap2 (thread start size prot flags fd offset)
  (let ((rv (inject-syscall thread 192 start size prot flags fd offset)))
    (if (< -4096 rv 0)
        (values nil (- rv))
        (unsigned rv))))

(def-debug-task alloc-injection-area (thread executable? &key (size *area-alloc-size*))
  (multiple-value-bind (addr errno)
      (inject-mmap2 thread
                    0 size
                    (if executable?
                        (logior PROT_READ PROT_EXEC)
                        (logior PROT_READ PROT_WRITE))
                    (logior MAP_PRIVATE MAP_ANONYMOUS)
                    -1 0)
    (unless addr
      (abort-task "Could not allocate an injection area: ~A" (strerror errno)))
    (push (make-memory-mapping :start-addr addr :end-addr (+ addr size)
                               :readable? t :writable? (not executable?)
                               :executable? executable?)
          (injected-mappings-of (injection-info-of (process-of thread))))
    (values addr size)))

(def-debug-task %init-code-injection (process iinfo)
  (with-threads-suspended (process :all)
    (let* ((start-ptr (or (entry-address-of (main-image-of process))
                          (abort-task "The process has no main text section.")))
           (bytes (length *syscall-bytes*))
           (save-buf (make-array bytes :element-type 'uint8)))
      ;; Inject the syscall at the image start address
      (setf (slot-value iinfo 'syscall-inject-ptr) start-ptr)
      (ptrace-copy-bytes (process-id-of process) start-ptr save-buf)
      (with-exit-unwind
          (progn
            (ptrace-copy-bytes (process-id-of process) start-ptr *syscall-bytes* :write? t)
            ;; Allocate a memory area
            (multiple-value-bind (start size)
                (alloc-injection-area (main-thread-of process) t)
              ;; Write the syscall at the start of it, and register
              ;; the rest as free:
              (setf (slot-value iinfo 'syscall-inject-ptr) start)
              (ptrace-copy-bytes (process-id-of process) start *syscall-bytes* :write? t)
              (push (list (+ start bytes) (- size bytes)) (free-code-areas-of iinfo))
              start))
        ;; Restore the image start address data
        (when (= (syscall-inject-ptr-of iinfo) start-ptr)
          (setf (slot-value iinfo 'syscall-inject-ptr) nil))
        (ptrace-copy-bytes (process-id-of process) start-ptr save-buf :write? t)))))

(def-debug-task %get-injected-syscall-address (thread)
  (let* ((process (process-of thread))
         (iinfo (injection-info-of process)))
    (aif (syscall-inject-ptr-of iinfo)
         it
         (%init-code-injection process iinfo))))
