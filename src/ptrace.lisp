;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug)

;; Errors

(defcvar "errno" :int)

(defcfun "strerror" :string
  (errno :int))

(defun check-errno (function rv)
  (let ((errno *errno*))
    (if (= rv -1)
        (cerror "ignore" "~A failed: ~A (~A)"
                function (strerror errno) errno)
        rv)))

(defmacro with-errno (call)
  `(check-errno ',(first call) ,call))

;; Signals

(defcfun "waitpid" pid_t
  (pid pid_t)
  (status :pointer)
  (options :int))

(defun wait-pending? (process)
  (= (waitpid process (null-pointer) WNOHANG) process))

(defun kill-thread (process thread signal)
  (with-errno (foreign-funcall "syscall" :int SYS_tgkill :int process :int thread :int signal)))

(defvar *sigchld-lock* (bordeaux-threads:make-lock "SIGCHLD LOCK"))
(defvar *sigchld-callbacks* (make-hash-table :test #'eql))

(defun sigchld-handler (signal info context)
  (with-foreign-slots ((si_pid si_status si_code) info siginfo_t)
    (awhen (with-lock-held (*sigchld-lock*)
             (gethash si_pid *sigchld-callbacks*))
      (funcall it si_pid si_status si_code)))
  (sb-unix::sigchld-handler signal info context))

(sb-unix::enable-interrupt sb-unix::sigchld #'sigchld-handler)

(defun set-sigchld-handler (process-id callback &key allow-overwrite?)
  (with-lock-held (*sigchld-lock*)
    (if callback
        (progn
          (when (and (not allow-overwrite?)
                     (gethash process-id *sigchld-callbacks*))
            (cerror "Overwrite the old hook."
                    "SIGCHLD hook for process ~A already exists." process-id))
          (setf (gethash process-id *sigchld-callbacks*) callback))
        (remhash process-id *sigchld-callbacks*))))

;; Ptrace

(defcfun "ptrace" :long
    (request ptrace-request)
    (process pid_t)
    (addr :pointer)
    (data :pointer))

(defun ptrace-attach (process)
  (with-errno (ptrace :PTRACE_ATTACH process (null-pointer) (null-pointer))))

(defun ptrace-detach (process &optional (signal 0))
  (with-errno (ptrace :PTRACE_DETACH process (null-pointer) (make-pointer signal))))

(defun ptrace-continue (process &optional (signal 0))
  (with-errno (ptrace :PTRACE_CONT process (null-pointer) (make-pointer signal))))

(defun ptrace-kill (process)
  (with-errno (ptrace :PTRACE_KILL process (null-pointer) (null-pointer))))

(defun ptrace-get-event-msg (process)
  (with-foreign-object (data :unsigned-long)
    (with-errno (ptrace :PTRACE_GETEVENTMSG process (null-pointer) data))
    (mem-ref data :unsigned-long)))

(defun ptrace-get-registers (process)
  (with-foreign-object (reginfo 'user_regs_struct)
    (with-errno (ptrace :PTRACE_GETREGS process (null-pointer) reginfo))
    (macrolet ((convert (&rest names)
                 `(list ,@(loop for i in names
                             collect (make-keyword i)
                             collect `(foreign-slot-value reginfo 'user_regs_struct ',i)))))
      (convert eax ebx ecx edx esi edi esp ebp eip eflags orig-eax))))

(defun ptrace-set-registers (process &rest registers)
  (when registers
    (with-foreign-object (reginfo 'user_regs_struct)
      (with-errno (ptrace :PTRACE_GETREGS process (null-pointer) reginfo))
      (macrolet ((convert (&rest names)
                   `(destructuring-bind (&key ,@names) registers
                      ,@(loop for i in names
                           collect `(when ,i
                                      (setf (foreign-slot-value reginfo 'user_regs_struct ',i) ,i))))))
        (convert eax ebx ecx edx esi edi esp ebp eip eflags orig-eax)
        (with-errno (ptrace :PTRACE_SETREGS process (null-pointer) reginfo))))))

(defun ptrace-copy-bytes (process address array &key
                          (start 0) (end (array-total-size array)) (write? nil))
  (declare (type (array (unsigned-byte 8)) array)
           (type (unsigned-byte 32) address))
  (assert (<= 0 start end (array-total-size array)))
  (with-foreign-object (word :long)
    (let* ((wsize (foreign-type-size :long))
           (in-first (mod address wsize))
           (abase (- address in-first)))
      (do ((base abase (+ base wsize))
           (idx (- start in-first) (+ idx wsize)))
          ((>= idx end))
        (when (or (not write?) (< idx 0) (> (+ idx wsize) end))
          (setf (mem-ref word :long)
                (with-errno (ptrace :PTRACE_PEEKDATA process (make-pointer base) (null-pointer)))))
        (loop for i from 0 below wsize and j from idx when (<= start j (1- end))
           do (if write?
                  (setf (mem-ref word :unsigned-char i)
                        (row-major-aref array j))
                  (setf (row-major-aref array j)
                        (mem-ref word :unsigned-char i))))
        (when write?
          (with-errno (ptrace :PTRACE_POKEDATA process (make-pointer base)
                              (mem-ref word :pointer)))))
      array)))

