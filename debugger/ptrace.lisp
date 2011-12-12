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
  (= (waitpid process (null-pointer) (logior WNOHANG __WALL)) process))

(defun wait-for-process (pid)
  (with-foreign-object (status :int)
    (let ((rv (with-errno (waitpid pid status (logior WNOHANG __WALL)))))
      (when (> rv 0)
        (let* ((sv (mem-ref status :int))
               (sig (logand sv #x7F))
               (core? (logtest sv #x80))
               (code (logand (ash sv -8) #xFF)))
          (cond ((= sv #xFFFF)
                 (values rv 0 CLD_CONTINUED))
                ((= sig 0)
                 (values rv code CLD_EXITED))
                ((= sig #x7F)
                 (values rv code (if (= code SIGTRAP) CLD_TRAPPED CLD_STOPPED)))
                (t
                 (values rv sig (if core? CLD_DUMPED CLD_KILLED)))))))))

(defcfun "syscall" :int
  (id :int)
  (process :int)
  (thread :int)
  (signal :int))

(defun kill-thread (process thread signal)
  (with-errno (syscall SYS_tgkill process thread signal)))

(defvar *sigchld-lock* (bordeaux-threads:make-recursive-lock "SIGCHLD LOCK"))
(defvar *sigchld-callbacks* (make-hash-table :test #'eql))
(defvar *sigchld-notify* (bt:make-condition-variable :name "SIGCHLD"))
(defvar *sigchld-check-thread* nil)

(defun sigchld-check-thread ()
  (let ((changed? nil))
    (loop
       (loop for (cb pid status code) in
            (bt:with-recursive-lock-held (*sigchld-lock*)
              (if changed?
                  (setf changed? nil)
                  (bt:condition-wait *sigchld-notify* *sigchld-lock*))
              (loop for try-pid being the hash-keys of *sigchld-callbacks*
                 when (multiple-value-bind (pid status code)
                          (wait-for-process try-pid)
                        (when pid
                          (list (gethash pid *sigchld-callbacks*)
                                pid status code)))
                 collect it))
          do (when cb
               (setf changed? t)
               (funcall cb pid status code))))))

(defun sigchld-handler (signal info context)
  (bt:condition-notify *sigchld-notify*)
  (sb-unix::sigchld-handler signal info context))

(sb-unix::enable-interrupt sb-unix::sigchld #'sigchld-handler)

(defun set-sigchld-handler (process-id callback &key allow-overwrite?)
  (with-recursive-lock-held (*sigchld-lock*)
    (unless (aand *sigchld-check-thread* (bt:thread-alive-p it))
      (setf *sigchld-check-thread*
            (make-thread #'sigchld-check-thread :name "SIGCHLD LOOP")))
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

(defun ptrace-set-options (process &rest options)
  (let ((iv (reduce #'logior (mapcar (lambda (x) (foreign-enum-value 'ptrace-options x)) options))))
    (with-errno (ptrace :PTRACE_SETOPTIONS process (null-pointer)
                        (make-pointer (unsigned iv))))))

(defun %ptrace-decode-registers (reginfo)
  (macrolet ((convert (&rest names)
               `(list ,@(loop for i in names
                           collect (make-keyword i)
                           collect `(foreign-slot-value reginfo 'user_regs_struct ',i)))))
    (convert eax ebx ecx edx esi edi esp ebp eip eflags orig-eax
             cs ss ds es fs gs)))

(defun ptrace-get-registers (process)
  (with-foreign-object (reginfo 'user_regs_struct)
    (with-errno (ptrace :PTRACE_GETREGS process (null-pointer) reginfo))
    (%ptrace-decode-registers reginfo)))

(defun ptrace-set-registers (process &rest registers)
  (when registers
    (with-foreign-object (reginfo 'user_regs_struct)
      (with-errno (ptrace :PTRACE_GETREGS process (null-pointer) reginfo))
      (macrolet ((convert (&rest names)
                   `(destructuring-bind (&key ,@names) registers
                      ,@(loop for i in names
                           collect `(when ,i
                                      (setf (foreign-slot-value reginfo 'user_regs_struct ',i) ,i))))))
        (convert eax ebx ecx edx esi edi esp ebp eip eflags orig-eax
                 cs ss ds es fs gs)
        (with-errno (ptrace :PTRACE_SETREGS process (null-pointer) reginfo))
        (%ptrace-decode-registers reginfo)))))

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
        (when (or (not write?) (< idx start) (> (+ idx wsize) end))
          (setf (mem-ref word :long)
                (ptrace :PTRACE_PEEKDATA process (make-pointer base) (null-pointer))))
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

