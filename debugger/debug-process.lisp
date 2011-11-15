;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug)

;; Thread and scheduler

(defvar *debug-process-lock* (make-recursive-lock "DEBUG-PROCESS LOCK"))
(defvar *debug-worker-thread* nil)
(defvar *debug-event-channel* (make-instance 'unbounded-channel))
(defvar *debug-task-scheduler* (make-instance 'debug-task-scheduler :name "DEBUG"))

(defvar *debugged-processes* nil)

(defun debug-worker-thread ()
  "The thread that executes all ptrace requests for the debugger."
  (catch 'quit-worker-thread
    (loop
       (let ((message (recv *debug-event-channel*)))
         (with-recursive-lock-held (*debug-process-lock*)
           (with-simple-restart (abort "Abort executing ~S" message)
             (apply (first message) (rest message))))
         (format t "Handled ~S~%" message)))))

(defun %submit-debug-command (command &rest args)
  (when (null *debug-worker-thread*)
    (setf *debug-worker-thread*
          (make-thread #'debug-worker-thread
                       :name "DEBUG-PROCESS WORKER"
                       :initial-bindings `((*standard-output* . ,*standard-output*)
                                           (*error-output* . ,*error-output*)))))
  (send *debug-event-channel* (list* command args)))

(defun run-scheduled-debug-tasks ()
  (run-scheduled-tasks *debug-task-scheduler*)
  (when (has-scheduled-tasks? *debug-task-scheduler*)
    (%submit-debug-command #'run-scheduled-debug-tasks)))

(defun run-new-debug-task (task)
  (initial-schedule-task *debug-task-scheduler* task)
  (run-scheduled-debug-tasks))

(defun spawn-debug-task (command &rest args)
  (let ((task (make-task (list* command args))))
    (%submit-debug-command #'run-new-debug-task task)
    task))

(defun/cc wait-debug-task (task &key (on-abort nil on-abortp))
  (if (in-task-context?)
      (loop while (not (task-finished? task))
         do (wait-on-queue (finish-wait-queue-of task)))
      (without-call/cc
        (with-recursive-lock-held (*debug-process-lock*)
          (loop while (not (task-finished? task))
             do (condition-wait (finish-condition-of task) *debug-process-lock*)))))
  (if (slot-boundp task 'signalled-condition)
      (let ((scond (signalled-condition-of task)))
        (cond (on-abortp (values on-abort scond))
              (scond     (signal scond))
              (t         (error "Task aborted: ~A" (name-of task)))))
      (values-list (return-values-of task))))

(defun/cc call-debug-task (command &rest args)
  (wait-debug-task (apply #'spawn-debug-task command args)))

;; SIGCHLD event dispatch

(defun %register-debug-chld-handler (thread)
  (flet ((thread-chld-handler (pid status code)
           (unless (= pid (thread-id-of thread))
             (format t "PID mismatch: ~A != ~A" pid (thread-id-of thread)))
           (unless (wait-pending? pid)
             (format t "SIGCHLD without pending wait: ~A" pid))
           (awhen (case code
                    (#.CLD_EXITED
                     (make-instance 'debug-thread-state-exited
                                    :thread thread
                                    :return-code status))
                    ((#.CLD_KILLED #.CLD_DUMPED)
                     (make-instance 'debug-thread-state-killed
                                    :thread thread
                                    :signal-id status
                                    :dumped? (= code CLD_DUMPED)))
                    ((#.CLD_TRAPPED #.CLD_STOPPED)
                     (case (logand status #xFF)
                       (#.SIGTRAP
                        (make-instance 'debug-thread-state-trapped
                                       :thread thread
                                       :ptrace-event
                                       (foreign-enum-keyword
                                        'ptrace-event (ash status -8) :errorp nil)))
                       (#.SIGSTOP
                        (make-instance 'debug-thread-state-paused :thread thread))
                       (otherwise
                        (make-instance 'debug-thread-state-signalled
                                       :thread thread :signal-id status)))))
             (when (typep it 'debug-thread-state-dead)
               (set-sigchld-handler pid nil))
             (with-recursive-lock-held (*debug-process-lock*)
               (setf (slot-value thread 'pending-state) it)
               (%submit-debug-command 'process-thread-state-change thread it)))))
    (set-sigchld-handler (thread-id-of thread) #'thread-chld-handler)))

(defun %add-debug-thread (process thread-id &key initial-state)
  (assert (not (member thread-id (threads-of process) :key #'thread-id-of)))
  (let* ((thread (make-instance 'debug-thread
                                :process process :thread-id thread-id))
         (state (make-instance (or initial-state 'debug-thread-state-stopped)
                               :thread thread)))
    (setf (slot-value thread 'confirmed-state) state
          (slot-value thread 'pending-state) state)
    (%register-debug-chld-handler thread)
    (push thread (slot-value process 'threads))
    thread))

(defun %set-thread-state (thread state)
  (with-slots (confirmed-state pending-state) thread
    (let ((new-state (if (symbolp state)
                         (make-instance state :thread thread)
                         state)))
      (setf confirmed-state new-state)
      (when (typep new-state 'debug-thread-state-inactive)
        (removef (slot-value (process-of thread) 'threads) thread)))))

(defun %resume-thread (thread &key deliver-signal)
  (with-slots (confirmed-state) thread
    (assert (typep confirmed-state 'debug-thread-state-stopped))
    (when (ptrace-continue (thread-id-of thread)
                           (cond ((integerp deliver-signal) deliver-signal)
                                 ((and deliver-signal
                                       (typep confirmed-state 'debug-thread-state-signalled))
                                  (signal-id-of confirmed-state))
                                 (t 0)))
      (%set-thread-state thread 'debug-thread-state-running))))

(defun %suspend-thread (thread)
  (with-slots (confirmed-state) thread
    (assert (typep confirmed-state 'debug-thread-state-running))
    (unless (typep confirmed-state 'debug-thread-state-stopping)
      (kill-thread (process-id-of (process-of thread))
                   (thread-id-of thread)
                   SIGSTOP)
      (%set-thread-state thread 'debug-thread-state-stopping)
      t)))

(defun %set-thread-options (thread)
  (ptrace-set-options (thread-id-of thread)
                      :PTRACE_O_TRACEFORK
                      :PTRACE_O_TRACEVFORK
                      :PTRACE_O_TRACECLONE
                      :PTRACE_O_TRACEEXEC))

(defun process-thread-state-change (thread state)
  (let ((process (process-of thread))
        (cur-state (confirmed-state-of thread))
        (new-state (pending-state-of thread)))
    (when (eq state new-state)
      (typecase new-state
        ;; Eat spurious stops
        (debug-thread-state-paused
         (unless (typep cur-state 'debug-thread-state-stopping)
           (%resume-thread thread)
           (return-from process-thread-state-change)))
        ;; Handle forks
        (debug-thread-state-trapped
         (case (ptrace-event-of new-state)
           ((:PTRACE_EVENT_FORK :PTRACE_EVENT_VFORK :PTRACE_EVENT_CLONE)
            (let* ((new-pid (ptrace-get-event-msg (thread-id-of thread)))
                   (tgid (process-id-of process))
                   (thread? (member new-pid (process-thread-ids tgid)))
                   (thread-process (if thread?
                                       process
                                       (make-instance 'debug-process :process-id new-pid)))
                   (thread (%add-debug-thread thread-process new-pid)))
              (%set-thread-options thread)
              (change-class new-state 'debug-thread-state-forked
                            :new-child-pid new-pid
                            :thread thread)))
           (:PTRACE_EVENT_EXEC
            (change-class new-state 'debug-thread-state-about-to-exec)))))
      ;; Fetch registers
      (when (typep new-state 'debug-thread-state-stopped)
        (let ((regs (ptrace-get-registers (thread-id-of thread))))
          (setf (slot-value new-state 'register-values) regs
                (slot-value thread 'register-values) regs)))
      ;; Deliver the state change
      (%set-thread-state thread new-state)
      (setf (slot-value process 'last-changed-thread) thread)
      ;; Wake up tasks
      (wake-up-tasks (event-condition-of process))
      (wake-up-tasks (event-condition-of thread))
      (run-scheduled-debug-tasks))))
