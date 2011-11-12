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
                                       (make-instance 'debug-process :process-id new-pid))))
              (change-class new-state 'debug-thread-state-forked
                            :new-child-pid new-pid
                            :thread (%add-debug-thread thread-process new-pid))))
           (:PTRACE_EVENT_EXEC
            (change-class new-state 'debug-thread-state-about-to-exec)))))
      ;; Deliver the state change
      (%set-thread-state thread new-state)
      (setf (slot-value process 'last-changed-thread) thread)
      ;; Wake up tasks
      (wake-up-tasks (event-condition-of process))
      (wake-up-tasks (event-condition-of thread))
      (run-scheduled-debug-tasks))))

;; Event waiting

(def-debug-task read-recent-state (process-or-thread)
  (with-global-control (process-or-thread :exclusive? nil)
    (most-recent-state-of process-or-thread)))

(def-debug-task wait-for-state (process-or-thread
                                &key
                                (type t) (not-type nil)
                                (filter (constantly t))
                                (new? (typep process-or-thread 'debug-process)))
  (flet ((match-state (state)
           (print state)
           (and (typep state type) (not (typep state not-type))
                (funcall filter state))))
    (let ((cur-state (read-recent-state process-or-thread)))
      (loop for state = (read-recent-state process-or-thread)
         do (progn
              (when (and (match-state state)
                         (not (and new? (eq cur-state state))))
                (return-from wait-for-state state))
              (wait-on-queue (event-condition-of process-or-thread)))))))

(def-debug-task suspend-threads (process threads)
  (let ((we-stopped nil))
    (loop for thread-list = (if (eq threads :all)
                                (threads-of process)
                                threads)
       while (some #'is-running? thread-list)
       do (loop
             for thread in thread-list
             for state = (most-recent-state-of thread)
             do (when (and (typep state 'debug-thread-state-running)
                           (%suspend-thread thread))
                  (pushnew thread we-stopped)))
       do (wait-for-state process))
    we-stopped))

(def-debug-task resume-threads (thread-list)
  (loop
     for thread in thread-list
     for state = (most-recent-state-of thread)
     do (when (typep state 'debug-thread-state-paused)
          (%resume-thread thread))))

(defmacro with-threads-suspended ((process threads) &body code)
  (with-unique-names (we-suspended)
    (once-only (process threads)
      `(with-global-control (,process :exclusive? (eq ,threads :all))
         (let ((,we-suspended (suspend-threads ,process ,threads)))
           (with-exit-unwind
               (progn ,@code)
             (resume-threads ,we-suspended)))))))

(def-debug-task suspend-thread (thread &key (wait? t))
  (let* ((state (read-recent-state thread))
         (we-suspend? (and (typep state 'debug-thread-state-running)
                           (%suspend-thread thread))))
    (when wait?
      (wait-for-state thread :not-type 'debug-thread-state-running))
    we-suspend?))

(def-debug-task resume-thread (thread &key deliver-signal)
  (when (typep (confirmed-state-of thread) 'debug-thread-state-stopped)
    (%resume-thread thread :deliver-signal deliver-signal)))

(defun %restore-trap-state (thread state)
  (when (and (typep (confirmed-state-of thread) 'debug-thread-state-trapped)
             (typep state 'debug-thread-state-trapped))
    (setf (slot-value thread 'confirmed-state) state)))

(defmacro with-thread-suspended ((thread &key exclusive? save-trap-state?) &body code)
  (with-unique-names (we-suspended init-state)
    (once-only (thread)
      `(with-thread-control (,thread :exclusive? ,exclusive?)
         (let ((,we-suspended (suspend-thread ,thread))
               ,@(if save-trap-state? `((,init-state (confirmed-state-of ,thread)))))
           (with-exit-unwind
               (progn ,@code)
             (if ,we-suspended
                 (resume-thread thread)
                 ,(if save-trap-state? `(%restore-trap-state ,thread ,init-state)))))))))

(def-debug-task terminate-debug (process)
  (with-threads-suspended (process :all)
    (dolist (thread (threads-of process))
      (without-call/cc (ptrace-detach (thread-id-of thread) SIGCONT))
      (set-sigchld-handler (thread-id-of thread) nil)
      (%set-thread-state thread 'debug-thread-state-detached))
    (removef *debugged-processes* process)))

(def-debug-task initialize-debug (process)
  (with-global-control (process :exclusive? t)
    (flet ((start-attach-to-thread (id)
             (let* ((thread (%add-debug-thread process id
                                               :initial-state 'debug-thread-state-running)))
               (if (ptrace-attach id)
                   (%set-thread-state thread 'debug-thread-state-stopping)
                   (%set-thread-state thread 'debug-thread-state-detached))
               thread)))
      (let* ((pid (process-id-of process))
             (main-thread (start-attach-to-thread pid)))
        (unless main-thread
          (abort-task "Could not attach to main thread: ~A" pid))
        (setf (slot-value process 'main-thread) main-thread
              (slot-value process 'last-changed-thread) main-thread)
        (push process *debugged-processes*)
        (loop while (is-running? process)
           do (progn
                (wait-for-state process)
                (let ((ids (process-thread-ids pid))
                      (known (mapcar #'thread-id-of (threads-of process))))
                  (dolist (id (set-difference ids known))
                    (start-attach-to-thread id)))))
        (setf (main-mapping-of process)
              (select-main-executable (process-memory-maps pid))))
      (values process (length (threads-of process))))))

(defun/cc start-debug (process-id)
  (let ((process (make-instance 'debug-process :process-id process-id)))
    (call-debug-task 'initialize-debug process)))

(defun/cc stop-debug (process)
  (call-debug-task 'terminate-debug process))

