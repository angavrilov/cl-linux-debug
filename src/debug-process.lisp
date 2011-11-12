;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug)

(def (class* e) debug-thread-state ()
  ((thread :reader t)))

(def (class* e) debug-thread-state-running (debug-thread-state)
  ())

(def (class* e) debug-thread-state-dead (debug-thread-state)
  ())

(def (class* e) debug-thread-state-exited (debug-thread-state-dead)
  ((return-code :reader t)))

(def (class* e) debug-thread-state-killed (debug-thread-state-dead)
  ((signal-id :reader t)
   (dumped? :reader t)))

(def (class* e) debug-thread-state-stopped (debug-thread-state)
  ())

(def (class* e) debug-thread-state-paused (debug-thread-state-stopped)
  ())

(def (class* e) debug-thread-state-trapped (debug-thread-state-stopped)
  ((ptrace-event :reader-t)))

(def (class* e) debug-thread-state-forked (debug-thread-state-trapped)
  ((new-child-pid :reader t)
   (thread :reader t)))

(def (class* e) debug-thread-state-about-to-exec (debug-thread-state-trapped)
  ())

(def (class* e) debug-thread-state-signalled (debug-thread-state-stopped)
  ((signal-id :reader t)))

(def (class* e) debug-thread ()
  ((process :reader t)
   (thread-id :reader t)
   (confirmed-state :reader t)
   (pending-state :reader t)
   (stop-pending? :accessor t)))

(def (class* e) debug-process ()
  ((process-id :reader t)
   (lock (make-recursive-lock "DEBUG-PROCESS LOCK") :reader t)
   (worker-thread :reader t)
   (event-channel (make-instance 'unbounded-channel) :reader t)
   (threads nil :reader t)
   (exclusive-task nil :reader t)))

(defgeneric is-running? (obj)
  (:method ((obj debug-thread))
    (typep (confirmed-state-of obj) 'debug-thread-state-running))
  (:method ((obj debug-process))
    (some #'is-running? (threads-of obj))))

(defparameter *debugged-process* nil)
(defparameter *in-debug-process-lock* nil)

(defun in-debugger-thread? (process)
  (eq process *debugged-process*))

(defmacro with-debug-process-lock ((process) &body code)
  `(with-recursive-lock-held ((lock-of ,process))
     (let ((*in-debug-process-lock* t))
       ,@code)))

(defun debug-worker-thread (process)
  "The thread that executes all ptrace requests for the debugger."
  (let ((*debugged-process* process))
    (catch 'quit-worker-thread
      (loop
         (let ((message (recv (event-channel-of process))))
           (with-debug-process-lock (process)
             (with-simple-restart (abort "Abort executing ~S" message)
               (apply (first message) (rest message))))
           (format t "Handled ~S~%" message))))))

(defun %submit-debug-command (process command &rest args)
  (send (event-channel-of process) (list* command args)))

(def (function e) start-debug (process-id)
  (let ((process (make-instance 'debug-process
                                :process-id process-id)))
    (setf (slot-value process 'worker-thread)
          (make-thread (lambda () (debug-worker-thread process))
                       :name "DEBUG-PROCESS WORKER"
                       :initial-bindings `((*standard-output* . ,*standard-output*)
                                           (*error-output* . ,*error-output*))))
    (%submit-debug-command process 'attach-to-process)
    process))

(defun %register-debug-chld-handler (process thread)
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
             (with-debug-process-lock (process)
               (setf (slot-value thread 'pending-state) it)
               (%submit-debug-command process 'process-thread-state-change thread it)))))
    (set-sigchld-handler (thread-id-of thread) #'thread-chld-handler)))

(defun %add-debug-thread (process thread-id &key initial-state)
  (let* ((thread (make-instance 'debug-thread
                                :process process :thread-id thread-id))
         (state (make-instance (or initial-state 'debug-thread-state-stopped) :thread thread)))
    (setf (slot-value thread 'confirmed-state) state
          (slot-value thread 'pending-state) state)
    (with-debug-process-lock (process)
      (assert (not (member thread-id (threads-of process) :key #'thread-id-of)))
      (%register-debug-chld-handler process thread)
      (push thread (slot-value process 'threads))
      thread)))

(defun %resume-thread (thread &key deliver-signal)
  (with-slots (confirmed-state pending-state) thread
    (assert (and (in-debugger-thread? (process-of thread))
                 (typep confirmed-state 'debug-thread-state-stopped)))
    (when (ptrace-continue (thread-id-of thread)
                           (cond ((integerp deliver-signal) deliver-signal)
                                 ((and deliver-signal
                                       (typep confirmed-state 'debug-thread-state-signalled))
                                  (signal-id-of confirmed-state))
                                 (t 0)))
      (let ((new-state (make-instance 'debug-thread-state-running :thread thread)))
        (setf confirmed-state new-state
              pending-state new-state)))))

(defun %resume-continuation (callback new-state old-state)
  (with-simple-restart (abort "Abort resuming the task.")
    (catch 'cancel-task
      (funcall callback new-state old-state))))

(defun process-thread-state-change (thread state)
  (let ((cur-state (confirmed-state-of thread))
        (new-state (pending-state-of thread)))
    (when (eq state new-state)
      (typecase new-state
        ;; Eat spurious stops
        (debug-thread-state-paused
         (if (stop-pending? thread)
             (setf (stop-pending? thread) nil)
             (progn
               (%resume-thread thread)
               (return-from process-thread-state-change))))
        ;; Handle forks
        (debug-thread-state-trapped
         (case (ptrace-event-of new-state)
           ((:PTRACE_EVENT_FORK :PTRACE_EVENT_VFORK :PTRACE_EVENT_CLONE)
            (let* ((new-pid (ptrace-get-event-msg (thread-id-of thread)))
                   (tgid (process-id-of *debugged-process*))
                   (thread? (member new-pid (process-thread-ids tgid))))
              (change-class new-state 'debug-thread-state-forked
                            :new-child-pid new-pid
                            :thread (when thread?
                                      (%add-debug-thread *debugged-process* new-pid)))))
           (:PTRACE_EVENT_EXEC
            (change-class new-state 'debug-thread-state-about-to-exec)))))
      ;; Deliver the state change
      (setf (slot-value thread 'confirmed-state) new-state)
      (with-slots (exclusive-task) *debugged-process*
        (awhen exclusive-task
          (setf exclusive-task nil)
          (%resume-continuation it new-state cur-state))))))

(defun/cc wait-state-change ()
  (let/cc callback
    (assert (and *debugged-process*
                 (null (exclusive-task-of *debugged-process*))))
    (setf (slot-value *debugged-process* 'exclusive-task) callback)))

(defun/cc attach-to-process ()
  (flet ((start-attach-to-thread (id)
           (let* ((thread (%add-debug-thread *debugged-process* id
                                             :initial-state 'debug-thread-state-running)))
             (when (ptrace-attach id)
               (setf (slot-value thread 'stop-pending?) t)))))
    (start-attach-to-thread (process-id-of *debugged-process*))
    (loop while (is-running? *debugged-process*)
       do (progn
            (wait-state-change)
            (let ((ids (process-thread-ids (process-id-of *debugged-process*)))
                  (known (mapcar #'thread-id-of (threads-of *debugged-process*))))
              (dolist (id (set-difference ids known))
                (start-attach-to-thread id)))))
    (print "attach complete")))

