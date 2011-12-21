;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug)

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
           (and (typep state type) (not (typep state not-type))
                (funcall filter state))))
    (let ((cur-state (read-recent-state process-or-thread)))
      (loop for state = (read-recent-state process-or-thread)
         do (progn
              (when (and (match-state state)
                         (not (and new? (eq cur-state state))))
                (return-from wait-for-state state))
              (wait-on-queue (event-condition-of process-or-thread)))))))

;; Suspend all threads

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

;; Suspend one thread

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

(defmacro with-thread-suspended ((thread &key exclusive?) &body code)
  (with-unique-names (we-suspended)
    (once-only (thread)
      `(with-thread-control (,thread :exclusive? ,exclusive?)
         (let ((,we-suspended (suspend-thread ,thread)))
           (with-exit-unwind
               (progn ,@code)
             (when ,we-suspended
               (resume-thread thread))))))))

(defun find-any-thread-to-suspend (process)
  (or (when (typep process 'debug-thread)
        process)
      (dolist (thread (threads-of process))
        (when (typep (confirmed-state-of thread) 'debug-thread-state-stopped)
          (return thread)))
      (main-thread-of process)))

(defmacro with-any-thread-suspended ((process thread-var &key exclusive?) &body code)
  `(let ((,thread-var (find-any-thread-to-suspend ,process)))
     (with-thread-suspended (,thread-var :exclusive? ,exclusive?)
       ,@code)))

(defun %set-registers (thread &rest regs)
  (setf (slot-value thread 'register-values)
        (apply #'ptrace-set-registers (thread-id-of thread) regs)))

(defun %restore-thread-state (thread we-suspended? old-state)
  (let ((cur-state (confirmed-state-of thread)))
    (assert (not (typep cur-state 'debug-thread-state-running)))
    (when (typep cur-state 'debug-thread-state-stopped)
      (apply #'%set-registers thread (register-values-of old-state))
      (if we-suspended?
          (%resume-thread thread)
          (progn
            (setf (slot-value thread 'confirmed-state) old-state)
            (when (typep old-state 'debug-thread-state-paused)
              (setf (unsafe-signal-state? old-state)
                    (or (not (typep cur-state 'debug-thread-state-paused))
                        (unsafe-signal-state? cur-state)))))))))

(defmacro with-thread-excursion ((thread) &body code)
  (with-unique-names (we-suspended start-state)
    (once-only (thread)
      `(with-thread-control (,thread :exclusive? t)
         (let ((,we-suspended (suspend-thread ,thread))
               (,start-state (confirmed-state-of ,thread)))
           (with-exit-unwind
               (progn ,@code)
             (%restore-thread-state ,thread ,we-suspended ,start-state)))))))

;; Debug init and termination

(def-debug-task terminate-debug (process)
  (with-threads-suspended (process :all)
    (dolist (thread (threads-of process))
      (without-call/cc (ptrace-detach (thread-id-of thread) SIGCONT))
      (set-sigchld-handler (thread-id-of thread) nil)
      (%set-thread-state thread 'debug-thread-state-detached))
    (close (mem-file-of process))
    (slot-makunbound process 'mem-file)
    (removef *debugged-processes* process)))

(defun process-memory-maps (process)
  (proc-memory-maps (process-id-of process)))

(def-debug-task initialize-debug (process)
  (with-global-control (process :exclusive? t)
    (flet ((start-attach-to-thread (id)
             (when (ptrace-attach id)
               (%add-debug-thread process id
                                  :initial-state 'debug-thread-state-stopping))))
      (let* ((pid (process-id-of process))
             (main-thread (start-attach-to-thread pid)))
        (unless main-thread
          (abort-task "Could not attach to main thread: ~A" pid))
        (setf (slot-value process 'main-thread) main-thread
              (slot-value process 'last-changed-thread) main-thread)
        (push process *debugged-processes*)
        (loop while (is-running? process)
           do (progn
                (atypecase (wait-for-state process :not-type 'debug-thread-state-running)
                  (debug-thread-state-stopped
                   (%set-thread-options (thread-of it))))
                (let ((ids (process-thread-ids pid))
                      (known (mapcar #'thread-id-of (threads-of process))))
                  (dolist (id (set-difference ids known))
                    (start-attach-to-thread id)))))
        (setf (slot-value process 'mem-file)
              (open (process-memory-file pid) :direction :io :if-exists :overwrite
                    :element-type 'uint8))
        (format t "Loading the debugged executable...~%")
        (let ((executable (executable-of process)))
          (load-executable-mappings executable (process-memory-maps process))))
      (values process (length (threads-of process))))))

(defun/cc start-debug (process-id)
  (let ((process (make-instance 'debug-process :process-id process-id)))
    (call-debug-task 'initialize-debug process)))

(defun/cc stop-debug (process)
  (call-debug-task 'terminate-debug process))

(def-debug-task resume-all-threads (process)
  (with-global-control (process :exclusive? t)
    (dolist (thread (threads-of process))
      (resume-thread thread))))

(def-debug-task stop-all-threads (process)
  (with-global-control (process :exclusive? t)
    (suspend-threads process :all)))

