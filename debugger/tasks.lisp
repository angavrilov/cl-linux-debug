;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug)

(def class* debug-task-scheduler ()
  ((name nil :reader t)
   (all-tasks nil :accessor t)
   (active-tasks nil :accessor t)
   (auto-squash-errors? nil :accessor t)))

(def class* debug-task-wait-queue ()
  ((name nil :reader t)
   (waiting-tasks nil :accessor t)))

(def (class* e) debug-task ()
  ((name nil :reader t)
   (scheduler nil :reader t)
   (cur-continuation nil :accessor t)
   (exit-unwinds nil :accessor t)
   (cur-wait-queue nil :accessor t)
   (held-locks nil :accessor t)
   (return-values :reader t)
   (signalled-condition :reader t)
   (finish-condition (make-condition-variable
                      :name "DEBUG-TASK FINISH CONDITION") :reader t)
   (finish-wait-queue (make-instance 'debug-task-wait-queue
                                     :name "DEBUG-TASK FINISH QUEUE") :reader t)))

(defparameter *cur-debug-task* nil)

(defun in-task-context? ()
  (not (null *cur-debug-task*)))

(defmacro def-debug-task (name lambda-args &body code)
  `(defun/cc ,name ,lambda-args
     (without-call/cc (assert (in-task-context?)))
     ,@code))

(defun task-finished? (task)
  (null (finish-condition-of task)))

(defgeneric release-held-lock (lock task))
(defgeneric task-holds-lock? (lock task))

(defun finish-task (task result &key failed?)
  (removef (all-tasks-of (scheduler-of task)) task)
  (removef (active-tasks-of (scheduler-of task)) task)
  (setf (cur-continuation-of task) nil)
  (awhen (cur-wait-queue-of task)
    (deletef (waiting-tasks-of it) task))
  (dolist (lock (copy-list (held-locks-of task)))
    (release-held-lock lock task))
  (if failed?
      (setf (slot-value task 'signalled-condition) result)
      (setf (slot-value task 'return-values) result))
  (let ((fcond (finish-condition-of task)))
    (setf (slot-value task 'finish-condition) nil)
    (condition-notify fcond)
    (wake-up-tasks (finish-wait-queue-of task))))

(defun run-task (scheduler task)
  (assert (eq (scheduler-of task) scheduler))
  (assert (null (cur-wait-queue-of task)))
  (let ((*cur-debug-task* task))
    (multiple-value-bind (code info)
        (block run-core
          (handler-bind ((condition
                          (LAMBDA (ccond)
                            (when (auto-squash-errors? scheduler)
                              (return-from run-core
                                (values 'task-abort ccond))))))
            (restart-case
                (funcall (cur-continuation-of task))
              (abort ()
                :report (LAMBDA (stream)
                          (format stream "Abort the current debug task."))
                (values 'task-abort nil)))))
      (flet ((unwind (info failed?)
               (aif (pop (exit-unwinds-of task))
                    (progn
                      (setf (cur-continuation-of task)
                            (lambda/cc ()
                              (funcall it)
                              (values (if failed? 'task-abort 'task-exit) info)))
                      (run-task scheduler task))
                    (finish-task task info :failed? failed?))))
        (case code
          ((task-wait)
           (setf (cur-continuation-of task) info))
          ((task-yield)
           (setf (cur-continuation-of task) info)
           (pushnew task (active-tasks-of scheduler)))
          ((task-exit)
           (unwind info nil))
          (otherwise
           (unless (eq code 'task-abort)
             (format t "Task returned garbage: ~S ~S~%" code info)
             (setf info nil))
           (unwind info t)))))))

(defun has-scheduled-tasks? (scheduler)
  (not (null (active-tasks-of scheduler))))

(defun run-scheduled-tasks (scheduler)
  (let ((tasks (nreverse (active-tasks-of scheduler))))
    (setf (active-tasks-of scheduler) nil)
    (dolist (task tasks)
      (run-task scheduler task))))

(defun initial-schedule-task (scheduler task)
  (assert (null (scheduler-of task)))
  (setf (slot-value task 'scheduler) scheduler)
  (push task (all-tasks-of scheduler))
  (push task (active-tasks-of scheduler)))

(defun make-task (command-and-args &key name)
  (let* ((cmd (ensure-list command-and-args))
         (rname (or name (first cmd))))
    (make-instance 'debug-task
                   :name rname
                   :cur-continuation
                   (lambda/cc ()
                     (values 'task-exit
                             (multiple-value-list
                              (apply (first cmd) (rest cmd))))))))

(defun has-waiting-tasks? (queue)
  (not (null (waiting-tasks-of queue))))

(defun wake-up-tasks (queue)
  (let ((waiting (waiting-tasks-of queue)))
    (setf (waiting-tasks-of queue) nil)
    (dolist (task waiting)
      (if (eq (cur-wait-queue-of task) queue)
          (progn
            (setf (cur-wait-queue-of task) nil)
            (pushnew task (active-tasks-of (scheduler-of task))))
          (cerror "ignore" "Task ~A waiting on ~A, not on ~A"
                  task (cur-wait-queue-of task) queue)))
    (not (null waiting))))

(def-debug-task wait-on-queue (queue)
  (let/cc continuation
    (assert (null (cur-wait-queue-of *cur-debug-task*)))
    (pushnew *cur-debug-task* (waiting-tasks-of queue))
    (setf (cur-wait-queue-of *cur-debug-task*) queue)
    (values 'task-wait continuation)))

(def-debug-task exit-task (&rest retvals)
  (let/cc continuation
    (declare (ignore continuation))
    (values 'task-exit retvals)))

(def-debug-task abort-task (condition &rest args)
  (let/cc continuation
    (declare (ignore continuation))
    (values 'task-abort
            (etypecase condition
              (condition condition)
              (string (make-condition 'simple-error
                                      :format-control condition :format-arguments args))
              (symbol (apply make-condition condition args))))))

(def-debug-task yield-task ()
  (let/cc continuation
    (values 'task-yield continuation)))

(defun add-task-unwind (callback)
  (push callback (exit-unwinds-of *cur-debug-task*)))

(defun remove-task-unwind (callback)
  (assert (eq callback (first (exit-unwinds-of *cur-debug-task*))))
  (pop (exit-unwinds-of *cur-debug-task*)))

(defmacro with-exit-unwind (form &body unwinds)
  (with-unique-names (unwind)
    `(let ((,unwind (lambda () ,@unwinds)))
       (add-task-unwind ,unwind)
       (prog1
           ,form
        (remove-task-unwind ,unwind)
        (funcall ,unwind)))))

(def class* debug-task-r/w-lock ()
  ((name nil :reader t)
   (readers nil :accessor t)
   (writer nil :accessor t)
   (tasks-waiting-read nil :reader t)
   (tasks-waiting-write nil :reader t)
   (on-read-locked-cb nil :accessor t)
   (on-write-locked-cb nil :accessor t)
   (on-released-cb nil :accessor t)))

(defun make-debug-r/w-lock (&key (name "R/W LOCK"))
  (make-instance 'debug-task-r/w-lock
                 :name name
                 :tasks-waiting-read
                 (make-instance 'debug-task-wait-queue
                                :name (format nil "~A READ WQ" name))
                 :tasks-waiting-write
                 (make-instance 'debug-task-wait-queue
                                :name (format nil "~A WRITE WQ" name))))

(defmethod task-holds-lock? ((lock debug-task-r/w-lock) task)
  (cond ((eq task (writer-of lock)) :write)
        ((member task (readers-of lock)) :read)))

(defmethod release-held-lock ((lock debug-task-r/w-lock) task)
  (case (task-holds-lock? lock task)
    (:read (deletef (readers-of lock) task))
    (:write (setf (writer-of lock) nil)))
  (deletef (held-locks-of task) lock)
  (when (and (null (readers-of lock)) (null (writer-of lock)))
    (awhen (on-released-cb-of lock)
      (funcall it lock))
    (wake-up-tasks (tasks-waiting-write-of lock))
    (wake-up-tasks (tasks-waiting-read-of lock))))

(def-debug-task lock-r/w-lock (lock mode)
  (awhen (task-holds-lock? lock *cur-debug-task*)
    (if (or (eq it mode)
            (and (eq it :write) (eq mode :read)))
        (return-from lock-r/w-lock :already-locked)
        (error "Cannot re-lock ~S as ~S: already ~S"
               lock mode it)))
  (ecase mode
    (:read (loop while (not (null (writer-of lock)))
              do (wait-on-queue (tasks-waiting-read-of lock)))
           (pushnew *cur-debug-task* (readers-of lock))
           (awhen (on-read-locked-cb-of lock)
             (funcall it lock)))
    (:write (loop while (not (and (null (readers-of lock))
                                  (null (writer-of lock))))
               do (wait-on-queue (tasks-waiting-write-of lock)))
            (setf (writer-of lock) *cur-debug-task*)
            (awhen (on-write-locked-cb-of lock)
              (funcall it lock))))
  (pushnew lock (held-locks-of *cur-debug-task*))
  lock)

(defmacro with-r/w-lock-held ((lock mode) &body code)
  (with-unique-names (status)
    `(let* ((,status (lock-r/w-lock ,lock ,mode)))
       (with-exit-unwind
           (progn ,@code)
         (unless (eq ,status :already-locked)
           (release-held-lock ,status *cur-debug-task*))))))
