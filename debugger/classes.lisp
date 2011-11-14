;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug)

;; Thread states

(def (class* e) debug-thread-state ()
  ((thread :reader t)))

(def (class* e) debug-thread-state-inactive (debug-thread-state)
  ())

(def (class* e) debug-thread-state-detached (debug-thread-state-inactive)
  ())

(def (class* e) debug-thread-state-active (debug-thread-state)
  ())

(def (class* e) debug-thread-state-running (debug-thread-state-active)
  ())

(def (class* e) debug-thread-state-stopping (debug-thread-state-running)
  ())

(def (class* e) debug-thread-state-dead (debug-thread-state-inactive)
  ())

(def (class* e) debug-thread-state-exited (debug-thread-state-dead)
  ((return-code :reader t)))

(def (class* e) debug-thread-state-killed (debug-thread-state-dead)
  ((signal-id :reader t)
   (dumped? :reader t)))

(def (class* e) debug-thread-state-stopped (debug-thread-state-active)
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
   (event-condition (make-instance 'debug-task-wait-queue :name "Thread Event")
                           :reader t)
   (control-lock (make-debug-r/w-lock :name "Thread Lock") :reader t)))

(def (class* e) debug-process ()
  ((process-id :reader t)
   (main-thread nil :reader t)
   (threads nil :reader t)
   (executable (make-instance 'loaded-executable) :reader t)
   (injection-info nil :accessor t)
   (last-changed-thread nil :reader t)
   (event-condition (make-instance 'debug-task-wait-queue :name "Global Event")
                           :reader t)
   (control-lock (make-debug-r/w-lock :name "Global Lock") :reader t)))

(def class* code-injection-info ()
  ((process :reader t)
   (injected-mappings nil :accessor t)
   (syscall-inject-ptr nil :reader t)
   (free-code-areas nil :accessor t)))

(defgeneric is-running? (obj)
  (:method ((obj debug-thread-state)) nil)
  (:method ((obj debug-thread-state-running)) t)
  (:method ((obj debug-thread))
    (is-running? (confirmed-state-of obj)))
  (:method ((obj debug-process))
    (some #'is-running? (threads-of obj))))

(defgeneric most-recent-state-of (obj)
  (:method ((obj debug-thread))
    (confirmed-state-of obj))
  (:method ((obj debug-process))
    (most-recent-state-of (last-changed-thread-of obj))))

(defmacro with-global-control ((process &key exclusive?) &body code)
  `(with-r/w-lock-held ((control-lock-of ,process)
                        (if ,exclusive? :write :read))
     ,@code))

(defmacro with-thread-control ((thread &key exclusive?) &body code)
  `(with-global-control ((process-of ,thread) :exclusive? nil)
     (with-r/w-lock-held ((control-lock-of ,thread)
                          (if ,exclusive? :write :read))
       ,@code)))
