;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.gui)

(defmacro within-main-loop (&body code)
  `(gtk:within-main-loop (gdk:with-gdk-threads-lock ,@code)))

(defun tree-path-of-list (list)
  (aprog1 (make-instance 'tree-path)
    (setf (tree-path-indices it) list)))

(defun ensure-string (data)
  (if (stringp data) data (format nil "~S" data)))

;;; Query dialog and streams

(defun run-query-dialog (request &key no-text?)
  (let* ((dlg (make-instance 'message-dialog
                             :message-type (if no-text? :info :question)
                             :title "Query"
                             :text request
                             :buttons :ok))
         (carea (dialog-content-area dlg))
         (entry (make-instance 'entry :widght-request 600)))
    (unless no-text?
      (box-pack-start carea entry)
      (connect-signal entry "activate"
                      (lambda (v)
                      (declare (ignore v))
                      (dialog-response dlg :ok))))
    (setf (dialog-default-response dlg) :ok)
    (setf (gtk-window-keep-above dlg) t)
    (widget-show dlg)
    (gtk-window-present dlg)
    (prog1
        (if (eq (dialog-run dlg) :ok)
            (entry-text entry)
            nil)
      (object-destroy dlg))))

(defun call-dialog (cb)
  (if (eq (bt:current-thread) gtk::*main-thread*)
      (funcall cb)
      (let ((rchan (make-instance 'cl-linux-debug::async-channel)))
        (within-main-loop
          (let ((rvc (constantly nil)))
            (unwind-protect
                 (setf rvc (funcall cb))
              (chanl:send rchan rvc))))
        (chanl:recv rchan))))

(defun call-query-dialog (request &key no-text?)
  (call-dialog (lambda () (run-query-dialog request :no-text? no-text?))))

(defclass debug-dialog-stream (fundamental-character-input-stream
                               fundamental-character-output-stream)
  ((in-stream :initform (make-string-input-stream "") :accessor in-stream-of)
   (out-string :initarg :query :initform "" :accessor out-string-of)
   (out-stream :initform (make-string-output-stream) :accessor out-stream-of)))

(defmethod stream-element-type ((stream debug-dialog-stream))
  (stream-element-type (out-stream-of stream)))

(defmethod close ((stream debug-dialog-stream) &key abort)
  (unless abort
    (let ((str (get-output-stream-string (out-stream-of stream))))
      (unless (equal str "")
        (call-query-dialog str :no-text? t)))))

(defmethod interactive-stream-p ((stream debug-dialog-stream)) t)

(defmethod stream-read-char-no-hang ((stream debug-dialog-stream))
  (read-char (in-stream-of stream) nil nil))

(defmethod stream-read-char ((stream debug-dialog-stream))
  (loop
     (aif (read-char (in-stream-of stream) nil nil)
          (return it)
          (progn
            (let ((str (get-output-stream-string (out-stream-of stream))))
              (unless (equal str "")
                (setf (out-string-of stream) str)))
            (setf (in-stream-of stream)
                  (make-string-input-stream
                   (aif (call-query-dialog (out-string-of stream))
                        (concatenate 'string it '(#\Newline))
                        "")))))))

(defmethod stream-clear-input ((stream debug-dialog-stream))
  (clear-input (in-stream-of stream)))

(defmethod stream-clear-output ((stream debug-dialog-stream))
  (setf (out-string-of stream) "")
  (clear-output (out-stream-of stream)))

(defmethod stream-unread-char ((stream debug-dialog-stream) char)
  (unread-char char (in-stream-of stream)))

(defmethod stream-fresh-line ((stream debug-dialog-stream))
  (fresh-line (out-stream-of stream)))

(defmethod stream-write-char ((stream debug-dialog-stream) char)
  (write-char char (out-stream-of stream)))

;;; Restart selection dialog

(defun run-restart-dialog (condition restarts)
  (let* ((dlg (make-instance 'message-dialog
                             :message-type :error
                             :title "Unhandled condition"
                             :text "An unhandled condition has occured:"
                             :buttons :none))
         (carea (dialog-content-area dlg))
         (cond-scroll (make-instance 'scrolled-window
                                :hscrollbar-policy :never
                                :vscrollbar-policy :automatic))
         (cond-text-buffer (make-instance 'text-buffer))
         (cond-text-view (make-instance 'text-view
                                        :editable nil
                                        :buffer cond-text-buffer
                                        :wrap-mode :word-char
                                        :height-request 100))
         (restart-scroll (make-instance 'scrolled-window
                                        :hscrollbar-policy :never
                                        :vscrollbar-policy :automatic))
         (restart-list-view (make-instance 'tree-view :headers-visible t :rules-hint t
                                           :width-request 600 :height-request 150))
         (restart-model (make-instance 'list-store :column-types
                                       '("gchararray" "gchararray" "gchararray")))
         (selection (tree-view-selection restart-list-view)))
    ;; Condition text
    (text-buffer-insert cond-text-buffer condition)
    (container-add cond-scroll cond-text-view)
    (box-pack-start carea cond-scroll)
    ;; Restart list
    (box-pack-start carea (make-instance 'label :label "Available restarts:" :xalign 0.0))
    (setf (tree-view-model restart-list-view) restart-model
          (tree-selection-mode selection) :browse)
    (flet ((add-column (id title min-width expand?)
             (let ((column (make-instance 'tree-view-column :title title
                                          :min-width min-width
                                          :resizable t
                                          :expand expand?))
                   (renderer (make-instance 'cell-renderer-text :text "A text" :xalign 0.0)))
               (setf (tree-view-column-sizing column) :fixed)
               (tree-view-column-pack-start column renderer)
               (tree-view-column-add-attribute column renderer "text" id)
               (tree-view-column-add-attribute column renderer "foreground" 2)
               (tree-view-append-column restart-list-view column))))
      (add-column 0 "Name" 150 nil)
      (add-column 1 "Comment" 400 t))
    (container-add restart-scroll restart-list-view)
    (box-pack-start carea restart-scroll)
    (loop with color = "black"
       for res in restarts and i from 0
       for name = (ignore-errors (restart-name (car res)))
       do (when (member name '(sb-thread::terminate-thread))
            (setf color "darkgray"))
       do (list-store-insert-with-values
           restart-model i
           (ensure-string (symbol-name name))
           (ensure-string (cdr res))
           color)
       do (when (member name '(gdk::return-from-callback
                               gobject::return-from-g-closure
                               gobject::return-from-interface-method-implementation
                               gobject::return-from-property-getter
                               gobject::return-without-error-from-property-setter))
            (setf color "darkgray")))
    (when restarts
      (tree-selection-select-path selection (tree-path-of-list '(0))))
    (connect-signal restart-list-view "row-activated"
                    (lambda (v p c)
                      (declare (ignore v p c))
                      (dialog-response dlg :accept)))
    ;; Buttons
    (dialog-add-button dlg "Debug" :cancel)
    (dialog-add-button dlg "Recover" :accept)
    (setf (dialog-default-response dlg) :accept)
    (setf (gtk-window-keep-above dlg) t)
    (widget-show dlg)
    (gtk-window-present dlg)
    (prog1
        (aif (aand (eq (dialog-run dlg) :accept)
                   (tree-selection-selected-rows selection))
             (let* ((idx (first (tree-path-indices (first it))))
                    (restart (car (nth idx restarts))))
               (lambda ()
                 (with-open-stream (dds (make-instance 'debug-dialog-stream
                                                       :query "Enter the restart parameter:"))
                   (let ((*debug-io* dds) (*query-io* dds)
                         (*standard-input* dds) (*standard-output* dds))
                     (invoke-restart-interactively restart)))))
             (lambda () nil))
      (object-destroy dlg))))

(defun call-restart-dialog (condition restarts)
  (call-dialog (lambda () (run-restart-dialog condition restarts))))

;;; Restart selection dialog hook

(defun get-swank-hook ()
  (let ((swank-hook-sym (ignore-errors (read-from-string "swank:swank-debugger-hook"))))
    (if (and swank-hook-sym (fboundp swank-hook-sym))
        (symbol-function swank-hook-sym))))

(defun gui-restart-handler (condition hook)
  (let ((swank-hook (get-swank-hook))
        (frame (or sb-debug:*stack-top-hint*
                   (sb-di:frame-down (sb-di:top-frame)))))
    (let ((*debugger-hook* swank-hook))
      (let ((cond-txt (with-output-to-string (stream)
                        (ignore-errors (format stream "~S: ~A" (type-of condition) condition))
                        (format stream "~%~%Backtrace:~%")
                        (let ((sb-debug::*in-the-debugger* t)
                              (sb-debug::*current-frame* frame))
                          (sb-debug:backtrace 100 stream))))
            (restarts (loop for res in (compute-restarts)
                         collect (cons res (ignore-errors (format nil "~A" res))))))
        (funcall (call-restart-dialog cond-txt restarts))))
    (when swank-hook
      (funcall swank-hook condition hook))))

(defun enable-gui-debugger-hook ()
  (setf *debugger-hook* #'gui-restart-handler))

;;; Support for offloading computation to another thread,
;;; with a progress dialog to occupy the message loop.

(defun run-in-other-thread (code &key parent title)
  (let* ((dlg (make-instance 'dialog :title (or title "Processing")
                             :resizable nil))
         (carea (dialog-content-area dlg))
         (progress (make-instance 'progress-bar
                                  :width-request 300 :height-request 15
                                  :xspacing 15 :yspacing 15))
         (running t)
         (result nil))
    (when parent
      (setf (gtk-window-transient-for dlg) (widget-toplevel parent)))
    (box-pack-start carea progress)
    (gtk-main-add-timeout 100 (lambda ()
                                (with-gdk-threads-lock
                                  (when running
                                    (progress-bar-pulse progress)
                                    t))))
    (setf (gtk-window-deletable dlg) nil)
    (widget-show dlg)
    (bt:make-thread (lambda (&aux rv)
                      (unwind-protect
                           (with-simple-restart (abort "Abort run-in-another-thread")
                             (setf rv (funcall code)))
                        (within-main-loop
                          (setf result rv running nil)
                          (dialog-response dlg :accept))))
                    :name "run-in-another-thread")
    (loop while (not (eq (dialog-run dlg) :accept)))
    (setf running nil)
    (object-destroy dlg)
    result))

(defmacro in-another-thread ((parent &key title) &body code)
  `(run-in-other-thread (lambda () ,@code) :parent ,parent :title ,title))
