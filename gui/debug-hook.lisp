;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.gui)

(defmacro within-main-loop (&body code)
  `(gtk:within-main-loop (gdk:with-gdk-threads-lock ,@code)))

(defun tree-path-of-list (list)
  (aprog1 (make-instance 'tree-path)
    (setf (tree-path-indices it) list)))

(defun ensure-string (data)
  (if (stringp data) data (format nil "~S" data)))

(defun run-restart-dialog (condition restarts)
  (let* ((dlg (make-instance 'message-dialog
                             :message-type :error
                             :title "Unhandled condition"
                             :text "An unhandled condition has occured:"
                             :buttons :none))
         (carea (dialog-content-area dlg))
         (cond-txt (with-output-to-string (stream)
                     (ignore-errors (format stream "~S" condition))))
         (cond-scroll (make-instance 'scrolled-window
                                :hscrollbar-policy :never
                                :vscrollbar-policy :automatic))
         (cond-text-buffer (make-instance 'text-buffer))
         (cond-text-view (make-instance 'text-view
                                        :editable nil
                                        :buffer cond-text-buffer
                                        :wrap-mode :word-char
                                        :height-request 50))
         (restart-scroll (make-instance 'scrolled-window
                                        :hscrollbar-policy :never
                                        :vscrollbar-policy :automatic))
         (restart-list-view (make-instance 'tree-view :headers-visible t :rules-hint t
                                           :width-request 600 :height-request 150))
         (restart-model (make-instance 'list-store :column-types
                                       '("gchararray" "gchararray" "gchararray")))
         (selection (tree-view-selection restart-list-view)))
    ;; Condition text
    (text-buffer-insert cond-text-buffer cond-txt)
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
               (lambda () (invoke-restart-interactively restart)))
             (lambda () nil))
      (object-destroy dlg))))

(defun call-restart-dialog (condition restarts)
  (if (eq (bt:current-thread) gtk::*main-thread*)
      (run-restart-dialog condition restarts)
      (let ((rchan (make-instance 'cl-linux-debug::async-channel)))
        (within-main-loop
          (let ((rvc (constantly nil)))
            (unwind-protect
                 (setf rvc (run-restart-dialog condition restarts))
              (chanl:send rchan rvc))))
        (chanl:recv rchan))))

(defun get-swank-hook ()
  (let ((swank-hook-sym (ignore-errors (read-from-string "swank:swank-debugger-hook"))))
    (if (and swank-hook-sym (fboundp swank-hook-sym))
        (symbol-function swank-hook-sym))))

(defun gui-restart-handler (condition hook)
  (let ((swank-hook (get-swank-hook)))
    (let ((*debugger-hook* swank-hook))
      (let ((restarts (loop for res in (compute-restarts)
                         collect (cons res (ignore-errors (format nil "~A" res))))))
        (funcall (call-restart-dialog condition restarts))))
    (when swank-hook
      (funcall swank-hook condition hook))))

(defun enable-gui-debugger-hook ()
  (setf *debugger-hook* #'gui-restart-handler))

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
