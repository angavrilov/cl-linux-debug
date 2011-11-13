;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

(def (class* e) address-chunk ()
  ((start-address :reader t)
   (length :reader t)))

(def (class* e) data-chunk (address-chunk)
  ((start-offset 0 :reader t)
   (data-bytes :reader t)))

(defun make-chunk-table ()
  (make-binary-tree :red-black #'<
                    :key #'start-address-of
                    :test #'=))

(defun lookup-chunk (table address)
  (awhen (lower-bound address table)
    (if (or (null (length-of it))
            (< (- address (start-address-of it)) (length-of it)))
        it nil)))

(def (class* e) image-section (data-chunk)
  ((image :reader t)
   (elf-section :reader t)
   (file-offset :reader t)
   (section-name :reader t)))

(def (class* e) executable-image ()
  ((path :reader t)
   (elf-data :reader t)
   (sections :reader t)
   (shared-lib? :reader t)
   (eh-frame nil :reader t)))

(defvar *executables* nil)

(defun enum-elf-sections (executable elf)
  (loop for section in (sections elf)
     collect (let ((name (name section))
                   (addr (address (sh section)))
                   (offset (offset (sh section)))
                   (size (elf:size (sh section)))
                   (data (data section)))
               (make-instance 'image-section
                              :image executable
                              :elf-section section
                              :section-name (if (equal name "") nil name)
                              :start-address (if (= addr 0) nil addr)
                              :file-offset offset
                              :length size
                              :data-bytes (if (typep data '(vector uint8)) data)))))

(defun get-executable-image (path)
  (or (find path *executables* :key #'path-of :test #'equal)
      (let* ((elf (read-elf path))
             (exec (make-instance 'executable-image
                                  :path path
                                  :elf-data elf
                                  :shared-lib? (eq (elf:type (header elf))
                                                   :shared-object))))
        (setf (slot-value exec 'sections) (enum-elf-sections exec elf))
        (awhen (find ".eh_frame" (sections-of exec) :key #'section-name-of :test #'equal)
          (with-simple-restart (continue "Skip decoding eh_frame.")
            (setf (slot-value exec 'eh-frame)
                  (parse-unwind-data (data-bytes-of it) :eh-frame? t))))
        (pushnew exec *executables*)
        exec)))

(def (class* e) loaded-section (data-chunk)
  ((executable :reader t)
   (image :reader t)
   (image-section :reader t)
   (relocated? :reader t)
   (mapping :reader t)))

(def method section-name-of ((section loaded-section))
  (section-name-of (image-section-of section)))

(def (class* e) loaded-function (data-chunk)
  ((section :reader t)
   (unwind-info nil :accessor t)
   (symbol-name nil :accessor t)))

(def (class* e) loaded-executable ()
  ((main-image nil :reader t)
   (all-images nil :accessor t)
   (sections nil :accessor t)
   (section-map (make-chunk-table) :reader t)
   (function-map (make-chunk-table) :reader t)))

(defun add-function-with-remap (executable source-addr remap-table &key length)
  (let* ((remap (lower-bound source-addr remap-table)))
    (when remap
      (let* ((section (cdr remap))
             (offset (- source-addr (car remap))))
        (when (< offset (length-of section))
          (let* ((effective-addr (+ (start-address-of section) offset))
                 (cur-function (lookup-chunk (function-map-of executable) effective-addr))
                 (new-function (if (or (null cur-function)
                                       (and (null (length-of cur-function))
                                            (/= (start-address-of cur-function) effective-addr)))
                                   (make-instance 'loaded-function
                                                  :section section
                                                  :start-address effective-addr
                                                  :length length
                                                  :start-offset (+ (start-offset-of section) offset)
                                                  :data-bytes (data-bytes-of section))
                                   cur-function)))
            (unless (eq cur-function new-function)
              (insert new-function (function-map-of executable)))
            (if (= (start-address-of new-function) effective-addr)
                new-function
                nil)))))))

(defun add-unwind-info (executable unwind-info remap-table)
  (when (integerp (dwarf-unwind-fde-start-addr unwind-info))
    (awhen (add-function-with-remap executable
                                    (dwarf-unwind-fde-start-addr unwind-info)
                                    remap-table
                                    :length (dwarf-unwind-fde-length unwind-info))
      (setf (unwind-info-of it) unwind-info))))

(defun compute-mapped-sections (executable image mappings)
  (let* ((own-mappings (remove-if-not
                        (lambda (x) (equal (memory-mapping-file-path x) (path-of image)))
                        mappings))
         (remap-table (make-binary-tree :red-black #'< :key #'car :test #'=)))
    (dolist (mapping own-mappings)
      (let* ((start-offset (memory-mapping-file-offset mapping))
             (start-addr (memory-mapping-start-addr mapping))
             (length (- (memory-mapping-end-addr mapping) start-addr))
             (end-offset (+ length start-offset))
             (shift (- start-addr start-offset)))
        (dolist (section (sections-of image))
          (let* ((section-offset (file-offset-of section))
                 (map-base (+ shift (file-offset-of section)))
                 (map-start (max 0 (- start-offset section-offset)))
                 (map-end (min (length-of section) (- end-offset section-offset)))
                 (relocated? (aand (start-address-of section)
                                   (/= map-base it))))
            (when (< map-start map-end)
              (let* ((loaded (make-instance 'loaded-section
                                            :executable executable
                                            :image image
                                            :start-address (+ map-base map-start)
                                            :length (- map-end map-start)
                                            :start-offset map-start
                                            :data-bytes (data-bytes-of section)
                                            :image-section section
                                            :relocated? relocated?
                                            :mapping mapping)))
                (push loaded (sections-of executable))
                (when (start-address-of section)
                  (insert (cons (+ (start-address-of section) map-start) loaded) remap-table))
                (insert loaded (section-map-of executable))))))))
    remap-table))

(def (function e) load-executable-mappings (executable mappings)
  (let* ((exec-mappings (remove-if-not #'memory-mapping-executable? mappings))
         (exec-paths (remove-if (lambda (p)
                                  (or (null p) (equal p "[vdso]")
                                      (ends-with-subseq "(deleted)" p)))
                                (mapcar #'memory-mapping-file-path exec-mappings)))
         (new-paths (set-difference exec-paths
                                    (mapcar #'path-of (all-images-of executable))
                                    :test #'equal)))
    (dolist (path new-paths)
      (with-simple-restart (continue "Ignore this executable")
        (let* ((image (get-executable-image path)))
          (push image (all-images-of executable))
          (when (not (shared-lib? image))
            (assert (not (main-image-of executable)))
            (setf (slot-value executable 'main-image) image))
          (let ((remaps (compute-mapped-sections executable image mappings)))
            (dolist (frame (eh-frame-of image))
              (when (typep frame 'dwarf-unwind-fde)
                (add-unwind-info executable frame remaps)))))))))

