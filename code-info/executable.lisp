;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

(defvar *executables* nil)

(defvar *shared-executables* '("/usr/bin/wine" "/usr/bin/wine-preloader")
  "Executables to treat as shared libraries.")

(defun get-executable-image (path)
  (or (find path *executables* :key #'path-of :test #'equal)
      (aprog1
          (let ((ext (string-upcase (pathname-type path))))
            (cond ((member ext '("EXE" "DLL") :test #'equal)
                   (load-exe-image path))
                  (t
                   (load-elf-image path))))
        (when (member (namestring path) *shared-executables* :test #'equal)
          (setf (slot-value it 'shared-lib?) t))
        (pushnew it *executables*))))

(defun compute-loaded-image (executable image mappings)
  (let* ((relocation-offset (detect-image-relocation image executable mappings))
         (loaded-image (make-instance 'loaded-image
                                      :executable executable
                                      :origin image
                                      :sections nil
                                      :relocation-offset relocation-offset)))
    (dolist (section (remove-if-not #'loaded? (sections-of image)))
      (let* ((reloc-start (+ (start-address-of section) relocation-offset))
             (mapping (find-if (lambda (mapping)
                                 (<= (memory-mapping-start-addr mapping) reloc-start
                                     (- (memory-mapping-end-addr mapping) (length-of section))))
                               mappings)))
        (let* ((loaded (make-instance 'loaded-section
                                      :loaded-image loaded-image
                                      :start-address reloc-start
                                      :length (length-of section)
                                      :start-offset (start-offset-of section)
                                      :data-bytes (data-bytes-of section)
                                      :origin section
                                      :relocation-offset relocation-offset
                                      :mapping mapping)))
          (push loaded (slot-value loaded-image 'sections))
          (insert loaded (section-map-of loaded-image))
          (push loaded (slot-value executable 'sections))
          (insert loaded (section-map-of executable)))))
    loaded-image))

(def (function e) load-executable-mappings (executable mappings)
  (let* ((exec-mappings (remove-if-not #'memory-mapping-executable? mappings))
         (exec-paths (remove-if (lambda (p)
                                  (or (null p)
                                      (starts-with-subseq "[" p)
                                      (ends-with-subseq "(deleted)" p)))
                                (mapcar #'memory-mapping-file-path exec-mappings)))
         (new-paths (set-difference (remove-duplicates exec-paths :test #'equal)
                                    (mapcar #'path-of (all-images-of executable))
                                    :test #'equal)))
    (dolist (path new-paths)
      (with-simple-restart (continue "Ignore this executable")
        (let* ((image (get-executable-image path))
               (loaded (compute-loaded-image executable image mappings)))
          (push loaded (all-images-of executable))
          (when (not (shared-lib? image))
            (assert (not (main-image-of executable)))
            (setf (slot-value executable 'main-image) loaded)))))))

(defun disassemble-function (executable fname)
  (awhen (remove-if-not (lambda (rgn)
                          (typep (origin-of rgn) 'executable-region-function))
                        (find-regions-by-name executable fname))
    (assert (= (length it) 1))
    (bind ((rgn (first it))
           (orgn (origin-of rgn))
           (image (image-of (section-of orgn)))
           (got (find-section-by-name image ".got.plt"))
           (start (start-offset-of orgn))
           (length (length-of orgn))
           (insns (disassemble-all (data-bytes-of orgn)
                                   :start start :end (+ start length)
                                   :base-address (start-address-of orgn))))
      (assert (and got insns))
      (values insns (start-address-of got) (relocation-offset-of rgn)))))
