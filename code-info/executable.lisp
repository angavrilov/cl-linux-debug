;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

(defvar *executables* nil)

(defun elf-flag? (value kwd bitmask)
  (or (eq value kwd)
      (and (integerp value) (logtest value bitmask))))

(defun enum-elf-sections (executable elf)
  (loop for section in (sections elf)
     collect (let* ((name (name section))
                    (hdr (sh section))
                    (addr (address hdr))
                    (offset (offset hdr))
                    (size (elf:size hdr))
                    (data (data section))
                    (type (elf:type hdr))
                    (loaded? (elf-flag? (flags hdr) :allocatable 2)))
               (make-instance 'image-section
                              :image executable
                              :elf-section section
                              :elf-type type
                              :section-name name
                              :start-address (if loaded? addr nil)
                              :file-offset offset
                              :length size
                              :loaded? loaded?
                              :linked-elf (if (/= (link hdr) 0)
                                              (nth (link hdr) (sections elf)))
                              :writable? (elf-flag? (flags hdr) :writable 1)
                              :executable? (elf-flag? (flags hdr) :executable 4)
                              :data-bytes (if (and (typep data '(vector uint8))
                                                   (not (eq type :nobits)))
                                              data)))))

(defun get-executable-image (path)
  (or (find path *executables* :key #'path-of :test #'equal)
      (let* ((elf (read-elf path))
             (exec (make-instance 'executable-image
                                  :path path
                                  :elf-data elf
                                  :entry-address (elf::entry (header elf))
                                  :shared-lib? (eq (elf:type (header elf))
                                                   :shared-object)))
             (sections (enum-elf-sections exec elf)))
        (setf (slot-value exec 'sections) sections)
        (dolist (section sections)
          (when (start-address-of section)
            (insert section (section-map-of exec))))
        (annotate-regions-from-symbols exec)
        ;; Discard processed elf section content to save memory
        (dolist (section sections)
          (setf (slot-value (elf-section-of section) 'elf:data) nil))
        (pushnew exec *executables*)
        exec)))


(defun compute-loaded-image (executable image mappings)
  (let* ((own-mappings (remove-if-not
                        (lambda (x) (equal (memory-mapping-file-path x) (path-of image)))
                        mappings))
         (exec-section (or (find-section-by-name image ".text")
                           (first (sort (remove-if-not #'executable? (sections-of image))
                                        #'> :key #'length-of))))
         (exec-mapping (or (find-if (lambda (mapping)
                                      (let ((base (memory-mapping-file-offset mapping))
                                            (len (- (memory-mapping-end-addr mapping)
                                                    (memory-mapping-start-addr mapping))))
                                        (and (memory-mapping-executable? mapping)
                                             (<= 0
                                                 (- (file-offset-of exec-section) base)
                                                 (- len (length-of exec-section))))))
                                    own-mappings)
                           (error "Could not find a mapping for the executable section: ~S ~S"
                                  exec-section mappings)))
         (exec-mapped-start (+ (memory-mapping-start-addr exec-mapping)
                               (- (file-offset-of exec-section)
                                  (memory-mapping-file-offset exec-mapping))))
         (relocation-offset (- exec-mapped-start
                               (start-address-of exec-section)))
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
                                  (or (null p) (equal p "[vdso]")
                                      (ends-with-subseq "(deleted)" p)))
                                (mapcar #'memory-mapping-file-path exec-mappings)))
         (new-paths (set-difference exec-paths
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

