;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

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
               (make-instance 'elf-image-section
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

(defun load-elf-image (path)
  (let* ((elf (read-elf path))
         (exec (make-instance 'elf-executable-image
                              :path path
                              :md5-hash (sb-md5:md5sum-file path)
                              :elf-data elf
                              :entry-address (elf::entry (header elf))
                              :shared-lib? (eq (elf:type (header elf))
                                               :shared-object)))
         (sections (enum-elf-sections exec elf)))
    (set-executable-sections exec sections)
    (annotate-regions-from-symbols exec)
    ;; Discard processed elf section content to save memory
    (dolist (section sections)
      (setf (slot-value (elf-section-of section) 'elf:data) nil))
    exec))

(defmethod detect-image-relocation ((image elf-executable-image) executable mappings)
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
                                                 (- len (min (length-of exec-section) 4096))))))
                                    own-mappings)
                           (error "Could not find a mapping for the executable section: ~S ~S"
                                  exec-section mappings)))
         (exec-mapped-start (+ (memory-mapping-start-addr exec-mapping)
                               (- (file-offset-of exec-section)
                                  (memory-mapping-file-offset exec-mapping)))))
    (- exec-mapped-start
       (start-address-of exec-section))))
