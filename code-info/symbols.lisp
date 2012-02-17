;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

(defun register-region (image address &key
                        size (type 'executable-region) allow-subrange? allow-splice?)
  (when (eql size 0)
    (setf size nil))
  (flet ((make-new-region ()
           (let ((section (find-section-by-address image address)))
             (unless section
               (return-from register-region nil))
             (aprog1
                 (make-instance type
                                :start-address address
                                :length size
                                :section section
                                :data-bytes (data-bytes-of section)
                                :start-offset (- address (start-address-of section)))
               (insert it (region-map-of image))))))
    (multiple-value-bind (rgn offset) (find-region-by-address image address)
      (cond ((null rgn)
             (make-new-region))
            ((and (/= offset 0) (or allow-splice?
                                    (null (length-of rgn))))
             (make-new-region))
            ((or (= offset 0) allow-subrange?)
             (when (and (= offset 0)
                        (not (typep rgn type))
                        (subtypep type (class-of rgn)))
               (change-class rgn type))
             (when (and (null (length-of rgn))
                        (not (null size)))
               (setf (length-of rgn) size))
             (if (typep rgn type) rgn nil))))))

(defun register-symbols (image table-section)
  (dolist (symbol (elf:data (elf-section-of table-section)))
    (let* ((region-type (case (logand (info symbol) #xF)
                          (0 'executable-region)
                          (1 'executable-region-object)
                          (2 'executable-region-function))))
      (when (and region-type
                 (/= (value symbol) 0)
                 (not (equal (sym-name symbol) "")))
        (awhen (register-region image (value symbol)
                                :size (elf:size symbol)
                                :type region-type)
          (setf (symbol-name-of it) (sym-name symbol)))))))

(defun register-eh-frame (image fde)
  (awhen (let ((addr (dwarf-unwind-fde-start-addr fde)))
           (if (integerp addr) addr
               (case (first addr)
                 (:pc-rel (+ (dwarf-unwind-fde-offset fde) (second addr))))))
    (let ((rgn (register-region image it
                                :size (dwarf-unwind-fde-length fde)
                                :type 'executable-region-function
                                :allow-subrange? t)))
      (if rgn
          (setf (assoc-value (unwind-info-of rgn)
                             (- it (start-address-of rgn)))
                fde)
          (cerror "skip" "Could not register frame: ~S at ~A" fde it)))))

(defun register-eh-frames (image section eh-frame?)
  (let ((frames (parse-unwind-data (data-bytes-of section)
                                   :eh-frame? eh-frame?
                                   :base-addr (start-address-of section))))
    (dolist (frame frames)
      (when (typep frame 'dwarf-unwind-fde)
        (register-eh-frame image frame)))))

(defun region-unwind-table (region)
  (unpack-unwind-table (unwind-info-of region)))

(defun get-image-relocations (image)
  (flet ((describe-rel (reloc syms)
           (multiple-value-bind (shift mask size)
               (etypecase reloc
                 ((or elf::elf-rel-32 elf::elf-rela-32) (values 8 #xFF 4))
                 ((or elf::elf-rel-64 elf::elf-rela-64) (values 32 #xFFFFFFFF 8)))
             (list (offset reloc)
                   size
                   (nth (ash (info reloc) (- shift)) syms)
                   (logand (info reloc) mask)
                   (typecase reloc
                     ((or elf::elf-rela-32 elf::elf-rela-64)
                      (elf::addend reloc))
                     (t 0))))))
    (mapcan (lambda (section &aux (syms (data (linked-elf-of section))))
              (mapcar (lambda (reloc)
                        (describe-rel reloc syms))
                      (elf:data (elf-section-of section))))
            (remove-if-not (lambda (x) (member x '(:rel :rela)))
                           (sections-of image) :key #'elf-type-of))))

(defun annotate-got (image got relocs)
  (dolist (rel-info relocs)
    (let* ((addr (first rel-info))
           (got-offset (- addr (start-address-of got))))
      (when (< -1 got-offset (length-of got))
        (awhen (register-region image addr
                                :size (second rel-info)
                                :type 'executable-region-got-entry
                                :allow-splice? t)
          (setf (symbol-name-of it) (sym-name (third rel-info))))))))

(defun annotate-plt (image plt got)
  (dolist (insn (disassemble-all (data-bytes-of plt)
                                 :base-address (start-address-of plt)))
    (let ((arg1 (x86-instruction-argument1 insn)))
      (when (and (eq (x86-instruction-mnemonic insn) :jmp)
                 (typep arg1 'x86-argument-memory))
        (let* ((iaddr (x86-instruction-address insn))
               (ilen (x86-instruction-length insn))
               (disp (x86-argument-memory-displacement arg1))
               (base (x86-argument-memory-base-reg arg1))
               (index (x86-argument-memory-index-reg arg1))
               (addr (or (when (and (null base) (null index))
                           (if (< -1 (- disp (start-address-of got)) (length-of got))
                               disp
                               (+ disp ilen -4)))
                         (when (and (eq base :ebx) (null index))
                           (+ disp (start-address-of got)))))
               (got-rgn (find-region-by-address image addr)))
          (when (typep got-rgn 'executable-region-got-entry)
            (awhen (register-region image iaddr
                                    :size ilen
                                    :type 'executable-region-plt-entry
                                    :allow-splice? t)
              (setf (symbol-name-of it) (symbol-name-of got-rgn)
                    (got-entry-of it) got-rgn))))))))

(defun annotate-got-plt (image)
  (let ((relocs (get-image-relocations image))
        (got (find-section-by-name image ".got"))
        (got-plt (find-section-by-name image ".got.plt")))
    (when got
      (annotate-got image got relocs))
    (when got-plt
      (annotate-got image got-plt relocs)
      (awhen (find-section-by-name image ".plt")
        (annotate-plt image it got-plt)))))

(defun annotate-regions-from-symbols (image)
  (with-simple-restart (continue "Skip parsing symbols")
    (awhen (find :symtab (sections-of image) :key #'elf-type-of)
      (register-symbols image it))
    (awhen (find :dynsym (sections-of image) :key #'elf-type-of)
      (register-symbols image it)))
  (with-simple-restart (continue "Skip parsing .eh_frames")
    (awhen (find-section-by-name image ".eh_frame")
      (register-eh-frames image it t)))
  (with-simple-restart (continue "Skip parsing .got & .plt")
    (annotate-got-plt image)))
