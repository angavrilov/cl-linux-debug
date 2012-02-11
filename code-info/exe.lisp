;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

(define-binary-type uchar () (elf::unsigned-integer :bytes 1))
(define-binary-type ushort () (elf::unsigned-integer :bytes 2))
(define-binary-type ulong  () (elf::unsigned-integer :bytes 4))

(define-binary-class exe-dos-header ()
  ((magic-number (string :length 2))
   (e_cblp ushort)       ; Bytes on last page of file
   (e_cp USHORT)         ; Pages in file
   (e_crlc USHORT)       ; Relocations
   (e_cparhdr USHORT)    ; Size of header in paragraphs
   (e_minalloc USHORT)   ; Minimum extra paragraphs needed
   (e_maxalloc USHORT)   ; Maximum extra paragraphs needed
   (e_ss USHORT)         ; Initial (relative) SS value
   (e_sp USHORT)         ; Initial SP value
   (e_csum USHORT)       ; Checksum
   (e_ip USHORT)         ; Initial IP value
   (e_cs USHORT)         ; Initial (relative) CS value
   (e_lfarlc USHORT)     ; File address of relocation table
   (e_ovno USHORT)       ; Overlay number
   (e_res (elf::raw-bytes :length 8)) ; Reserved words
   (e_oemid USHORT)      ; OEM identifier (for e_oeminfo)
   (e_oeminfo USHORT)    ; OEM information; e_oemid specific
   (e_res2 (elf::raw-bytes :length 20)) ; Reserved words
   (e_lfanew ULONG)      ; File address of new exe header
   ))

(defun read-dos-header (stream)
  (let ((header (read-value 'exe-dos-header stream)))
    (unless (string= (magic-number header) "MZ")
      (error "Invalid EXE signature: ~S" (magic-number header)))
    header))

(define-binary-class exe-win-header ()
  ((magic-number ULONG)
   (machine USHORT)
   (section-count USHORT)
   (time-date-stamp ULONG)
   (symtab-ptr ULONG)
   (symtab-cnt ULONG)
   (opthdr-size USHORT)
   (characteristics USHORT)))

(define-binary-class exe-aux-header ()
  ((magic-number USHORT)
   ;; Standard
   (major-linker-version UCHAR)
   (minor-linker-version UCHAR)
   (size-of-code ULONG)
   (size-of-data ULONG)
   (size-of-bss ULONG)
   (entry-pt-addr ULONG)
   (base-of-code ULONG)
   (base-of-data ULONG)
   ;; NT
   (image-base ULONG)
   (section-alignment ULONG)
   (file-alignment ULONG)
   (major-os-version USHORT)
   (minor-os-version USHORT)
   (major-img-version USHORT)
   (minor-img-version USHORT)
   (major-subsys-version USHORT)
   (minor-subsys-version USHORT)
   (reserved1 ULONG)
   (image-size ULONG)
   (headers-size ULONG)
   (checksum ULONG)
   (subsystem USHORT)
   (dll-characteristics USHORT)
   (stack-reserve-size ULONG)
   (stack-commit-size ULONG)
   (heap-reserve-size ULONG)
   (heap-commit-size ULONG)
   (loader-flags ULONG)
   (rva-and-size-count ULONG)))

(defun read-win-header (stream offset)
  (file-position stream offset)
  (let ((header (read-value 'exe-win-header stream)))
    (unless (= (magic-number header) #x00004550)
      (error "Invalid PE signature: ~X" (magic-number header)))
    (let ((aux (read-value 'exe-aux-header stream)))
      (values header aux (+ offset 4 20 (opthdr-size header))))))

(define-binary-class exe-section-header ()
  ((name (elf::raw-bytes :length 8))
   (virtual-size ULONG)
   (virtual-addr ULONG)
   (raw-data-size ULONG)
   (raw-data-ptr ULONG)
   (relocation-ptr ULONG)
   (linenumber-ptr ULONG)
   (relocation-count USHORT)
   (linenumber-count USHORT)
   (characteristics ULONG)))

(defun read-exe-sections (stream offset count)
  (file-position stream offset)
  (loop for i from 0 below count
     collect (read-value 'exe-section-header stream)))

(defun load-exe-section (stream exec base header)
  (bind ((flags (characteristics header))
         (bss? (or (logtest flags #x80) (= (raw-data-ptr header) 0)))
         (executable? (logtest flags #x20000000))
         (writable? (logtest flags #x80000000))
         (length (min (raw-data-size header) (virtual-size header)))
         (buffer (unless bss? (make-byte-vector length))))
    (when buffer
      (file-position stream (raw-data-ptr header))
      (read-sequence buffer stream))
    (make-instance 'pe-image-section
                   :image exec
                   :header header
                   :section-name (parse-string (name header) 0)
                   :start-address (+ base (virtual-addr header))
                   :file-offset (raw-data-ptr header)
                   :length length
                   :loaded? t
                   :writable? writable?
                   :executable? executable?
                   :data-bytes buffer)))

(defun load-exe-sections (stream exec base hdrs)
  (loop for hdr in hdrs
     for section = (load-exe-section stream exec base hdr)
     collect section
     ;; Handle a weird way of implicitly specifying bss:
     when (> (virtual-size hdr) (length-of section))
     collect (make-instance 'pe-image-section
                            :image exec
                            :header hdr
                            :section-name
                            (if (string= (section-name-of section) ".data") ".bss" "")
                            :start-address (+ (start-address-of section) (length-of section))
                            :length (- (virtual-size hdr) (length-of section))
                            :loaded? t
                            :writable? (writable? section) :executable? (executable? section)
                            :data-bytes nil :file-offset nil)))

(defun load-exe-image (path)
  (with-open-file (stream path :element-type 'uint8)
    (bind ((dos-hdr
            (read-dos-header stream))
           ((:values win-hdr aux-hdr sect-pos)
            (read-win-header stream (e_lfanew dos-hdr)))
           (section-hdrs
            (read-exe-sections stream sect-pos (section-count win-hdr)))
           (base (image-base aux-hdr))
           (shared-lib? (logtest (characteristics win-hdr) #x2000))
           (exec
            (make-instance 'pe-executable-image
                           :path path
                           :md5-hash (sb-md5:md5sum-file path)
                           :binary-timestamp (time-date-stamp win-hdr)
                           :dos-header dos-hdr :win-header win-hdr
                           :aux-header aux-hdr
                           :entry-address (+ base (entry-pt-addr aux-hdr))
                           :shared-lib? shared-lib?))
           (sections
            (load-exe-sections stream exec base section-hdrs)))
      (set-executable-sections exec sections)
      exec)))

(defmethod detect-image-relocation ((image pe-executable-image) executable mappings)
  (let* ((mapping (find-if (lambda (x) (and (equal (memory-mapping-file-path x) (path-of image))
                                       (= (memory-mapping-file-offset x) 0)))
                           mappings)))
    (unless mapping
      (error "Could not find the header mapping for ~S in ~S" image mappings))
    (- (memory-mapping-start-addr mapping)
       (image-base (aux-header-of image)))))
