;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

(defstruct dwarf-unwind-cie
  (version 1)
  (augmentation-spec "")
  (addr-size 4)
  (seg-size 0)
  (code-afct 1)
  (data-afct 1)
  (ret-register 0)
  (augmentation-data nil)
  (lsda-mode 0)
  (fde-mode 0)
  (signal-frame? nil)
  (personality-function nil)
  (opcodes nil))

(defstruct dwarf-unwind-fde
  (cie nil)
  (offset 0)
  (start-addr 0)
  (length 0)
  (augmentation-data nil)
  (opcodes nil))

(defparameter *default-addr-size* 4)

(defun parse-address (vector pos mode addr-size &key no-rel?)
  (if (= mode #xFF)
      (values nil 0)
      (let ((spos pos)
            (addr (ecase (logand mode #x0F)
                    (#x0 (parsef parse-int vector pos addr-size))
                    (#x1 (parse-leb128f vector pos))
                    (#x2 (parse-intf vector pos 2))
                    (#x3 (parse-intf vector pos 4))
                    (#x4 (parse-intf vector pos 8))
                    (#x8 (parsef parse-int vector pos addr-size :signed? t))
                    (#x9 (parse-leb128f vector pos :signed? t))
                    (#xa (parse-intf vector pos 2 :signed? t))
                    (#xb (parse-intf vector pos 4 :signed? t))
                    (#xc (parse-intf vector pos 8 :signed? t)))))
        (unless no-rel?
          (setf addr (ecase (logand mode #x70)
                       (#x00 addr)
                       (#x10 (list :pc-rel addr spos))
                       (#x20 (list :text-rel addr))
                       (#x30 (list :data-rel addr))
                       (#x40 (list :func-rel addr))
                       (#x50 (list :aligned addr))))
          (when (logtest mode #x80)
            (setf addr (list* :indirect (ensure-list addr)))))
        (values addr (- pos spos)))))

(defun parse-unwind-opcodes (vector pos end-pos cie)
  (let ((addr-size (dwarf-unwind-cie-addr-size cie))
        (fde-mode (dwarf-unwind-cie-fde-mode cie))
        (code-af (dwarf-unwind-cie-code-afct cie))
        (data-af (dwarf-unwind-cie-data-afct cie)))
    (labels ((decode-opcode ()
               (let ((opcode (parse-intf vector pos 1)))
                 (ecase (logand opcode #xC0)
                   (#x40 (list :advance-loc (* (logand opcode #x3F) code-af)))
                   (#x80 (list :offset (logand opcode #x3F)
                               (* (parse-leb128f vector pos) data-af)))
                   (#xC0 (list :restore (logand opcode #x3F)))
                   (#x00
                    (ecase opcode
                      (#x00 :nop)
                      (#x01 (list :set-loc (parsef parse-address vector pos fde-mode addr-size)))
                      (#x02 (list :advance-loc (* (parse-intf vector pos 1) code-af)))
                      (#x03 (list :advance-loc (* (parse-intf vector pos 2) code-af)))
                      (#x04 (list :advance-loc (* (parse-intf vector pos 4) code-af)))
                      (#x05 (list :offset (parse-leb128f vector pos)
                                  (* (parse-leb128f vector pos) data-af)))
                      (#x06 (list :restore (parse-leb128f vector pos)))
                      (#x07 (list :undefined (parse-leb128f vector pos)))
                      (#x08 (list :same-value (parse-leb128f vector pos)))
                      (#x09 (list :register (parse-leb128f vector pos) (parse-leb128f vector pos)))
                      (#x0a '(:remember-state))
                      (#x0b '(:restore-state))
                      (#x0c (list :def-cfa (parse-leb128f vector pos) (parse-leb128f vector pos)))
                      (#x0d (list :def-cfa-register (parse-leb128f vector pos)))
                      (#x0e (list :def-cfa-offset (parse-leb128f vector pos)))
                      ((#x0f #x10 #x16)
                       ;; Expressions not supported
                       (return-from parse-unwind-opcodes :unsupported))
                      (#x11 (list :offset (parse-leb128f vector pos)
                                  (* (parse-leb128f vector pos :signed? t) data-af)))
                      (#x12 (list :def-cfa (parse-leb128f vector pos)
                                  (* (parse-leb128f vector pos :signed? t) data-af)))
                      (#x13 (list :def-cfa-offset
                                  (* (parse-leb128f vector pos :signed? t) data-af)))
                      ;; GNU
                      (#x2e (list :gnu-args-size (parse-leb128f vector pos)))
                      (#x2f (list :offset (parse-leb128f vector pos)
                                  (- (* (parse-leb128f vector pos) data-af))))))))))
      (loop while (< pos end-pos)
         for code = (decode-opcode)
         unless (eq code :nop)
         collect code))))

(defun parse-cie-augmentation (cie)
  (let ((vector (dwarf-unwind-cie-augmentation-data cie))
        (pos 0))
    (loop for char across (dwarf-unwind-cie-augmentation-spec cie) and idx from 0
       do (case char
            (#\z (assert (= idx 0)))
            (#\L (setf (dwarf-unwind-cie-lsda-mode cie) (parse-intf vector pos 1)))
            (#\R (setf (dwarf-unwind-cie-fde-mode cie) (parse-intf vector pos 1)))
            (#\S (setf (dwarf-unwind-cie-signal-frame? cie) t))
            (#\P (let ((pmode (parse-intf vector pos 1)))
                   (setf (dwarf-unwind-cie-personality-function cie)
                         (parsef parse-address vector pos pmode
                                 (dwarf-unwind-cie-addr-size cie)))))
            (otherwise (return))))))

(defun parse-unwind-cie (vector pos end-pos eh-frame?)
  (let* ((version (parse-intf vector pos 1))
         (aug-string (parse-stringf vector pos))
         (asize (if eh-frame? *default-addr-size*
                    (parse-intf vector pos 1)))
         (ssize (if eh-frame? 0 (parse-intf vector pos 1)))
         (code-afct (parse-leb128f vector pos))
         (data-afct (parse-leb128f vector pos :signed? t))
         (ret-reg (ecase version
                    (1 (parse-intf vector pos 1))
                    (3 (parse-leb128f vector pos))))
         (aug-size (cond ((equal aug-string "") 0)
                         ((starts-with-subseq "z" aug-string)
                          (parse-leb128f vector pos))
                         (t (error "Unsupported augmentation: ~S" aug-string))))
         (aug-data (parse-bytesf vector pos aug-size))
         (cie (make-dwarf-unwind-cie
               :version version
               :augmentation-spec aug-string
               :addr-size asize
               :seg-size ssize
               :code-afct code-afct
               :data-afct data-afct
               :ret-register ret-reg
               :augmentation-data aug-data)))
    (parse-cie-augmentation cie)
    (setf (dwarf-unwind-cie-opcodes cie)
          (parse-unwind-opcodes vector pos end-pos cie))
    cie))

(defun parse-unwind-fde (vector pos end-pos cie base-addr)
  (let* ((fde-mode (dwarf-unwind-cie-fde-mode cie))
         (addr-size (dwarf-unwind-cie-addr-size cie))
         (aug-string (dwarf-unwind-cie-augmentation-spec cie))
         (base-pos pos)
         (start-addr (parsef parse-address vector pos fde-mode addr-size))
         (code-length (parsef parse-address vector pos fde-mode addr-size :no-rel? t))
         (aug-size (if (starts-with-subseq "z" aug-string)
                       (parse-leb128f vector pos)
                       0))
         (aug-data (parse-bytesf vector pos aug-size)))
    (make-dwarf-unwind-fde :cie cie :offset (+ base-addr base-pos)
                           :start-addr start-addr
                           :length code-length
                           :augmentation-data aug-data
                           :opcodes (parse-unwind-opcodes vector pos end-pos cie))))

(defun parse-unwind-record (vector pos cie-table eh-frame? base-addr)
  (let* ((start-pos pos)
         (size1 (parse-intf vector pos 4))
         (is-64? (= size1 #xFFFFFFFF))
         (size (if is-64?
                   (parse-intf vector pos 8)
                   size1)))
    (when (= size 0)
      (return-from parse-unwind-record (values nil pos)))
    (let* ((offset-base pos)
           (offset (if is-64?
                       (parse-intf vector pos 8 :signed? t)
                       (parse-intf vector pos 4 :signed? t)))
           (end-pos (+ offset-base size)))
      (values (with-simple-restart (continue "Ignore the current record")
                (if (= offset 0)
                    (setf (gethash start-pos cie-table)
                          (parse-unwind-cie vector pos end-pos eh-frame?))
                    (let ((cie (gethash (if eh-frame?
                                            (- offset-base offset)
                                            offset)
                                        cie-table)))
                      (when (null cie)
                        (error "Could not find CIE referenced from ~A: ~A" offset-base offset))
                      (parse-unwind-fde vector pos end-pos cie base-addr))))
              end-pos))))

(defun parse-unwind-data (vector &key eh-frame? (base-addr 0))
  (let ((cie-table (make-hash-table))
        (pos 0)
        (records nil))
    (loop while (< pos (length vector))
       do (multiple-value-bind (record new-pos)
              (parse-unwind-record vector pos cie-table eh-frame? base-addr)
            (setf pos new-pos)
            (when record
              (push record records))))
    (nreverse records)))
