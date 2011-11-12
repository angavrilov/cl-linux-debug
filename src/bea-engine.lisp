;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug)

(define-foreign-library libBeaEngine
  (t (:default "libBeaEngine")))

(pushnew #.(make-pathname :name nil :type nil
                          :defaults
                          (merge-pathnames #P"lib/"
                                           (asdf:system-definition-pathname :cl-linux-debug)))
         *foreign-library-directories*
         :test #'equal)

(use-foreign-library libBeaEngine)

(defcfun "Disasm" :int
  (dstruct :pointer))

(def (structure e) bea-argument
  mnemonic size access-write?)

(def (structure e) (bea-argument-register (:include bea-argument))
  id high?)

(def (structure e) (bea-argument-constant (:include bea-argument))
  relative?)

(def (structure e) (bea-argument-memory (:include bea-argument))
  base-reg index-reg scale displacement segment-reg)

(defun rename-bea-register (reg-id reg-type)
  (if (eq reg-type :general)
      reg-id
      (macrolet ((mk-vector (stem count)
                   (coerce (loop for i from 0 below count
                              collect (format-symbol :keyword "~A~A" stem i))
                           'vector)))
        (let ((idx (position reg-id (mk-vector #:reg 16) :test #'eq)))
          (assert (integerp idx))
          (svref (ecase reg-type
                   (:mmx (mk-vector #:mm 8))
                   (:fpu (mk-vector #:st 8))
                   (:sse (mk-vector #:xmm 16))
                   (:cr (mk-vector #:cr 16))
                   (:dr (mk-vector #:dr 16))
                   (:special #(:eflags :mxcsr))
                   (:mmu #(:gdtr :ldtr :idtr :tr))
                   (:segment #(:es :cs :ss :ds :fs :gs)))
                 idx)))))

(defun decode-register-type (arg-type-word)
  (let ((id (foreign-enum-keyword 'register-id (logand arg-type-word #xFFFF)))
        (type (foreign-enum-keyword 'register-type
                                    (logand arg-type-word #x0FFF0000))))
    (rename-bea-register id type)))

(defun decode-bea-argument (argument)
  (with-foreign-slots ((mnemonic arg-type arg-size arg-position access-mode memory segment-reg)
                       argument ArgType)
    (let ((mnemonic (foreign-string-to-lisp mnemonic :max-chars 32)))
      (ecase (foreign-enum-keyword 'arg-type-mode (logand arg-type (lognot #x0FFFFFFF)))
        (:NO_ARGUMENT NIL)
        (:REGISTER_TYPE
         (make-bea-argument-register
          :mnemonic mnemonic :size arg-size :access-write? (eq access-mode :write)
          :id (decode-register-type arg-type)
          :high? (and (= arg-size 8) (eq arg-position :high))))
        (:MEMORY_TYPE
         (with-foreign-slots ((base-register index-register scale displacement)
                              memory MemoryType)
           (flet ((no-none (x) (if (eq x :none) nil x)))
             (make-bea-argument-memory
              :mnemonic mnemonic :size arg-size :access-write? (eq access-mode :write)
              :base-reg (no-none base-register) :index-reg (no-none index-register)
              :scale scale :displacement displacement
              :segment-reg segment-reg))))
        (:CONSTANT_TYPE
         (make-bea-argument-constant
          :mnemonic mnemonic :size arg-size :access-write? nil
          :relative? (eq (foreign-enum-keyword 'arg-type-mode (logand arg-type #xFFFFFFF))
                         :RELATIVE_MODE)))))))

(def (structure e) bea-instruction
  (length 0 :type uint8)
  (text nil)
  ;; General information
  category-subset category-function
  opcode mnemonic branch-type
  flags-accessed ; flag -> mode alist
  addr-value immediate
  implicit-modified-reg
  ;; Arguments
  argument1 argument2 argument3
  ;; Prefix -> mode alist
  (prefixes nil) rex-prefix)

(defun decode-bea-flags (flags &aux result)
  (macrolet ((convert (&rest names)
               `(progn ,@(loop for name in names
                            collect `(let ((v (foreign-slot-value flags 'EFLStruct ',name)))
                                       (unless (eq v :unused)
                                         (push (cons ,(make-keyword name) v) result)))))))
    (convert of sf zf af pf cf tf if df nt rf)
    result))

(defun decode-bea-prefixes (prefix &aux result rexlst)
    (macrolet ((convert (&rest names)
                 `(progn ,@(loop for name in names
                            collect `(let ((v (foreign-slot-value prefix 'PrefixInfo ',name)))
                                       (unless (eq v :unused)
                                         (push (cons ,(make-keyword name) v) result)))))))
      (convert lock operand-size address-size repne rep
               fs ss gs es cs ds
               branch-taken branch-not-taken)
      (values result
              (with-foreign-slots ((w r x b state)
                                   (foreign-slot-value prefix 'PrefixInfo 'rex)
                                   REX_Struct)
                (when (/= state 0)
                  (when (/= w 0) (push :w rexlst))
                  (when (/= r 0) (push :r rexlst))
                  (when (/= x 0) (push :x rexlst))
                  (when (/= b 0) (push :b rexlst))
                  (list* :used rexlst))))))

(defun bea-instruction-to-lisp (disasm-obj length)
  (with-foreign-slots ((instr-text instruction argument1 argument2 argument3 prefix)
                       disasm-obj disasm)
    (with-foreign-slots ((category opcode mnemonic branch-type flags
                                   addr-value immediate implicit-modified-regs)
                         instruction InstrType)
      (bind ((subset (foreign-enum-keyword 'category-subset
                                           (logand category (lognot #x0000FFFF))))
             (function (foreign-enum-keyword 'category-function
                                             (logand category #x0000FFFF)))
             ((:values prefixes rex-prefix) (decode-bea-prefixes prefix)))
        (make-bea-instruction :length length
                              :text (foreign-string-to-lisp instr-text :max-chars 64)
                              :category-subset subset :category-function function
                              :opcode opcode
                              :mnemonic (foreign-string-to-lisp mnemonic :max-chars 16)
                              :branch-type (if (eq function :control-transfer) branch-type nil)
                              :addr-value addr-value :immediate immediate
                              :implicit-modified-reg
                              (if (logtest implicit-modified-regs
                                           (foreign-enum-value 'arg-type-mode :REGISTER_TYPE))
                                  (decode-register-type implicit-modified-regs))
                              :flags-accessed (decode-bea-flags flags)
                              :argument1 (decode-bea-argument argument1)
                              :argument2 (decode-bea-argument argument2)
                              :argument3 (decode-bea-argument argument3)
                              :prefixes prefixes :rex-prefix rex-prefix)))))

(def (function e) bea-disassemble (byte-vector offset &key option-list end real-eip arch (errorp t))
  (declare (type (vector uint8) byte-vector))
  (with-foreign-objects ((disasm-obj 'disasm)
                         (buffer :uint8 16))
    (with-foreign-slots ((eip virtual-addr security-block architecture options)
                         disasm-obj disasm)
      (let* ((end (or end (length byte-vector)))
             (size (min 16 (- end offset))))
        (assert (> size 0))
        (loop for i from 0 below size and j from offset
           do (setf (mem-ref buffer :uint8 i) (aref byte-vector j)))
        (setf eip buffer
              virtual-addr (or real-eip 0)
              security-block size
              architecture (ecase arch
                             ((nil 32) 0)
                             ((64 16) arch))
              options (reduce #'logior
                              (mapcar (lambda (x) (foreign-enum-value 'bea-options x))
                                      option-list)))
        (let ((rv (disasm disasm-obj)))
          (if (<= rv 0)
              (progn
                (when errorp
                  (error "Disassembly failed: ~A" rv))
                nil)
              (values (bea-instruction-to-lisp disasm-obj rv) rv)))))))
