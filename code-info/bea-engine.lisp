;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

(define-foreign-library libBeaEngine
  (t (:default #+x86-64 "libBeaEngine_64" #-x86-64 "libBeaEngine")))

(pushnew #.(aprog1
               (make-pathname :name nil :type nil
                              :defaults
                              (merge-pathnames #P"lib/"
                                               (asdf:system-definition-pathname :cl-linux-debug)))
             (format t "Library load dir: ~S~%" it))
         *foreign-library-directories*
         :test #'equal)

(use-foreign-library libBeaEngine)

(defcfun "Disasm" :int
  (dstruct :pointer))

(def (structure ea) x86-argument
  mnemonic size access-write?)

(def (structure ea) (x86-argument-register (:include x86-argument))
  id high?)

(def (structure ea) (x86-argument-constant (:include x86-argument))
  relative?)

(def (structure ea) (x86-argument-memory (:include x86-argument))
  base-reg index-reg scale displacement segment-reg)

(defstruct x86-register
  (name :x :type keyword)
  (category :x :type keyword)
  (index 0 :type fixnum)
  (size 0 :type fixnum)
  (read-mask 1 :type fixnum)
  (write-mask 1 :type fixnum)
  (side nil :type symbol))

(macrolet ((mkregs (general-specs specials mmus segments)
             (labels ((mk-list (stem count)
                        (loop for i from 0 below count
                           collect (format-symbol :keyword "~A~A" stem i)))
                      (mk-vector (stem count)
                        (coerce (mk-list stem count) 'vector)))
               (let ((abstract (mk-list '#:reg 16))
                     general-low general-high general-16 general-32 general-64)
                 (loop for (r64 r32 r16 low high) in (reverse general-specs)
                    do (progn
                         (when low (push low general-low))
                         (when high (push high general-high))
                         (when r16 (push r16 general-16))
                         (when r32 (push r32 general-32))
                         (push r64 general-64)))
                 (let ((specs (append
                               (loop for reg in abstract and i from 0
                                  collect `(,reg ,i :abstract))
                               (loop for reg in general-64 and i from 0
                                  collect `(,reg ,i :general 64 #xFF #xFF))
                               (loop for reg in general-32 and i from 0
                                  collect `(,reg ,i :general 32 #xF #xFF))
                               (loop for reg in general-16 and i from 0
                                  collect `(,reg ,i :general 16 #x3 #x3))
                               (loop for reg in general-low and i from 0
                                  collect `(,reg ,i :general 8 #x1 #x1 :low))
                               (loop for reg in general-high and i from 0
                                  collect `(,reg ,i :general 8 #x2 #x2 :high))
                               (loop for reg in (mk-list :mm 8) and i from 0
                                  collect `(,reg ,i :mmx 64))
                               (loop for reg in (mk-list :st 8) and i from 0
                                  collect `(,reg ,i :fpu 80))
                               (loop for reg in (mk-list :xmm 16) and i from 0
                                  collect `(,reg ,i :sse 128))
                               (loop for reg in (mk-list :cr 16) and i from 0
                                  collect `(,reg ,i :cr 32))
                               (loop for reg in (mk-list :dr 16) and i from 0
                                  collect `(,reg ,i :dr 32))
                               (loop for reg in specials and i from 0
                                  collect `(,reg ,i :special 32))
                               (loop for reg in mmus and i from 0
                                  collect `(,reg ,i :mmu 32))
                               (loop for reg in segments and i from 0
                                  collect `(,reg ,i :segment 16)))))
                   `(progn
                      (defparameter *x86-register-table*
                        (let ((table (make-hash-table :test #'eq)))
                          (dolist (item ',specs)
                            (destructuring-bind
                                  (name idx cat &optional (size 0) (rmask 1) (wmask 1) (side nil))
                                item
                              (setf (gethash name table)
                                    (make-x86-register :name name :index idx
                                                       :category cat :size size
                                                       :read-mask rmask :write-mask wmask
                                                       :side side))))
                          table))
                      (defun describe-x86-register (reg-id)
                        (gethash reg-id *x86-register-table*))
                      (defun rename-bea-register (reg-id reg-type reg-size side)
                        (let ((info (describe-x86-register reg-id)))
                          (svref (ecase reg-type
                                   (:general
                                    (ecase reg-size
                                      (64 ,(coerce general-64 'vector))
                                      (32 ,(coerce general-32 'vector))
                                      (16 ,(coerce general-16 'vector))
                                      (8 (ecase side
                                           (:low ,(coerce general-low 'vector))
                                           (:high ,(coerce general-high 'vector))))))
                                   (:mmx ,(mk-vector :mm 8))
                                   (:fpu ,(mk-vector :st 8))
                                   (:sse ,(mk-vector :xmm 16))
                                   (:cr ,(mk-vector :cr 16))
                                   (:dr ,(mk-vector :dr 16))
                                   (:special ,(coerce specials 'vector))
                                   (:mmu ,(coerce mmus 'vector))
                                   (:segment ,(coerce segments 'vector)))
                                 (x86-register-index info))))))))))
  (mkregs ((:rax :eax :ax :al :ah)
           (:rcx :ecx :cx :cl :ch)
           (:rdx :edx :dx :dl :dh)
           (:rbx :ebx :bx :bl :bh)
           (:rsp :esp :sp)
           (:rbp :ebp :bp)
           (:rsi :esi :si)
           (:rdi :edi :di)
           (:r8) (:r9) (:r10) (:r11)
           (:r12) (:r13) (:r14) (:r15))
          (:eflags :mxcsr)
          (:gdtr :ldtr :idtr :tr)
          (:es :cs :ss :ds :fs :gs)))


(defun decode-register-type (arg-type-word size side)
  (let ((id (foreign-enum-keyword 'register-id (logand arg-type-word #xFFFF)))
        (type (foreign-enum-keyword 'register-type
                                    (logand arg-type-word #x0FFF0000))))
    (rename-bea-register id type size side)))

(defparameter *address-size* 32)

(defun decode-bea-argument (argument)
  (with-foreign-slots ((mnemonic arg-type arg-size arg-position access-mode memory segment-reg)
                       argument ArgType)
    (let ((mnemonic (foreign-string-to-lisp mnemonic :max-chars 32)))
      (ecase (foreign-enum-keyword 'arg-type-mode (logand arg-type (lognot #x0FFFFFFF)))
        (:NO_ARGUMENT NIL)
        (:REGISTER_TYPE
         (make-x86-argument-register
          :mnemonic mnemonic :size arg-size :access-write? (eq access-mode :write)
          :id (decode-register-type arg-type arg-size arg-position)
          :high? (and (= arg-size 8) (eq arg-position :high))))
        (:MEMORY_TYPE
         (with-foreign-slots ((base-register index-register scale displacement)
                              memory MemoryType)
           (flet ((no-none (x)
                    (if (eq x :none) nil
                        (rename-bea-register x :general *address-size* nil))))
             (make-x86-argument-memory
              :mnemonic mnemonic :size arg-size :access-write? (eq access-mode :write)
              :base-reg (no-none base-register) :index-reg (no-none index-register)
              :scale scale :displacement displacement
              :segment-reg segment-reg))))
        (:CONSTANT_TYPE
         (make-x86-argument-constant
          :mnemonic mnemonic :size arg-size :access-write? nil
          :relative? (eq (foreign-enum-keyword 'arg-type-mode (logand arg-type #xFFFFFFF))
                         :RELATIVE_MODE)))))))

(def (structure ea) x86-instruction
  (offset 0)
  (address 0)
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

(defun decode-mnemonic (mnemonic)
  (let ((str (foreign-string-to-lisp mnemonic :max-chars 16))
        (table (load-time-value (make-hash-table :test #'equal))))
    (or (gethash str table)
        (setf (gethash str table)
              (read-from-string (concatenate 'string ":" str))))))

(defun bea-instruction-to-lisp (disasm-obj offset address length)
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
        (make-x86-instruction :offset offset
                              :address address
                              :length length
                              :text (foreign-string-to-lisp instr-text :max-chars 64)
                              :category-subset subset :category-function function
                              :opcode opcode
                              :mnemonic (decode-mnemonic mnemonic)
                              :branch-type (if (eq function :control-transfer) branch-type nil)
                              :addr-value addr-value :immediate immediate
                              :implicit-modified-reg
                              (if (logtest implicit-modified-regs
                                           (foreign-enum-value 'arg-type-mode :REGISTER_TYPE))
                                  (decode-register-type implicit-modified-regs *address-size* nil))
                              :flags-accessed (decode-bea-flags flags)
                              :argument1 (decode-bea-argument argument1)
                              :argument2 (decode-bea-argument argument2)
                              :argument3 (decode-bea-argument argument3)
                              :prefixes prefixes :rex-prefix rex-prefix)))))

(def (function e) disassemble-options (&rest flags)
  (reduce #'logior
          (mapcar (lambda (x) (foreign-enum-value 'bea-options x))
                  flags)))

(def (function e) x86-disassemble (byte-vector offset &key (option-flags 0) end real-eip arch (errorp t))
  (declare (type (vector uint8) byte-vector))
  (with-foreign-objects ((disasm-obj 'disasm)
                         (buffer :uint8 16))
    (with-foreign-slots ((eip virtual-addr security-block architecture options)
                         disasm-obj disasm)
      (let* ((end (or end (length byte-vector)))
             (size (min 16 (- end offset)))
             (*address-size* (ecase arch
                               ((nil 32) 32)
                               ((64 16) arch))))
        (assert (> size 0))
        (loop for i from 0 below size and j from offset
           do (setf (mem-ref buffer :uint8 i) (aref byte-vector j)))
        (setf eip buffer
              virtual-addr (or real-eip offset)
              security-block size
              architecture (if (= *address-size* 32) 0 *address-size*)
              options (if (integerp option-flags) option-flags
                          (apply #'disassemble-options option-flags)))
        (let ((rv (disasm disasm-obj)))
          (if (<= rv 0)
              (progn
                (when errorp
                  (error "Disassembly failed: ~A" rv))
                nil)
              (values (bea-instruction-to-lisp disasm-obj offset (or real-eip offset) rv) rv)))))))

(def (function e) disassemble-all (byte-vector &key (start 0) end (base-address 0) option-flags arch errorp)
  (let ((real-end (or end (length byte-vector)))
        (flags (if (integerp option-flags) option-flags
                   (apply #'disassemble-options option-flags)))
        (offset start)
        (bias (- base-address start)))
    (loop while (< offset real-end)
       collect (multiple-value-bind (op delta)
                   (x86-disassemble byte-vector offset
                                    :real-eip (+ bias offset) :end real-end :arch arch
                                    :errorp errorp
                                    :option-flags flags)
                 (if op
                     (incf offset delta)
                     (setf offset real-end))
                 op))))
