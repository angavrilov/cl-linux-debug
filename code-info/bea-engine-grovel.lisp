;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug.code-info)

(cc-flags "-I" #.(aprog1
                     (directory-namestring
                      (merge-pathnames #P"lib/"
                                       (asdf:system-definition-pathname :cl-linux-debug)))
                   (format t "Include path: ~S~%" it)))

(include "beaengine/BeaEngine.h")

(cstruct REX_Struct "REX_Struct"
  (w "W_" :type :uint8) ; all - booleans
  (r "R_" :type :uint8)
  (x "X_" :type :uint8)
  (b "B_" :type :uint8)
  (state "state" :type :uint8))

(constantenum (prefix-use-mode :base-type :uint8)
  ((:invalid "InvalidPrefix"))
  ((:superfluous "SuperfluousPrefix"))
  ((:unused "NotUsedPrefix"))
  ((:mandatory "MandatoryPrefix"))
  ((:used "InUsePrefix")))

(cstruct PrefixInfo "PREFIXINFO"
  (count "Number" :type :int32)
  (count-misused "NbUndefined" :type :int32)
  (lock "LockPrefix" :type prefix-use-mode)
  (operand-size "OperandSize" :type prefix-use-mode)
  (address-size "AddressSize" :type prefix-use-mode)
  (repne "RepnePrefix" :type prefix-use-mode)
  (rep "RepPrefix" :type prefix-use-mode)
  (fs "FSPrefix" :type prefix-use-mode)
  (ss "SSPrefix" :type prefix-use-mode)
  (gs "GSPrefix" :type prefix-use-mode)
  (es "ESPrefix" :type prefix-use-mode)
  (cs "CSPrefix" :type prefix-use-mode)
  (ds "DSPrefix" :type prefix-use-mode)
  (branch-taken "BranchTaken" :type prefix-use-mode)
  (branch-not-taken "BranchNotTaken" :type prefix-use-mode)
  (rex "REX" :type REX_Struct))

(cenum (flag-use-mode :base-type :uint8)
  ((:tested "TE_"))
  ((:modified "MO_"))
  ((:reset "RE_"))
  ((:set "SE_"))
  ((:undefined "UN_"))
  ((:restored "PR_"))
  ((:unused "NotUsedPrefix")))

(cstruct EFLStruct "EFLStruct"
  (of "OF_" :type flag-use-mode)
  (sf "SF_" :type flag-use-mode)
  (zf "ZF_" :type flag-use-mode)
  (af "AF_" :type flag-use-mode)
  (pf "PF_" :type flag-use-mode)
  (cf "CF_" :type flag-use-mode)
  (tf "TF_" :type flag-use-mode)
  (if "IF_" :type flag-use-mode)
  (df "DF_" :type flag-use-mode)
  (nt "NT_" :type flag-use-mode)
  (rf "RF_" :type flag-use-mode))

(cenum (register-id :base-type :int32)
  ((:none "NotUsedPrefix"))
  ((:reg0 "REG0"))
  ((:reg1 "REG1"))
  ((:reg2 "REG2"))
  ((:reg3 "REG3"))
  ((:reg4 "REG4"))
  ((:reg5 "REG5"))
  ((:reg6 "REG6"))
  ((:reg7 "REG7"))
  ((:reg8 "REG8"))
  ((:reg9 "REG9"))
  ((:reg10 "REG10"))
  ((:reg11 "REG11"))
  ((:reg12 "REG12"))
  ((:reg13 "REG13"))
  ((:reg14 "REG14"))
  ((:reg15 "REG15")))

(cstruct MemoryType "MEMORYTYPE"
  (base-register "BaseRegister" :type register-id)
  (index-register "IndexRegister" :type register-id)
  (scale "Scale" :type :int32)
  (displacement "Displacement" :type :int64))

(cenum (register-type :base-type :int32)
  ((:mmx "MMX_REG"))
  ((:general "GENERAL_REG"))
  ((:fpu "FPU_REG"))
  ((:sse "SSE_REG"))
  ((:cr "CR_REG"))
  ((:dr "DR_REG"))
  ((:special "SPECIAL_REG"))
  ((:mmu "MEMORY_MANAGEMENT_REG"))
  ((:segment "SEGMENT_REG")))

(cenum arg-type-mode
  ((:NO_ARGUMENT "NO_ARGUMENT"))
  ((:REGISTER_TYPE "REGISTER_TYPE"))
  ((:MEMORY_TYPE "MEMORY_TYPE"))
  ((:CONSTANT_TYPE "CONSTANT_TYPE"))
  ((:RELATIVE_MODE "RELATIVE_"))
  ((:ABSOLUTE_MODE "ABSOLUTE_")))

(constantenum (arg-position :base-type :int32)
  ((:low "LowPosition"))
  ((:high "HighPosition")))

(cenum (arg-access :base-type :int32)
  ((:none "0"))
  ((:read "READ"))
  ((:write "WRITE")))

(constantenum (segment-reg-id :base-type :uint32)
  ((:es "ESReg"))
  ((:ds "DSReg"))
  ((:fs "FSReg"))
  ((:gs "GSReg"))
  ((:cs "CSReg"))
  ((:ss "SSReg")))

(cstruct ArgType "ARGTYPE"
  (mnemonic "ArgMnemonic" :type :char :count :auto)
  (arg-type "ArgType" :type :int32)
  (arg-size "ArgSize" :type :int32)
  (arg-position "ArgPosition" :type arg-position)
  (access-mode "AccessMode" :type arg-access)
  (memory "Memory" :type MemoryType)
  (segment-reg "SegmentReg" :type segment-reg-id))

(cenum (category-subset :base-type :uint32)
  ((:general "GENERAL_PURPOSE_INSTRUCTION"))
  ((:fpu "FPU_INSTRUCTION"))
  ((:mmx "MMX_INSTRUCTION"))
  ((:sse "SSE_INSTRUCTION"))
  ((:sse2 "SSE2_INSTRUCTION"))
  ((:sse3 "SSE3_INSTRUCTION"))
  ((:ssse3 "SSSE3_INSTRUCTION"))
  ((:sse41 "SSE41_INSTRUCTION"))
  ((:sse42 "SSE42_INSTRUCTION"))
  ((:system "SYSTEM_INSTRUCTION"))
  ((:vm "VM_INSTRUCTION"))
  ((:undocumented "UNDOCUMENTED_INSTRUCTION"))
  ((:amd "AMD_INSTRUCTION"))
  ((:illegal "ILLEGAL_INSTRUCTION"))
  ((:aes "AES_INSTRUCTION"))
  ((:clmul "CLMUL_INSTRUCTION")))

(cenum (category-function :base-type :uint32)
  ((:data-transfer "DATA_TRANSFER"))
  ((:arithmetic "ARITHMETIC_INSTRUCTION"))
  ((:logical "LOGICAL_INSTRUCTION"))
  ((:shift "SHIFT_ROTATE"))
  ((:bit-byte "BIT_UInt8"))
  ((:control-transfer "CONTROL_TRANSFER"))
  ((:string "STRING_INSTRUCTION"))
  ((:in-out "InOutINSTRUCTION"))
  ((:enter-leave "ENTER_LEAVE_INSTRUCTION"))
  ((:flag-control "FLAG_CONTROL_INSTRUCTION"))
  ((:segment-register "SEGMENT_REGISTER"))
  ((:miscellaneous "MISCELLANEOUS_INSTRUCTION"))
  ((:comparison "COMPARISON_INSTRUCTION"))
  ((:logarithmic "LOGARITHMIC_INSTRUCTION"))
  ((:trigonometric "TRIGONOMETRIC_INSTRUCTION"))
  ((:unsupported "UNSUPPORTED_INSTRUCTION"))
  ((:load-constant "LOAD_CONSTANTS"))
  ((:fpu-control "FPUCONTROL"))
  ((:state-management "STATE_MANAGEMENT"))
  ((:conversion "CONVERSION_INSTRUCTION"))
  ((:shuffle-unpack "SHUFFLE_UNPACK"))
  ((:packed-sp "PACKED_SINGLE_PRECISION"))
  ((:simd-128 "SIMD128bits"))
  ((:simd-64 "SIMD64bits"))
  ((:cacheability-control "CACHEABILITY_CONTROL"))
  ((:fp-integer-conversion "FP_INTEGER_CONVERSION"))
  ((:specialized-128 "SPECIALIZED_128bits"))
  ((:simd-fp-packed "SIMD_FP_PACKED"))
  ((:simd-fp-horizontal "SIMD_FP_HORIZONTAL"))
  ((:agent-sync "AGENT_SYNCHRONISATION"))
  ((:packed-align-right "PACKED_ALIGN_RIGHT"))
  ((:packed-sign "PACKED_SIGN"))
  ((:packed-blending "PACKED_BLENDING_INSTRUCTION"))
  ((:packed-test "PACKED_TEST"))
  ((:packed-minmax "PACKED_MINMAX"))
  ((:horizontal-search "HORIZONTAL_SEARCH"))
  ((:packed-equality "PACKED_EQUALITY"))
  ((:streaming-load "STREAMING_LOAD"))
  ((:insertion-extraction "INSERTION_EXTRACTION"))
  ((:dot-product "DOT_PRODUCT"))
  ((:sad "SAD_INSTRUCTION"))
  ((:accelerator "ACCELERATOR_INSTRUCTION"))
  ((:round "ROUND_INSTRUCTION")))

(cenum (branch-type-id :base-type :int32)
  ((:none "NotUsedPrefix"))
  ((:jo "JO"))
  ((:jc "JC"))
  ((:je "JE"))
  ((:ja "JA"))
  ((:js "JS"))
  ((:jp "JP"))
  ((:jl "JL"))
  ((:jg "JG"))
  ((:jb "JB"))
  ((:jecxz "JECXZ"))
  ((:jmp "JmpType"))
  ((:call "CallType"))
  ((:ret "RetType"))
  ((:jno "JNO"))
  ((:jnc "JNC"))
  ((:jne "JNE"))
  ((:jna "JNA"))
  ((:jns "JNS"))
  ((:jnp "JNP"))
  ((:jnl "JNL"))
  ((:jng "JNG"))
  ((:jnb "JNB")))

(cstruct InstrType "INSTRTYPE"
  (category "Category" :type :int32)
  (opcode "Opcode" :type :uint32)
  (mnemonic "Mnemonic" :type :char :count :auto)
  (branch-type "BranchType" :type branch-type-id)
  (flags "Flags" :type EFLStruct)
  (addr-value "AddrValue" :type :uint64)
  (immediate "Immediat" :type :int64)
  (implicit-modified-regs "ImplicitModifiedRegs" :type :uint32))

(cenum bea-options
  ((:NO-TABULATION "NoTabulation"))
  ((:TABULATION "Tabulation"))
  ((:MASM-SYNTAX "MasmSyntax"))
  ((:GO-ASM-SYNTAX "GoAsmSyntax"))
  ((:NASM-SYNTAX "NasmSyntax"))
  ((:AT-SYNTAX "ATSyntax"))
  ((:PREFIXED-NUMERAL "PrefixedNumeral"))
  ((:SUFFIXED-NUMERAL "SuffixedNumeral"))
  ((:SHOW-SEGMENT-REGS "ShowSegmentRegs")))

(cstruct Disasm "DISASM"
  (eip "EIP" :type :pointer)
  (virtual-addr "VirtualAddr" :type :uint64)
  (security-block "SecurityBlock" :type :uint32)
  (instr-text "CompleteInstr" :type :char :count :auto)
  (architecture "Archi" :type :uint32)
  (options "Options" :type :uint64)
  (instruction "Instruction" :type InstrType)
  (argument1 "Argument1" :type ArgType)
  (argument2 "Argument2" :type ArgType)
  (argument3 "Argument3" :type ArgType)
  (prefix "Prefix" :type PrefixInfo))

