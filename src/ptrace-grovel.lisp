;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug)

(include "signal.h")
(include "unistd.h")
(include "sys/ptrace.h")
(include "sys/syscall.h")
(include "sys/types.h")
(include "sys/user.h")

;; Signals

(cstruct siginfo_t "struct siginfo"
         (si_pid "si_pid" :type :int)
         (si_status "si_status" :type :int)
         (si_code "si_code" :type :int))

(constant (CLD_EXITED "CLD_EXITED"))
(constant (CLD_KILLED "CLD_KILLED"))
(constant (CLD_DUMPED "CLD_DUMPED"))
(constant (CLD_TRAPPED "CLD_TRAPPED"))
(constant (CLD_STOPPED "CLD_STOPPED"))
(constant (CLD_CONTINUED "CLD_CONTINUED"))

(constant (WNOHANG "WNOHANG"))

(constant (SIGTRAP "SIGTRAP"))
(constant (SIGSTOP "SIGSTOP"))
(constant (SIGCONT "SIGCONT"))

(constant (SYS_tgkill "SYS_tgkill"))

;; Ptrace

(ctype pid_t "pid_t")

(cenum ptrace-request
       ((:PTRACE_TRACEME "PTRACE_TRACEME"))
       ((:PTRACE_ATTACH "PTRACE_ATTACH"))
       ((:PTRACE_DETACH "PTRACE_DETACH"))
       ((:PTRACE_CONT "PTRACE_CONT"))
       ((:PTRACE_KILL "PTRACE_KILL"))
       ((:PTRACE_GETREGS "PTRACE_GETREGS"))
       ((:PTRACE_SETREGS "PTRACE_SETREGS"))
       ((:PTRACE_GETSIGINFO "PTRACE_GETSIGINFO"))
       ((:PTRACE_PEEKTEXT "PTRACE_PEEKTEXT"))
       ((:PTRACE_PEEKDATA "PTRACE_PEEKDATA"))
       ((:PTRACE_POKETEXT "PTRACE_POKETEXT"))
       ((:PTRACE_POKEDATA "PTRACE_POKEDATA")))

(constantenum errno-vals
              ((:EBUSY "EBUSY"))
              ((:EFAULT "EFAULT"))
              ((:EINVAL "EINVAL"))
              ((:EIO "EIO"))
              ((:EPERM "EPERM"))
              ((:ESRCH "ESRCH")))

(cstruct user_regs_struct "struct user_regs_struct"
         (eax "eax" :type :long)
         (ebx "ebx" :type :long)
         (ecx "ecx" :type :long)
         (edx "edx" :type :long)
         (esi "esi" :type :long)
         (edi "edi" :type :long)
         (esp "esp" :type :long)
         (ebp "ebp" :type :long)
         (eip "eip" :type :long)
         (orig-eax "orig_eax" :type :long)
         (eflags "eflags" :type :long))

