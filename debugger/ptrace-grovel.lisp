;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-

(in-package :cl-linux-debug)

(include "signal.h")
(include "unistd.h")
(include "sys/ptrace.h")
(include "sys/syscall.h")
(include "sys/types.h")
(include "sys/user.h")
(include "sys/mman.h")
(include "sys/wait.h")

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
(constant (WEXITED "WEXITED"))
(constant (WSTOPPED "WSTOPPED"))
(constant (__WALL "__WALL"))

(constant (SIGTRAP "SIGTRAP"))
(constant (SIGSTOP "SIGSTOP"))
(constant (SIGCONT "SIGCONT"))

(constant (SYS_tgkill "SYS_tgkill"))

(constant (PROT_READ "PROT_READ"))
(constant (PROT_WRITE "PROT_WRITE"))
(constant (PROT_EXEC "PROT_EXEC"))

(constant (MAP_PRIVATE "MAP_PRIVATE"))
(constant (MAP_ANONYMOUS "MAP_ANONYMOUS"))

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
       ((:PTRACE_POKEDATA "PTRACE_POKEDATA"))
       ((:PTRACE_SETOPTIONS "PTRACE_SETOPTIONS"))
       ((:PTRACE_GETEVENTMSG "PTRACE_GETEVENTMSG")))

(cenum ptrace-options
       ((:PTRACE_O_TRACEFORK "PTRACE_O_TRACEFORK"))
       ((:PTRACE_O_TRACEVFORK "PTRACE_O_TRACEVFORK"))
       ((:PTRACE_O_TRACECLONE "PTRACE_O_TRACECLONE"))
       ((:PTRACE_O_TRACEEXEC "PTRACE_O_TRACEEXEC")))

(cenum ptrace-event
       ((:PTRACE_EVENT_FORK "PTRACE_EVENT_FORK"))
       ((:PTRACE_EVENT_VFORK "PTRACE_EVENT_VFORK"))
       ((:PTRACE_EVENT_CLONE "PTRACE_EVENT_CLONE"))
       ((:PTRACE_EVENT_EXEC "PTRACE_EVENT_EXEC")))

(constantenum errno-vals
              ((:EBUSY "EBUSY"))
              ((:EFAULT "EFAULT"))
              ((:EINVAL "EINVAL"))
              ((:EIO "EIO"))
              ((:EPERM "EPERM"))
              ((:ESRCH "ESRCH")))

#-x86-64
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
         (eflags "eflags" :type :long)
         (cs "xcs" :type :long)
         (ds "xds" :type :long)
         (ss "xss" :type :long)
         (es "xes" :type :long)
         (fs "xfs" :type :long)
         (gs "xgs" :type :long))

#+x86-64
(cstruct user_regs_struct "struct user_regs_struct"
         (r15 "r15" :type :long)
         (r14 "r14" :type :long)
         (r13 "r13" :type :long)
         (r12 "r12" :type :long)
         (rbp "rbp" :type :long)
         (rbx "rbx" :type :long)
         (r11 "r11" :type :long)
         (r10 "r10" :type :long)
         (r9 "r9" :type :long)
         (r8 "r8" :type :long)
         (rax "rax" :type :long)
         (rcx "rcx" :type :long)
         (rdx "rdx" :type :long)
         (rsi "rsi" :type :long)
         (rdi "rdi" :type :long)
         (orig-rax "orig_rax" :type :long)
         (rip "rip" :type :long)
         (cs "cs" :type :long)
         (eflags "eflags" :type :long)
         (rsp "rsp" :type :long)
         (ss "ss" :type :long)
         (fs_base "fs_base" :type :long)
         (gs_base "gs_base" :type :long)
         (ds "ds" :type :long)
         (es "es" :type :long)
         (fs "fs" :type :long)
         (gs "gs" :type :long))

