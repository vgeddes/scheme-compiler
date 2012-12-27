(declare (unit spec-x86-64)
         (uses nodes machine))

(include "arch-syntax")

(use matchable)
(use srfi-1)

;; Instruction Definitions

;; operand flags
;;
;;

;; operand types
;; i8     8-bit immediate
;; i32    32-bit immediate
;; i64    64-bit immediate
;; disp32 32-bit displacement
;; reg    64-bit register

;; operand flags
;; in   register is read
;; out  register is modified

(define-arch-registers x86-64 (rsp rbp rsi rdi rax rbx rcx rdx r8 r9 r10 r11 r12 r13 r14 r15))

(define-arch-instructions x86-64

 ;; ret

  (retnear
   ()
   "ret")

  ;; push

  (push.r
   ((reg in))
   "push  $1")

  ;; pop

  (pop.r
   ((reg out))
   "pop   $1")

  ;; lea

  (lea.mr
   ((reg in) (reg out))
   "lea   $2, [$1]")

  (lea.dr
   ((disp32) (reg out))
   "lea   $2, [rip + $1]")

  (lea.mdr
   ((reg in) (disp32) (reg out))
   "lea   $3, [$1 + $2]")

  ;; call

  (call.d
   ((disp32))
   "call  [rip + $1]")

  (call.r
   ((reg in))
   "call  [$1]")

  ;; add

  (add.rr
   ((reg in) (reg in out))
   "add   $2, $1")

  (add.i8r
   ((i8) (reg in out))
   "add   $2, 1$")

  (add.i32r
   ((i32) (reg in out))
   "add   $2, $1")

  ;; sub

  (sub.rr
   ((reg in) (reg in out))
   "sub   $2, $1")

  (sub.i8r
   ((i8) (reg in out))
   "sub   $2, $1")

  (sub.i32r
   ((i32) (reg in out))
   "sub   $2, $1")

  ;; and

  (and.rr
   ((reg in) (reg in out))
   "and   $2, $1")

  (and.i8r
   ((i8) (reg in out))
   "and   $2, $1")

  (and.i32r
   ((i32) (reg in out))
   "and   $2, $1")

  ;; or

  (ior.rr
   ((reg in) (reg in out))
   "or    $2, $1")

  (ior.i8r
   ((i8) (reg in out))
   "or    $2, $1")

  (ior.i32r
   ((i32) (reg in out))
   "or    $2, $1")

  ;; xor

  (xor.rr
   ((reg in) (reg in out))
   "xor   $2, $1")

  (xor.i32r
   ((i32) (reg in out))
   "xor   $2, $1")

  ;; shr

  (shr.i8r
   ((i8) (reg in out))
   "shr   $2, $1")

  ;; shl

  (shl.i8r
   ((i8) (reg in out))
   "shl   $2, $1")

  ;; mov

  (mov.rr
   ((reg in) (reg out))
   "mov   $2, $1")

  (mov.rd
   ((reg in) (disp32))
   "mov   [rip + $2], $1")

  (mov.rm
   ((reg in) (reg in))
   "mov   [$2], $1")

  (mov.rmd
   ((reg in) (reg in) (disp32))
   "mov   [$2 + $3], $1")

  (mov.dr
   ((disp32) (reg out))
   "mov   $2, [rip + $1]")

  (mov.mr
   ((reg in) (reg out))
   "mov   $2, [$1]")

  (mov.mdr
   ((reg in) (disp32) (reg out))
   "mov   $3, [$1 + $2]")

  (mov.i8r
   ((i8) (reg out))
   "mov   $2, $1")

  (mov.i32r
   ((i32) (reg out))
   "mov   $2, $1")

  (mov.i64r
   ((i64) (reg out))
   "mov   $2, $1")

  ;; setCC

  (sete.r
   ((reg out))
   "sete  $1")

  (setne.r
   ((reg out))
   "setne $1")

  (setl.r
   ((reg out))
   "setl  $1")

  (setle.r
   ((reg out))
   "setle $1")

  (setg.r
   ((reg out))
   "setg  $1")

  (setge.r
   ((reg out))
   "setge $1")

  ;; cmp

  (cmp.rr
   ((reg in) (reg in))
   "cmp   $2, $1")

  (cmp.i8r
   ((i8) (reg in))
   "cmp   $2, $1")

  (cmp.i32r
   ((i32) (reg in))
   "cmp   $2, $1")

  ;; jmp

  (jmp.d
   ((disp32))
   "jmp   [rip + $1]")

  (jmp.r
   ((reg in))
   "jmp   $1")

  (jmp.m
   ((reg in))
   "jmp   [$1]")

  (jmp.md
   ((reg in) (disp32))
   "jmp   [$1 + $2]")


  ;; jCC

  (jo.d
   ((disp32))
   "jo    [rip + $1]")

  (jno.d
   ((disp32))
   "jno   [rip + $1]")

  (je.d
   ((disp32))
   "je    [rip + $1]")

  (jne.d
   ((disp32))
   "jne   [rip + $1]")

  (jl.d
   ((disp32))
   "jl    [rip + $1]")

  (jle.d
   ((disp32))
   "jle   [rip + $1]")

  (jge.d
   ((disp32))
   "jge   [rip + $1]")

  (jg.d
   ((disp32))
   "jg    [rip + $1]"))
