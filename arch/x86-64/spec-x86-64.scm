(declare (unit spec-x86-64)
         (uses nodes))

(include "arch-syntax")

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

;;(registers (rsp rbp rsi rdi rax rbx rcx rdx r8 r9 r10 r11 r12 r13 r14 r15))

(define-arch-instructions x86-64

 ;; ret
  
  (retnear
   ()
   "ret")

  ;; push

  (push.r
   ((reg in))
   "pushq    $1")

  ;; pop

  (pop.r
   ((reg out))
   "popq    $2")

  ;; lea

  (lea.mr
   ((reg in) (reg out))
   "leaq    $1, $2")

  (lea.dr
   ((disp32 in) (reg out))
   "leaq    $1(%rip), $2")

  (lea.mdr
   ((reg in) (disp32 in) (reg out))
   "leaq    $2($1), $3")

  ;; call

  (call.d
   ((disp32))
   "call    $1(%rip)")

  ;; add

  (add.rr
   ((reg in) (reg in out))
   "addq    $1, $2")

  (add.i8r
   ((i8) (reg in out))
   "addq    $1, $2")

  (add.i32r
   ((i32) (reg in out))
   "addq    $1, $2")

  ;; sub

  (sub.rr
   ((reg in) (reg in out))
   "subq    $1, $2")

  (sub.i8r
   ((i8) (reg in out))
   "subq    $1, $2")

  (sub.i32r
   ((i32) (reg in out))
   "subq    $1, $2")

  ;; and

  (and.rr
   ((reg in) (reg in out))
   "andq    $1, $2")

  (and.i8r
   ((i8) (reg in out)) 
   "andq    $1, $2")

  (and.i32r
   ((i32) (reg in out))
   "andq    $1, $2")
  
  ;; or

  (ior.rr
   ((reg in) (reg in out))
   "orq     $1, $2")

  (ior.i8r
   ((i8) (reg in out))
   "orq     $1, $2")

  (ior.i32r
   ((i32) (reg in out))
   "orq     $1, $2")

  ;; xor

  (xor.rr
   ((reg in) (reg in out))
   "xorq    $1, $2")

  (xor.i32r
   ((i32) (reg in out))
   "xorq    $1, $2")

  ;; shr

  (shr.i8r
   ((i8) (reg in out))
   "shrq    $1, $2")

  ;; shl

  (shl.i8r
   ((i8) (reg in out))
   "shlq    $1, $2")
  
  ;; mov

  (mov.rr
   ((reg in) (reg out))
   "movq    $1, $2")

  (mov.rd
   ((reg in) (disp32))
   "movq    $1, $2(%rip)")

  (mov.rm
   ((reg in) (reg in))
   "movq    $1, ($2)")

  (mov.rmd
   ((reg in) (reg in) (disp32))
   "movq    $1, $3($2)")

  (mov.dr
   ((disp32) (reg out))
   "movq    $1(%rip), $2")

  (mov.mr
   ((reg in) (reg out))
   "movq    ($1), $2")

  (mov.mdr
   ((reg in) (disp32) (reg out))
   "movq    $2($1), $3")

  (mov.i8r
   ((i8) (reg out))
   "movq    $1, $2")

  (mov.i32r
   ((i32) (reg out))
   "movq    $1, $2")

  (mov.i64r
   ((i64) (reg out))
   "movq    $1, $2")

  ;; setCC

  (sete.r
   ((reg out))
   "sete    $1")

  (setne.r
   ((reg out))
   "setne   $1")

  (setl.r
   ((reg out))
   "setl    $1")

  (setle.r
   ((reg out))
   "setle   $1")

  (setg.r
   ((reg out))
   "setg    $1")

  (setge.r
   ((reg out))
   "setge   $1")

  ;; cmp

  (cmp.rr
   ((reg in) (reg in))
   "cmpq    $1, $2")

  (cmp.i8r
   ((i8) (reg in))
   "cmpq    $1, $2")

  (cmp.i32r
   ((i32) (reg in))
   "cmpq    $1, $2")

  ;; jmp

  (jmp.d
   ((disp32))
   "jmp     $1(%rip)")
  
  (jmp.r
   ((reg in))
   "jmp     $1")

  (jmp.m
   ((reg in))
   "jmp     ($1)")

  (jmp.md
   ((reg in) (disp32))
   "jmp     $2($1)")


  ;; jCC

  (jo.d
   ((disp32))
   "jo      $1(%rip)")

  (jno.d
   ((disp32))
   "jno     $1(%rip)")

  (je.d
   ((disp32))
   "je      $1(%rip)")

  (jne.d
   ((disp32))
   "jne     $1(%rip)")

  (jl.d
   ((disp32))
   "jl      $1(%rip)")

  (jle.d
   ((disp32))
   "jle     $1(%rip)")

  (jge.d
   ((disp32))
   "jge     $1(%rip)")

  (jg.d
   ((disp32))
   "jg     $1(%rip)"))

