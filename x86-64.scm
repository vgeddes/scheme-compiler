
;; Instruction Definitions

;; operand flags
;; 
;;

;; operand types
;; i8   8-bit immediate
;; i32  32-bit immediate
;; i64  64-bit immediate
;; m64  64-bit memory reference (using [base + disp] addressing)
;; r8   8-bit register
;; r64  64-bit register

;; operand flags
;; in   operand read during instruction execution
;; out  operand written to during instruction execution

(define-machine-instructions x86-64

 ;; ret
  
  (retnear
   ()
   "ret")

  ;; push

  (push64r
   ((r64 in))
   "pushq\t~a")

  ;; pop

  (pop64r
   ((r64 out))
   "popq\t~a")

  ;; lea

  (lea64mr
   ((m64 in) (r64 out))
   "leaq\t~a, ~a")

  ;; call

  (callrel32
   ((i32))
   "call\t~a")

  ;; add

  (add64rr
   ((r64 in) (r64 in out))
   "addq\t~a, ~a")

  (add64i8r
   ((i8) (r64 in out))
   "addq\t~a, ~a")

  (add64i32r
   ((i32) (r64 in out))
   "addq\t~a, ~a")

  ;; sub

  (sub64rr
   ((r64 in) (r64 in out))
   "subq\t~a, ~a")

  (sub64i8r
   ((i8) (r64 in out))
   "subq\t~a, ~a")

  (sub64i32r
   ((i32) (r64 in out))
   "subq\t~a, ~a")

  ;; and

  (and64rr
   ((r64 in) (r64 in out))
   "andq\t~a, ~a")

  (and64i8r
   ((i8) (r64 in out)) 
   "andq\t~a, ~a")

  (and64i32r
   ((i32) (r64 in out))
   "andq\t~a, ~a")
  
  ;; or

  (or64rr
   ((r64 in) (r64 in out))
   "orq\t~a, ~a")

  (or64i8r
   ((i8) (r64 in out))
   "orq\t~a, ~a")

  (or64i32r
   ((i32) (r64 in out))
   "orq\t~a, ~a")

  ;; xor

  (xor64rr
   ((r64 in) (r64 in out))
   "xorq\t~a, ~a")

  (xor64i32r
   ((i32) (r64 in out))
   "xorq\t~a, ~a")

  ;; shr

  (shr64i8r
   ((i8) (r64 in out))
   "shrq\t~a, ~a")

  ;; shl

  (shl64i8r
   ((i8) (r64 in out))
   "shlq\t~a, ~a")
  
  ;; mov

  (mov64rr
   ((r64 in) (r64 out))
   "movq\t~a, ~a")

  (mov64rm
   ((r64 in) (m64))
   "movq\t~a, ~a")

  (mov64mr
   ((m64) (r64 out))
   "movq\t~a, ~a")

  (mov64i8r
   ((i8) (r64 out))
   "movq\t~a, ~a")

  (mov64i32r
   ((i32) (r64 out))
   "movq\t~a, ~a")

  (mov64i64r
   ((i64) (r64 out))
   "movq\t~a, ~a")

  ;; setCC

  (sete8r
   ((r8 out))
   "sete\t~a")

  (setne8r
   ((r8 out))
   "setne\t~a")

  (setl8r
   ((r8 out))
   "setl\t~a")

  (setle8r
   ((r8 out))
   "setle\t~a")

  (setg8r
   ((r8 out))
   "setg\t~a")

  (setge8r
   ((r8 out))
   "setge\t~a")

  ;; cmp

  (cmp64rr
   ((r64 in) (r64 in))
   "cmpq\t~a, ~a")

  (cmp64i8r
   ((i8) (r64 in))
   "cmpq\t~a, ~a")

  (cmp64i32r
   ((i32) (r64 in))
   "cmpq\t~a, ~a")

  ;; jmp

  (jmp64rel32
   ((i32))
   "jmp\t~a")
  
  (jmp64r
   ((r64 in))
   "jmp\t~a")

  ;; jCC

  (jo32rel32
   ((i32))
   "jo\t~a")

  (jno32rel32
   ((i32))
   "jno\t~a")

  (je32rel32
   ((i32))
   "je\t~a")

  (jne32rel32
   ((i32))
   "jne\t~a")

  (jl32rel32
   ((i32))
   "jl\t~a")

  (jle32rel32
   ((i32))
   "jle\t~a")

  (jge32rel32
   ((i32))
   "jge\t~a")

  (jg32rel32
   ((i32))
   "jg\t~a"))
