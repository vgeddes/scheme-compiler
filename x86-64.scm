

(declare (unit x86-64))

(use matchable)
(use srfi-1)

(include "class-syntax")
(include "munch-syntax")

(define (make-use-fold-proc op-spec)
  (lambda (instr fun init)  
    (fold (lambda (spec operand acc)
            (case spec
              ((u du)
               (fun operand acc))
              ((m)
               (fun (second operand) acc))
              (else acc)))
          init
          op-spec
          (instr-operands instr))))

(define (make-def-fold-proc op-spec)
  (lambda (instr fun init)  
    (fold (lambda (spec operand acc)
            (case spec
              ((d du)
               (fun operand acc))
              (else acc)))
          init
          op-spec
          (instr-operands instr))))

(define (format-operand op)
  (match op
   ((? integer? op)
    (format "$~s" op))
   ((? symbol? op)
    (format "%~s" op))
   (('M base offset)
    (format "~s(%~s)" offset base))
   (('L label)
    (format "$~s" label))))

(define (format-instr instr)
  (apply format
         (cons
          (instr-descriptor-format (instr-descriptor instr))
          (map format-operand (instr-operands instr)))))
  

;; Instruction Definitions

;; operand specs
;; d:  variable definition         (register)
;; u:  variable use                (register)
;; ud: variable use and definition (register)
;; m: memory reference             (register + offset)
;; i: immediate                    (i8, i32 or i64)   

(instruction-info x86-64

  (call32rel32
   (i)
   "call ~a")

  ;; add

  (add64rr
   (u du)
   "addq ~a, ~a")

  (add64i8r
   (i du)
   "addq ~a, ~a")

  (add64i32r
   (i du)
   "addq ~a, ~a")

  ;; sub

  (sub64rr
   (u du)
   "subq ~a, ~a")

  (sub64i8r
   (i du)
   "subq ~a, ~a")

  (sub64i32r
   (i du)
   "subq ~a, ~a")

  ;; and

  (and64rr
   (u du)
   "andq ~a, ~a")

  (and64i8r
   (i du)
   "andq ~a, ~a")

  (and64i32r
   (i du)
   "andq ~a, ~a")
  
  ;; or

  (or64rr
   (u du)
   "orq ~a, ~a")

  (or64i8r
   (i du)
   "orq ~a, ~a")

  (or64i32r
   (i du)
   "orq ~a, ~a")

  ;; xor

  (xor64rr
   (u du)
   "xorq ~a, ~a")

  (xor64i32r
   (i du)
   "xorq ~a, ~a")

  ;; shr

  (shr64i8r
   (i du)
   "shrq ~a, ~a")

  ;; shl

  (shl64i8r
   (i du)
   "shlq ~a, ~a")
  
  ;; mov

  (mov64rr
   (u d)
   "movq ~a, ~a")

  (mov64rm
   (u m)
   "movq ~a, ~a")

  (mov64mr
   (m d)
   "movq ~a, ~a")

  (mov64i8r
   (i d)
   "movq ~a, ~a")

  (mov64i32r
   (i d)
   "movq ~a, ~a")

  ;; setCC

  (sete8r
   (d)
   "sete ~a")

  (setne8r
   (d)
   "setne ~a")

  (setl8r
   (d)
   "setl ~a")

  (setle8r
   (d)
   "setle ~a")

  (setg8r
   (d)
   "setg ~a")

  (setge8r
   (d)
   "setge ~a")

  ;; cmp

  (cmp64rr
   (u u)
   "cmpq ~a, ~a")

  (cmp64i8r
   (u u)
   "cmpq ~a, ~a")

  (cmp64i32r
   (u u)
   "cmpq ~a, ~a")

  ;; jmp

  (jmp64rel32
   (i)
   "jmp ~a")
  
  (jmp64r
   (u)
   "jmp ~a")

  ;; jCC

  (je32rel32
   (i)
   "je ~a")

  (jne32rel32
   (i)
   "jne ~a")

  (jl32rel32
   (i)
   "jl ~a")

  (jle32rel32
   (i)
   "jle ~a")

  (jge32rel32
   (i)
   "jge ~a")

  (jg32rel32
   (i)
   "jg ~a"))
