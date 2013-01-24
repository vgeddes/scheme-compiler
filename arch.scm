(declare (unit arch)
         (uses globals))

(module arch *

  (import scheme)
  (import chicken)

  (import globals)

(define (arch-init)
  ((arch-impl-init *arch*)))

;; Get all vregs that are read in this instr
(define (arch-vreg-uses instr)
  ((arch-impl-vreg-uses *arch*) instr))

;; Get all vregs that are written to in this instr
(define (arch-vreg-defs instr)
  ((arch-impl-vreg-defs *arch*) instr))

;; Generate a function to act as a bridge between the C runtime and the compiled Scheme.
;;
(define (arch-generate-bridge-context mod)
  ((arch-impl-generate-bridge-context *arch*) mod))

;; hooks for register allocation
(define (arch-rewrite cxt asn spd)
  ((arch-impl-rewrite *arch*) cxt asn spd))

(define (arch-assign-stack-slot cxt vreg)
  ((arch-impl-assign-stack-slot *arch*) cxt vreg))

;; Format an operand
;;
(define (arch-operand-format op)
  ((arch-impl-operand-format *arch*) op))

(define (arch-print-frame-info cxt port)
  ((arch-impl-print-frame-info *arch*) cxt port))


;; Instruction selection
;;
(define (arch-emit-statement cxt blk vreg-ref tree-stm)
  ((arch-impl-emit-statement *arch*) cxt blk vreg-ref tree-stm))

(define (arch-make-context name params mod)
  ((arch-impl-make-context *arch*) name params mod))


)
