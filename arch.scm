(declare (unit arch)
         (uses nodes arch-x86-64))


;; The default selected architecture
(define *arch* <arch-x86-64>)

;; Get all vregs that are read in this instr
(define (arch-vregs-read instr)
  ((arch-descriptor-vregs-read *arch*) instr))

;; Get all vregs that are written to in this instr
(define (arch-vregs-written instr)
  ((arch-descriptor-vregs-written *arch*) instr))

;; Generate a function to act as a bridge between the C runtime and the compiled Scheme.
;;
(define (arch-generate-bridge-context mod)
  ((arch-descriptor-generate-bridge-context *arch*) mod))

;; Format an operand
;;
(define (arch-operand-format op)
  ((arch-descriptor-operand-format *arch*) op))

;; Instruction selection
;;
(define (arch-emit-statement mc-blk tree)
  ((arch-descriptor-emit-statement *arch*) mc-blk tree))

(define (arch-make-context name params mod)
  ((arch-descriptor-make-context *arch*) name params mod))



