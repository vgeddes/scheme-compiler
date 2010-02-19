
(declare (unit arch)
         (uses nodes))

(use matchable)
(use srfi-1)

(include "arch-syntax")
(include "x86-64")

(define (format-operand op)
  (match op

   ;; $immediate
   ((? integer? op)
    (format "$~s" op))

   ;; %register
   ((? symbol? op)
    (format "%~s" op))

   ;; disp(%base)
   (($ x86-memref base-reg disp #f #f)
    (format "~s(%~s)" disp base-reg))

   ;; mov (%base, %offset, scale) %reg
   (($ x86-memref base-reg #f offset-reg scale)
    (format "(%~s,%~s,~s)" base-reg offset-reg scale))

   ;; mov disp(%base, %offset, scale) %reg
   (($ x86-memref base-reg disp offset-reg scale)
    (format "~s(%~s,%~s,~s)" disp base-reg offset-reg scale))

   (('L label)
    (format "~s" label))))

(define (format-instr instr)
  (apply format
         (cons
          (instr-descriptor-format (instr-descriptor instr))
          (map format-operand (instr-operands instr)))))

(define (operand-spec-flags operand-spec)
  (cdr operand-spec))

(define (operand-spec-type operand-spec)
  (cdr operand-spec))

;; order of operands not preserved
(define (filter-operands-by-flag flag operand-specs operands)
  (fold (lambda (operand operand-spec x)
          (if (memq flag (operand-spec-flags operand-spec))
              (cons operand x)
              x))
        '()
        operands
        operand-specs))


(define (make-instr-with-descriptor descriptor operands)
  (let ((use-list (filter-operands-by-flag 'in  (instr-descriptor-operand-spec descriptor) operands))
        (def-list (filter-operands-by-flag 'out (instr-descriptor-operand-spec descriptor) operands)))
    (make-instr descriptor operands use-list def-list)))
