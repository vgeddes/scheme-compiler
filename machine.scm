
(declare (unit arch)
         (uses nodes))

(use matchable)
(use srfi-1)

(include "arch-syntax")
(include "x86-64")


(define mem
  (lambda operands
    (match operands
      ((base disp)
       (make-x86-memref base disp)))))

(define (format-operand op)
  (match op

   ((? boolean? op)
     (format "~s" op))
   
   ;; $immediate
   ((? integer? op)
    (format "$~s" op))

   ;; %register
   ((? symbol? op)
    (format "%~s" op))

   ;; disp(%base)
   (($ x86-memref base disp)
    (format "~s(%~s)" disp base))

   ;; (%base)
   (($ x86-memref base #f)
    (format "(%~s)" base))

   ;; pc-relative jump
   (('label label)
    (format "~s(%rip)" label))))

(define (format-instr instr)
  (apply format
         (cons
          (instr-descriptor-format (instr-descriptor instr))
          (map format-operand (instr-operands instr)))))

(define (operand-spec-flags operand-spec)
  (cdr operand-spec))

(define (operand-spec-type operand-spec)
  (car operand-spec))

;; order of operands not preserved
(define (filter-operands flag operand-specs operands)
  (fold (lambda (operand operand-spec x)
          (cond 
            ((memq flag (operand-spec-flags operand-spec))
             (match operand 
               (($ x86-memref (? symbol? reg) disp)
                (cons reg x))
               ((? symbol? reg)
                (cons reg x))
               (_ x)))
             (else x)))
        '()
        operands
        operand-specs))

(define (instr-temp-uses instr))

(define (instr-temp-defs instr))
 

(define (make-instr-with-descriptor descriptor operands)
  (let ((use-list (filter-operands 'in  (instr-descriptor-operand-spec descriptor) operands))
        (def-list (filter-operands 'out (instr-descriptor-operand-spec descriptor) operands)))
    (make-instr descriptor operands use-list def-list '())))

