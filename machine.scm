
(declare (unit machine)
         (uses nodes))

(use matchable)
(use srfi-1)

(include "machine-syntax")
(include "struct-syntax")
(include "x86-64")

;; formatting

(define (machine-module-print mod port)
  (machine-context-for-each
     (lambda (context)
       (machine-context-print context port))
     mod))

(define (machine-context-print context port)
  (struct-case context
    ((machine-context name args entry)
     (fprintf port "# context name=~a args=~a\n" name (map (lambda (arg) (machine-operand-format arg)) args))
     (machine-block-for-each
        (lambda (block)
            (machine-block-print block port))
        context))))

(define (machine-block-print block port)
  (struct-case block
    ((machine-block name head tail succ)
     ;; print label
     (fprintf port "~a:\n" name)
     ;; print code
     (machine-instr-for-each
        (lambda (instr)
           (machine-instr-print instr port))
         block)
     (fprintf port "\n"))))

(define (machine-instr-print instr port)
  (fprintf port "  ")
  (fprintf port
    (apply format
           (cons
            (machine-descriptor-format (machine-instr-descriptor instr))
            (map machine-operand-format (machine-instr-ops instr)))))
  (fprintf port "\n"))

(define (machine-operand-format op)
  (match op
   ;; machine address
   (($ machine-addr-x86-64 #f disp)
     (format "~s(%rip)" disp))

   (($ machine-addr-x86-64 ($ machine-vreg name) #f)
     (format "(%~s)" name))

   (($ machine-addr-x86-64 ($ machine-vreg name) disp)
     (format "~s(%~s)" disp name))
   
   ;; machine constant
   (($ machine-imm size value)
    (format "~s" value))

   ;; machine vreg
   (($ machine-vreg name)
    (format "%~s" name))

   (_ (pretty-print op) (error "no matching pattern")))) 


;; iteration

(define (machine-context-for-each f mod)
  (for-each f (machine-module-contexts mod)))


(define (machine-block-for-each f context)
  (define (visit-block block f)
    (let ((succ (machine-block-successors block)))
      (f block)
      (for-each (lambda (succ)
                  (visit-block succ f))
                succ)))
    (visit-block (machine-context-start context) f))


(define (machine-instr-for-each f block)
  (let ((head (machine-block-head block)))
    (let walk ((x head))
      (cond
       ((not (null? x))
        (f x)
        (walk (machine-instr-next x)))))))



(define (machine-block-append-instr! block instr)
  (cond
    ((and (null? (machine-block-head block))
          (null? (machine-block-tail block)))
     (machine-block-head-set! block instr)
     (machine-block-tail-set! block instr))
    (else
     (let ((tail (machine-block-tail block)))
       (machine-instr-prev-set! instr tail)
       (machine-instr-next-set! tail instr)
       (machine-block-tail-set! block instr))))
  block)

;; utils
(define (machine-block-append instr*)
  (cond
    ((null? instr) '())
    ((= (length instr*) 1)
     (car instr*))
    (else
     (let f ((prev (first instr*))
             (next (second instr*)) 
             (rest (cddr instr*))) 
        (machine-instr-set-next! prev next)
        (machine-instr-set-prev! next prev)
        (if (null? rest)
            (car instr*) 
            (f next (car rest) (cdr rest))))))) 



