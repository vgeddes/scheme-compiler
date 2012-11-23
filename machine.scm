
(declare (unit machine)
         (uses nodes))

(use matchable)
(use srfi-1)

(include "struct-syntax")

;; aggregate structures 
(define-struct mc-module   (contexts))
(define-struct mc-context  (name args start vregs))
(define-struct mc-block    (name head tail succ cxt))

;; instructions
(define-struct mc-spec       (name fmt fmt-indices verifiers reads writes))
(define-struct mc-instr      (spec ops next prev implicit-uses sp-load sp-store number block data))
 
;; operands 

(define-struct mc-vreg         (name hreg pref users data))

(define-struct mc-imm          (size value))
(define-struct mc-disp         (value))

;; Constructors

(define (mc-make-module)
  (make-mc-module '()))

(define (mc-make-context name params mod)
  (arch-make-context name params mod))

(define (mc-make-block cxt name)
  (make-mc-block name '() '() '() cxt))

(define (mc-make-instr blk spec implicit-uses operands)
  (let ((instr (make-mc-instr spec operands '() '() implicit-uses '() '()  #f blk '())))
    (for-each (lambda (op)
                (cond
                   ((mc-vreg? op)
                    (mc-vreg-add-user op instr))))
              operands)
    (and blk (mc-block-append blk instr))
    instr))

(define mc-make-vreg
  (lambda operands
    (match operands
       ((name)
        (make-mc-vreg name #f #f '() '()))
       ((name hreg pref)
        (make-mc-vreg name hreg pref '() '()))
       (else (assert-not-reached)))))
          

;; Operand Protocol

(define (mc-operand-equal? o1 o2)
 (or (mc-vreg-equal? o1 o2)
     (mc-imm-equal?  o1 o2)
     (mc-disp-equal? o1 o2)))

(define (mc-operand-format op)
  (arch-operand-format op))

;; Vreg Protocol

(define (mc-vreg-equal? v1 v2)
  (and (mc-vreg? v1) (mc-vreg? v2) (eq? v1 v2)))

(define (mc-vreg-add-user vreg instr)
  (mc-vreg-users-set! vreg (cons instr (mc-vreg-users vreg))))

(define (mc-vreg-remove-user vreg instr)
  (mc-vreg-users-set! vreg
     (lset-difference mc-vreg-equal? (mc-vreg-users vreg) (list instr))))

(define (mc-vreg-param? v)
  (mc-vreg-attribs v))

;; Imm Protocol

(define (mc-imm-equal? i1 i2)
  (and (mc-imm? i1) (mc-imm? i2) (eq? i1 i2)))

;; Disp Protocol

(define (mc-disp-equal? d1 d2)
  (and (mc-disp? d1) (mc-disp? d2) (eq? d1 d2)))

;; Instruction Protocol

;; Get all vregs that are read 
(define (mc-instr-vregs-read instr)
  (arch-vregs-read instr))

;; Get all vregs that are written
(define (mc-instr-vregs-written instr)
  (arch-vregs-written instr))

(define (mc-instr-is-read? instr vreg)
  (and (find (lambda (x)
               (mc-vreg-equal? x vreg))
             (mc-instr-vregs-read instr))
       #t))

(define (mc-instr-is-written? instr vreg)
  (and (find (lambda (x)
               (mc-vreg-equal? x vreg))
             (mc-instr-vregs-written instr))
       #t))

;; Replace a vreg
(define (mc-instr-replace-vreg instr vreg x)
  (define (replace ops)
    (reverse
    (fold (lambda (op ops)
            (cond
               ((mc-operand-equal? op vreg)
                (mc-vreg-remove-user op instr)
                (mc-vreg-add-user x instr)
                (cons x ops))
               (else
                (cons op ops))))
          '()
           ops)))
   (mc-instr-ops-set! instr (replace (mc-instr-ops instr))))

;; Context Protocol

(define mc-context-allocate-vreg
  (lambda operands
    (match operands
      ((cxt name rest* ...)
       (let ((vregs (mc-context-vregs cxt)))
         (cond
           ((find (lambda (vreg)
                    (eq? (mc-vreg-name vreg) name))
                  vregs)
               => (lambda (vreg) vreg))
           (else
             (let ((vreg (apply mc-make-vreg (cons name rest*))))
               (mc-context-vregs-set! cxt (cons vreg vregs))
               vreg)))))
       (else (assert-not-reached)))))

;; Printing

(define (mc-module-print mod port)

  (fprintf port "section  .text\n\n")
  (fprintf port "  global __scheme_exec\n\n")
  (mc-context-for-each
     (lambda (context)
       (mc-context-print context port))
     mod))

(define (mc-context-print context port)
  (struct-case context
    ((mc-context name args entry)

     (fprintf port "  # context: name=~s args=~s\n" name (map (lambda (arg) (mc-vreg-name arg)) args))
     
     (mc-block-for-each
        (lambda (block)
            (mc-block-print block port))
        context))))

(define (mc-block-print block port)
  (struct-case block
    ((mc-block name head tail succ)
     ;; print label
     (fprintf port "  ~a:\n" name)
     ;; print code
     (mc-instr-for-each
        (lambda (instr)
           (mc-instr-print instr port))
         block)
     (fprintf port "\n"))))

(define (mc-instr-print instr port)
  (let* ((fmt         (mc-spec-fmt (mc-instr-spec instr)))
         (fmt-indices (mc-spec-fmt-indices (mc-instr-spec instr)))
         (ops-vect    (list->vector (mc-instr-ops instr)))
         (ops-sorted  (reverse (fold (lambda (i x)
                                      (cons (vector-ref ops-vect (- i 1)) x))
                                    '()
                                    fmt-indices))))
;;  (fprintf port "                                     # live = ~s\n" (map (lambda (vreg) (mc-vreg-name vreg)) (mc-instr-data instr)))
  (fprintf port "    ")
  (fprintf port
    (apply format
           (cons
             fmt
             (map mc-operand-format ops-sorted))))
  (fprintf port "\n")))


(define (mc-block-append blk instr)
  (cond
    ((and (null? (mc-block-head blk))
          (null? (mc-block-tail blk)))
     (mc-block-head-set! blk instr)
     (mc-block-tail-set! blk instr))
    (else
     (let ((tail (mc-block-tail blk)))
       (mc-instr-prev-set! instr tail)
       (mc-instr-next-set! tail instr)
       (mc-block-tail-set! blk instr))))
  blk)

(define (mc-block-insert-after blk instr x)
  (let ((next (mc-instr-next instr))
        (prev (mc-instr-prev instr)))
    (cond
      ((and (null? next))
       (mc-instr-prev-set! x     instr)
       (mc-instr-next-set! x     '())
       (mc-instr-next-set! instr x)
       (mc-block-tail-set! blk   x))
      (else
       (mc-instr-prev-set! x     instr)
       (mc-instr-next-set! x     next)
       (mc-instr-next-set! instr x)
       (mc-instr-prev-set! next  x)))))

(define (mc-block-insert-before blk instr x)
  (let ((next (mc-instr-next instr))
        (prev (mc-instr-prev instr)))
    (cond
      ((and (null? prev))
       (mc-instr-next-set! x     instr)
       (mc-instr-prev-set! x     '())
       (mc-instr-prev-set! instr x)
       (mc-block-head-set! blk   x))
      (else
       (mc-instr-prev-set! x     prev)
       (mc-instr-next-set! x     instr)
       (mc-instr-prev-set! instr x)
       (mc-instr-next-set! prev  x)))))


;; iteration

(define (mc-context-for-each f mod)
  (for-each f (mc-module-contexts mod)))


(define (mc-block-for-each f context)
  (define (visit-block block f)
    (let ((succ (mc-block-succ block)))
      (f block)
      (for-each (lambda (succ)
                  (visit-block succ f))
                succ)))
    (visit-block (mc-context-start context) f))


(define (mc-instr-for-each f block)
  (let ((head (mc-block-head block)))
    (let walk ((x head))
      (cond
       ((not (null? x))
        (f x)
        (walk (mc-instr-next x)))))))


