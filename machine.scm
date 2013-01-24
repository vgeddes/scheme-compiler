(declare (unit machine)
         (uses globals arch helpers))

(module machine *

  (import scheme)
  (import chicken)
  (import extras)

  (import globals)
  (import arch)
  (import helpers)

  (use matchable)
  (use srfi-1)

  ;; aggregate structures
  (define-struct mod   (cxts))
  (define-struct cxt   (name strt vrgs vrgs-count hreg-params stk-params stk-locals))
  (define-struct blk   (name head tail succ cxt))

  ;; instructions
  (define-struct spec  (name fmt fmt-indices verifiers reads writes))
  (define-struct inst  (spec ops nxt prv idx blk attrs))

  ;; operands
  (define-struct vreg  (id hreg reserved usrs slot data))
  (define-struct imm   (size value))
  (define-struct disp  (value))

  ;; constructors
  (define (mod-make)
    (make-mod '()))

  (define (cxt-make name params mod)
    (arch-make-context name params mod))

  (define (blk-make cxt name)
    (make-blk name '() '() '() cxt))

  (define (inst-make spec blk ops attrs)
    (let ((instr (make-inst spec ops '() '() #f blk attrs)))
      (for-each (lambda (op)
                  (cond
                   ((vreg? op)
                    (vreg-add-user op instr))))
                ops)
      (and blk (blk-append blk instr))
      instr))

  (define (disp-make value)
    (make-disp value))

  (define (imm-make size value)
    (make-imm size value))

  ;; Operand Protocol

  (define (mop-equal? o1 o2)
    (or (vreg-equal?  o1 o2)
        (imm-equal?   o1 o2)
        (disp-equal?  o1 o2)))

  (define (mop-format op)
    (arch-operand-format op))

  ;; Vreg Protocol

  (define (vreg-equal? v1 v2)
    (and (vreg? v1) (vreg? v2) (eq? v1 v2)))

  (define (vreg-add-user vr instr)
    (vreg-usrs-set! vr (cons instr (vreg-usrs vr))))

  (define (vreg-remove-user vr instr)
    (vreg-usrs-set! vr (lset-difference vreg-equal? (vreg-usrs vr) (list instr))))

  ;; Imm Protocol

  (define (imm-equal? i1 i2)
    (and (imm? i1) (imm? i2) (eq? i1 i2)))

  ;; Disp Protocol

  (define (disp-equal? d1 d2)
    (and (disp? d1) (disp? d2) (eq? d1 d2)))

  ;; Instruction Protocol

  ;; Get all vregs that are used at this instr
  (define (vreg-uses instr)
    (arch-vreg-uses instr))

  ;; Get all vregs that are defined at this instr
  (define (vreg-defs instr)
    (arch-vreg-defs instr))

  (define (vreg-use? vr instr)
    (and (find (lambda (x)
                 (vreg-equal? x vr))
               (vreg-uses instr))
         #t))

  (define (vreg-def? vr instr)
    (and (find (lambda (x)
                 (vreg-equal? x vr))
               (vreg-defs instr))
         #t))

  (define (inst-attr inst name)
    (cond
     ((assq name (inst-attrs inst))
      => cdr)
     (else #f)))

  (define (inst-attr-add inst attr val)
    (inst-attrs-set! inst (cons (cons attr val) (inst-attrs inst))))

  ;; Replace a vreg
  (define (inst-replace-vreg instr vr x)
    (define (replace ops)
      (reverse
       (fold (lambda (op ops)
               (cond
                ((mop-equal? op vr)
                 (vreg-remove-user op instr)
                 (vreg-add-user x instr)
                 (cons x ops))
                (else
                 (cons op ops))))
             '()
             ops)))
    (inst-ops-set! instr (replace (inst-ops instr))))

  (define (vreg-alloc cxt)
    (let ((vreg (make-vreg (cxt-vrgs-count cxt) #f #f '() #f '())))
      (cxt-vrgs-set! cxt (cons vreg (cxt-vrgs cxt)))
      (cxt-vrgs-count-set! cxt (+ 1 (cxt-vrgs-count cxt)))
      vreg))

  ;; Printing

  (define (mod-print mod port)
    (fprintf port "section  .text\n\n")
    (fprintf port "  global __scheme_exec\n\n")
    (cxt-for-each (lambda (cxt)
                    (cxt-print cxt port))
                  mod))

  (define (cxt-print cxt port)
    (struct-case cxt
      ((cxt name strt vrgs vrgs-count hreg-params stk-params stk-locals)
       (arch-print-frame-info cxt port)
       (blk-for-each
        (lambda (block)
          (blk-print block port))
        cxt))))

  (define (blk-print block port)
    (struct-case block
                 ((blk name head tail succ)
                  ;; print label
                  (fprintf port "  ~a:\n" name)
                  ;; print code
                  (inst-for-each
                   (lambda (instr)
                     (inst-print instr port))
                   block)
                  (fprintf port "\n"))))


  (define (inst-print instr port)
    (let* ((fmt         (spec-fmt (inst-spec instr)))
           (fmt-indices (spec-fmt-indices (inst-spec instr)))
           (ops-vect    (list->vector (inst-ops instr)))
           (ops-sorted  (reverse (fold (lambda (i x)
                                         (cons (vector-ref ops-vect (- i 1)) x))
                                       '()
                                       fmt-indices))))
      (fprintf port "    ")
      (fprintf port
               (apply format
                      (cons
                       fmt
                       (map mop-format ops-sorted))))
      (fprintf port "\n")))

  ;; Block

  (define prev-set inst-prv-set!)
  (define next-set inst-nxt-set!)
  (define tail-set blk-tail-set!)
  (define head-set blk-head-set!)

  (define (blk-prepend blk instr)
    (cond
     ((and (null? (blk-head blk))
           (null? (blk-tail blk)))
      (head-set blk instr)
      (tail-set blk instr))
     (else
      (let ((head (blk-tail blk)))
        (prev-set head instr)
        (next-set instr head)
        (head-set blk instr))))
    blk)

  (define (blk-append blk instr)
    (cond
     ((and (null? (blk-head blk))
           (null? (blk-tail blk)))
      (head-set blk instr)
      (tail-set blk instr))
     (else
      (let ((tail (blk-tail blk)))
        (prev-set instr tail)
        (next-set tail instr)
        (tail-set blk instr))))
    blk)

  (define (blk-insert blk curs pos inst)
    (let ((next (inst-nxt curs))
          (prev (inst-prv curs)))
      (case pos
        ((after)
         (cond
          ((and (null? next))
           (prev-set inst curs)
           (next-set inst '())
           (next-set curs inst)
           (tail-set blk  inst))
          (else
           (prev-set inst curs)
           (next-set inst next)
           (next-set curs inst)
           (prev-set next inst))))
        ((before)
         (cond
          ((and (null? prev))
           (next-set inst curs)
           (prev-set inst '())
           (prev-set curs inst)
           (head-set blk  inst))
          (else
           (prev-set inst prev)
           (next-set inst curs)
           (prev-set curs inst)
           (next-set prev inst))))
        (else (assert-not-reached)))))

  ;; iteration

  (define (cxt-for-each f mod)
    (for-each f (mod-cxts mod)))


  (define (blk-for-each f cxt)
    (define (visit-block blk f)
      (let ((succ (blk-succ blk)))
        (f blk)
        (for-each (lambda (succ)
                    (visit-block succ f))
                  succ)))
    (visit-block (cxt-strt cxt) f))

  (define (inst-for-each f blk)
    (let ((head (blk-head blk)))
      (let walk ((x head))
        (cond
         ((not (null? x))
          (f x)
          (walk (inst-nxt x)))))))

)
