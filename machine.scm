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
  (define-struct cxt   (name args strt vrgs))
  (define-struct blk   (name head tail succ cxt))

  ;; instructions
  (define-struct spec  (name fmt fmt-indices verifiers reads writes))
  (define-struct inst  (spec ops nxt prv iu id idx blk data))

  ;; operands
  (define-struct vreg    (name hreg pref usrs data))
  (define-struct imm     (size value))
  (define-struct disp   (value))

  ;; constructors
  (define (mod-make)
    (make-mod '()))

  (define (cxt-make name params mod)
    (arch-make-context name params mod))

  (define (blk-make cxt name)
    (make-blk name '() '() '() cxt))

  (define (inst-make blk spec iu id ops)
    (let ((instr (make-inst spec ops '() '() iu id #f blk '())))
      (for-each (lambda (op)
                  (cond
                   ((vreg? op)
                    (vreg-add-user op instr))))
                ops)
      (and blk (blk-append blk instr))
      instr))

  (define vreg-make
    (lambda operands
      (match operands
             ((name)
              (make-vreg name #f #f '() '()))
             ((name hreg pref)
              (make-vreg name hreg pref '() '()))
             (else (assert-not-reached)))))

  (define (disp-make value)
    (make-disp value))

  (define (imm-make size value)
    (make-imm size value))


  ;; Operand Protocol

  (define (mop-equal? o1 o2)
    (or (vreg-equal? o1 o2)
        (imm-equal? o1 o2)
        (disp-equal? o1 o2)))

  (define (mop-format op)
    (arch-operand-format op))

  ;; Vreg Protocol

  (define (vreg-equal? v1 v2)
    (and (vreg? v1) (vreg? v2) (eq? v1 v2)))

  (define (vreg-add-user vr instr)
    (vreg-usrs-set! vr (cons instr (vreg-usrs vr))))

  (define (vreg-remove-user vr instr)
    (vreg-usrs-set! vr
                    (lset-difference vreg-equal? (vreg-usrs vr) (list instr))))

  ;; Imm Protocol

  (define (imm-equal? i1 i2)
    (and (imm? i1) (imm? i2) (eq? i1 i2)))

  ;; Disp Protocol

  (define (disp-equal? d1 d2)
    (and (disp? d1) (disp? d2) (eq? d1 d2)))

  ;; Instruction Protocol

  ;; Get all vregs that are read
  (define (inst-vregs-read instr)
    (arch-vregs-read instr))

  ;; Get all vregs that are written
  (define (inst-vregs-written instr)
    (arch-vregs-written instr))

  (define (inst-is-read? instr vr)
    (and (find (lambda (x)
                 (vreg-equal? x vr))
               (inst-vregs-read instr))
         #t))

  (define (inst-is-written? instr vr)
    (and (find (lambda (x)
                 (vreg-equal? x vr))
               (inst-vregs-written instr))
         #t))

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

  ;; Context Protocol

  (define cxt-alloc-vreg
    (lambda operands
      (match operands
             ((cxt name rest* ...)
              (let ((vregs (cxt-vrgs cxt)))
                (cond
                 ((find (lambda (vr)
                          (eq? (vreg-name vr) name))
                        vregs)
                  => (lambda (vr) vr))
                 (else
                  (let ((vr (apply vreg-make (cons name rest*))))
                    (cxt-vrgs-set! cxt (cons vr vregs))
                    vr)))))
             (else (assert-not-reached)))))

  ;; Printing

  (define (mod-print mod port)
    (fprintf port "section  .text\n\n")
    (fprintf port "  global __scheme_exec\n\n")
    (cxt-for-each
     (lambda (cxt)
       (cxt-print cxt port))
     mod))

  (define (cxt-print cxt port)
    (struct-case cxt
                 ((cxt name args entry)

                  (fprintf port "  # context: name=~s args=~s\n" name (map (lambda (arg) (vreg-name arg)) args))

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
      ;;  (fprintf port "                                     # live = ~s\n" (map (lambda (vreg) (vreg-name vreg)) (inst-data instr)))
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
           (prev-set inst     curs)
           (next-set inst     '())
           (next-set curs inst)
           (tail-set blk   inst))
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
