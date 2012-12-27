
(declare (unit machine)
         (uses nodes arch))

(use matchable)
(use srfi-1)

(include "struct-syntax")

;; aggregate structures
(define-struct mmod   (cxts))
(define-struct mcxt   (name args strt vrgs))
(define-struct mblk   (name head tail succ cxt))

;; instructions
(define-struct mspec  (name fmt fmt-indices verifiers reads writes))
(define-struct minst  (spec ops nxt prv iu idx blk data))

;; operands
(define-struct mvr    (name hreg pref usrs data))
(define-struct mim    (size value))
(define-struct mdi    (value))

;; constructors
(define (mmod-make)
  (make-mmod '()))

(define (mcxt-make name params mod)
  (arch-make-context name params mod))

(define (mblk-make cxt name)
  (make-mblk name '() '() '() cxt))

(define (minst-make blk spec iu ops)
  (let ((instr (make-minst spec ops '() '() iu #f blk '())))
    (for-each (lambda (op)
                (cond
                   ((mvr? op)
                    (mvr-add-user op instr))))
              ops)
    (and blk (mblk-append blk instr))
    instr))

(define mvr-make
  (lambda operands
    (match operands
       ((name)
        (make-mvr name #f #f '() '()))
       ((name hreg pref)
        (make-mvr name hreg pref '() '()))
       (else (assert-not-reached)))))

(define (mdi-make value)
  (make-mdi value))

(define (mim-make size value)
  (make-mim size value))


;; Operand Protocol

(define (mop-equal? o1 o2)
 (or (mvr-equal? o1 o2)
     (mim-equal? o1 o2)
     (mdi-equal? o1 o2)))

(define (mop-format op)
  (arch-operand-format op))

;; Vreg Protocol

(define (mvr-equal? v1 v2)
  (and (mvr? v1) (mvr? v2) (eq? v1 v2)))

(define (mvr-add-user vreg instr)
  (mvr-usrs-set! vreg (cons instr (mvr-usrs vreg))))

(define (mvr-remove-user vreg instr)
  (mvr-usrs-set! vreg
     (lset-difference mvr-equal? (mvr-usrs vreg) (list instr))))

(define (mvr-param? v)
  (mvr-attribs v))

;; Imm Protocol

(define (mim-equal? i1 i2)
  (and (mim? i1) (mim? i2) (eq? i1 i2)))

;; Disp Protocol

(define (mdi-equal? d1 d2)
  (and (mdi? d1) (mdi? d2) (eq? d1 d2)))

;; Instruction Protocol

;; Get all vregs that are read
(define (minst-vregs-read instr)
  (arch-vregs-read instr))

;; Get all vregs that are written
(define (minst-vregs-written instr)
  (arch-vregs-written instr))

(define (minst-is-read? instr vreg)
  (and (find (lambda (x)
               (mvr-equal? x vreg))
             (minst-vregs-read instr))
       #t))

(define (minst-is-written? instr vreg)
  (and (find (lambda (x)
               (mvr-equal? x vreg))
             (minst-vregs-written instr))
       #t))

;; Replace a vreg
(define (minst-replace-vreg instr vreg x)
  (define (replace ops)
    (reverse
    (fold (lambda (op ops)
            (cond
               ((mop-equal? op vreg)
                (mvr-remove-user op instr)
                (mvr-add-user x instr)
                (cons x ops))
               (else
                (cons op ops))))
          '()
           ops)))
   (minst-ops-set! instr (replace (minst-ops instr))))

;; Context Protocol

(define mcxt-alloc-vreg
  (lambda operands
    (match operands
      ((cxt name rest* ...)
       (let ((vregs (mcxt-vrgs cxt)))
         (cond
           ((find (lambda (vreg)
                    (eq? (mvr-name vreg) name))
                  vregs)
               => (lambda (vreg) vreg))
           (else
             (let ((vreg (apply mvr-make (cons name rest*))))
               (mcxt-vrgs-set! cxt (cons vreg vregs))
               vreg)))))
       (else (assert-not-reached)))))

;; Printing

(define (mmod-print mod port)
  (fprintf port "section  .text\n\n")
  (fprintf port "  global __scheme_exec\n\n")
  (mcxt-for-each
     (lambda (cxt)
       (mcxt-print cxt port))
     mod))

(define (mcxt-print cxt port)
  (struct-case cxt
    ((mcxt name args entry)

     (fprintf port "  # context: name=~s args=~s\n" name (map (lambda (arg) (mvr-name arg)) args))

     (mblk-for-each
        (lambda (block)
            (mblk-print block port))
        cxt))))

(define (mblk-print block port)
  (struct-case block
    ((mblk name head tail succ)
     ;; print label
     (fprintf port "  ~a:\n" name)
     ;; print code
     (minst-for-each
        (lambda (instr)
           (minst-print instr port))
         block)
     (fprintf port "\n"))))


(define (minst-print instr port)
  (let* ((fmt         (mspec-fmt (minst-spec instr)))
         (fmt-indices (mspec-fmt-indices (minst-spec instr)))
         (ops-vect    (list->vector (minst-ops instr)))
         (ops-sorted  (reverse (fold (lambda (i x)
                                      (cons (vector-ref ops-vect (- i 1)) x))
                                    '()
                                    fmt-indices))))
;;  (fprintf port "                                     # live = ~s\n" (map (lambda (vreg) (mvr-name vreg)) (minst-data instr)))
  (fprintf port "    ")
  (fprintf port
    (apply format
           (cons
             fmt
             (map mop-format ops-sorted))))
  (fprintf port "\n")))

;; Block

(define prev-set minst-prv-set!)
(define next-set minst-nxt-set!)
(define tail-set mblk-tail-set!)
(define head-set mblk-head-set!)

(define (mblk-prepend blk instr)
  (cond
   ((and (null? (mblk-head blk))
         (null? (mblk-tail blk)))
    (head-set blk instr)
    (tail-set blk instr))
   (else
    (let ((head (mblk-tail blk)))
      (prev-set head instr)
      (next-set instr head)
      (head-set blk instr))))
  blk)

(define (mblk-append blk instr)
  (cond
    ((and (null? (mblk-head blk))
          (null? (mblk-tail blk)))
     (head-set blk instr)
     (tail-set blk instr))
    (else
     (let ((tail (mblk-tail blk)))
       (prev-set instr tail)
       (next-set tail instr)
       (tail-set blk instr))))
  blk)

(define (mblk-insert blk curs pos inst)
  (let ((next (minst-nxt curs))
        (prev (minst-prv curs)))
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

(define (mcxt-for-each f mod)
  (for-each f (mmod-cxts mod)))


(define (mblk-for-each f cxt)
  (define (visit-block blk f)
    (let ((succ (mblk-succ blk)))
      (f blk)
      (for-each (lambda (succ)
                  (visit-block succ f))
                succ)))
    (visit-block (mcxt-strt cxt) f))


(define (minst-for-each f blk)
  (let ((head (mblk-head blk)))
    (let walk ((x head))
      (cond
       ((not (null? x))
        (f x)
        (walk (minst-nxt x)))))))
