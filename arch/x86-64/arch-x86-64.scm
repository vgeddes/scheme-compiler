
(declare (unit arch-x86-64)
         (uses machine spec-x86-64 rules-x86-64 tree nodes))

(use matchable)
(use srfi-1)

(include "arch-syntax")

(define *regs*  '(rax rbx rcx rdx rdi rsi r8 r9 r10 r11 r12 r13 r14 r15))

;; Standard C calling convention
(define *ccall*
  '((args         (rdi rsi rdx rcx r8 r9))
    (ret          rax)
    (callee-save  (rbp rbx r12 r13 r14 r15))))

;; Our calling convention (non-TCO)
(define *scall*
  '((args         (rdi rsi r8 r9 r10 r11 r12 r13 r14 r15))
    (ret          rax)
    (callee-save  ())))

;; Our tail calling convention
(define *tcall*
  '((args         (rdi rsi r8 r9 r10 r11 r12 r13 r14 r15))
    (ret          #f)
    (callee-save  ())))

(define (cc-args cc)
  (match cc
    ((('args args) _ _)
     args)))

(define (cc-ret cc)
  (match cc
    ((_ ('ret ret) _)
     ret)))

(define (cc-callee-save cc)
  (match cc
    ((_ _ ('callee-save x))
     x)))



(define (ces? hreg)
  (and (memq hreg *callee-save*) #t))

(define (crs? hreg)
  (not ces? hreg))

;;
;; Arguments passed to a context are constrained to registers r8 ... r15.
;; Since we want to minimise the length of pre-colored live ranges, we move the args into temps.
;;
;; So we insert moves to copy each constrained arg into an unconstrained vreg, and add
;; hints so that the allocator can eliminate the move if possible
;;

(define (make-context name params mod)
  (let* ((cxt         (make-mcxt name '() '() '()))
         (tmp*        (map (lambda (param hint)
                             (mcxt-alloc-vreg cxt param #f hint))
                           params
                           (cc-args *scall*)))
         (arg*        (map (lambda (arg hreg)
                             (mcxt-alloc-vreg cxt (gensym 't) hreg #f))
                           params
                           (cc-args *scall*)))
         (blk         (mblk-make cxt name)))

    (arch-emit-code x86-64 blk
      (push.r (hreg rbp))
      (mov.rr (hreg rsp) (hreg rbp)))

    ;; insert move from each arg into a tmp
    (for-each (lambda (arg tmp)
                (arch-emit-code x86-64 blk
                  (mov.rr arg tmp)))
              arg* tmp*)

    (mcxt-strt-set! cxt blk)
    (mcxt-args-set! cxt arg*)
    (mmod-cxts-set! mod (cons cxt (mmod-cxts mod)))
    cxt))

;; move temps back into callee save regs


(define (operand-format-x86-64 op)
  (cond
    ((mvr? op)
     (format "~s" (mvr-name op)))
    ((mim? op)
     (format "~s" (mim-value op)))
    ((mdi? op)
     (format "~s" (mdi-value op)))
    (else (assert-not-reached))))

(define (vregs-read-x86-64 instr)
  (let* ((ops           (minst-ops instr))
         (reads         ((mspec-reads (minst-spec instr)) ops)))
  (cond
    ((x86-64.xor.rr? instr)
     (cond
       ((mvr-equal? (first ops) (second ops))
        (list))
       (else reads)))
    (else reads))))

(define (vregs-written-x86-64 instr)
  (let* ((ops  (minst-ops instr))
         (writes ((mspec-writes (minst-spec instr)) ops)))
     writes))


;; Generate x86-64 code for the 'return' instruction
;;
(define (emit-return-x86-64 blk value)

  ;; move return value into %rax
  (cond
    ((tree-constant? value)
     (case (tree-constant-size value)
       ((i32)
        (arch-emit-code x86-64 blk
          (mov.i64r  (imm i64 0) (hreg rax))
          (mov.i32r  (imm i32 (tree-constant-value value)) (hreg rax))))
       ((i64)
        (arch-emit-code x86-64 blk
          (mov.i64r  (imm i64 (tree-constant-value value)) (hreg rax))))
       (else (assert-not-reached))))
    ((tree-temp? value)
     (arch-emit-code x86-64 blk
       (mov.rr (vreg (tree-temp-name value)) (hreg rax)))))
  ;; stack frame management
  (arch-emit-code x86-64 blk
    (mov.rr (hreg rbp) (hreg rsp))
    (pop.r  (hreg rbp))
    (retnear)))





;;
;; Generates code for moving args into the arg-passing hregs defined by the given calling convention
;;
;; returns the list of hregs used to pass params
;;
(define (shuf-args blk args dst cc)
  (let* ((pair    (let f ((arg*   args)
                          (hreg*  (cc-args cc))
                          (x     '())
                          (y     '()))
                    (match hreg*
                      (() (cons
                           (map (lambda (hreg)
                                  (mcxt-alloc-vreg (mblk-cxt blk) (gensym 't) hreg #f))
                                (reverse x))
                           (map (lambda (hreg)
                                  (mcxt-alloc-vreg (mblk-cxt blk) (gensym 't) hreg #f))
                                (lset-difference eq? *regs* (list 'rax) x (cc-callee-save cc)))))
                      ((hreg . hreg*)
                       (if (not (null? arg*))
                           (f (cdr arg*) hreg* (cons (car arg*) x) y)
                           (f arg*       hreg* x                   (cons hreg y)))))))
         (hargs   (car pair))
         (clobs   (cdr pair))
         (union   (lset-union (lambda (x y)
                                (eq? (mvr-hreg x) (mvr-hreg y)))
                                 hargs dst clobs)))

    ;; Select instructions for each move
    (for-each
     (lambda (reg arg)
       (cond
        ((mvr? arg)
         (arch-emit-code x86-64 blk
           (mov.rr arg reg)))
        ((mim? arg)
         (arch-emit-code x86-64 blk
           (mov.i64r arg reg)))
        (else (print arg) (assert-not-reached))))
     hargs args)

    union))


;;
;; Lower conventional C ABI calls to x86-64
;;
;; We move each positional arg to a constrained temp (i.e pre-colored to a hardware register)
;;
;; Standard argument passing registers (in order): rdi rsi rdx rcx r8 r9
;;
;; We create a selection tree for each move so we can produce efficient code for moving the arg
;; to the constrained temp. This is mostly useful for immediate -> register moves.
;;
;; Example:
;;
;; lower (call fib45 (t5 t67 3 t34)) =>
;;
;;    mov  r8,  t5
;;    mov  r9,  t67
;;    mov  r10, 3
;;    mov  r11, t34
;;    call  [rip + fib45]  # relative jmp to fib45.
;;    mov  t0 eax
;;
;; For liveness analysis and register allocation purposes, we hint that 'jmp' implicitly uses r8, r9, r10 and r11
;;

(define (emit-ccall-x86-64 blk tgt args dst)
  (let* ((margs  (map (lambda (arg)
                        (cond
                         ((tree-temp? arg)
                          (mcxt-alloc-vreg (mblk-cxt blk) (tree-temp-name arg)))
                         ((tree-constant? arg)
                          (mim-make (tree-constant-size arg) (tree-constant-value arg)))
                         (else (assert-not-reached))))
                      args))
         (mdst   (if dst (mcxt-alloc-vreg (mblk-cxt blk) dst #f #f) #f))
         (mtgt   (cond
                  ((tree-label? tgt)
                   (mdi-make (tree-label-name tgt)))
                  ((tree-temp? tgt)
                   (mcxt-alloc-vreg (mblk-cxt blk) (tree-temp-name tgt)))
                  (else (assert-not-reached))))
        (rax    (mcxt-alloc-vreg (mblk-cxt blk) (gensym 't) 'rax #f))
        (clobs  (shuf-args blk margs (if mdst (list rax) '()) *ccall*)))
    (cond
     ((tree-label? tgt)
      (x86-64.call.d blk clobs mtgt))
     ((tree-temp? tgt)
      (x86-64.call.r blk clobs mtgt))
     (else (assert-not-reached)))
    (if mdst
        (arch-emit-code x86-64 blk
                        (mov.rr rax mdst)))))

(define (emit-scall-x86-64 blk tgt args dst)
  (let* ((margs  (map (lambda (arg)
                        (cond
                         ((tree-temp? arg)
                          (mcxt-alloc-vreg (mblk-cxt blk) (tree-temp-name arg)))
                         ((tree-constant? arg)
                          (mim-make (tree-constant-size arg) (tree-constant-value arg)))
                         (else (assert-not-reached))))
                      args))
         (mdst   (mcxt-alloc-vreg (mblk-cxt blk) dst #f #f))
         (mtgt   (cond
                  ((tree-label? tgt)
                   (mdi-make (tree-label-name tgt)))
                  ((tree-temp? tgt)
                   (mcxt-alloc-vreg (mblk-cxt blk) (tree-temp-name tgt)))
                  (else (assert-not-reached))))
        (rax    (mcxt-alloc-vreg (mblk-cxt blk) (gensym 't) 'rax #f))
        (clobs  (shuf-args blk margs (list rax) *scall*)))
    ;; emit call
    (cond
     ((tree-label? tgt)
      (x86-64.call.d blk clobs mtgt))
     ((tree-temp? tgt)
      (x86-64.call.r blk clobs mtgt))
     (else (assert-not-reached)))
    ;; Move rax to dst
    (arch-emit-code x86-64 blk
                    (mov.rr rax mdst))))

(define (generate-bridge-context-x86-64 mod)
  (let* ((cxt  (make-mcxt '__scheme_exec '() '() '()))
         (ptr  (mcxt-alloc-vreg cxt (gensym 't) 'rsi #f)))

    (mcxt-args-set! cxt (list ptr))
    (mcxt-strt-set! cxt (mblk-make cxt '__scheme_exec))

    (arch-emit-code x86-64 (mcxt-strt cxt)
      ;; prologue
      (push.r  (hreg rbp))
      (mov.rr  (hreg rsp) (hreg rbp))
      ;; jump to __begin
      (jmp.d   (disp 'begin)))

    (mmod-cxts-set! mod (cons cxt (mmod-cxts mod)))

    cxt))

;; Selects x86-64 instructions for a tree node
;;
(define (emit-stm block stm)
  (match stm
    (($ tree-instr 'return _ value)
     (emit-return-x86-64 block value))
    (($ tree-instr 'call _ tgt args)
     (let ((conv (tree-instr-attr stm 'callconv)))
       (case conv
         ((c)
          (emit-ccall-x86-64 block tgt args #f))
         (else '()))))
    (($ tree-instr 'assign _ dst ($ tree-instr 'call _ tgt args))
     (let ((conv (tree-instr-attr (tree-assign-value stm) 'callconv)))
       (case conv
         ((c)
          (emit-ccall-x86-64 block tgt args dst))
         ((s)
          (emit-scall-x86-64 block tgt args dst))
         (else '()))))
    (_ (munch-x86-64 block stm))))

(define <arch-x86-64>
        (make-arch-descriptor
           make-context
           operand-format-x86-64
           vregs-read-x86-64
           vregs-written-x86-64
           generate-bridge-context-x86-64
           emit-stm))
