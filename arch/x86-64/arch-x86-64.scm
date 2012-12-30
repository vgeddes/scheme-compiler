(declare (unit arch-x86-64)
         (uses globals helpers tree machine spec-x86-64 rules-x86-64))

(module arch-x86-64 *

  (import scheme)
  (import chicken)

  (use matchable)
  (use srfi-1)

  (import globals)
  (import helpers)
  (import tree    (prefix tree tr-))
  (import machine (prefix machine mc-))

  (import spec-x86-64)
  (import rules-x86-64)
  (import arch-syntax)

  (define *regs*  '(rax rbx rcx rdx rdi rsi r8 r9 r10 r11 r12 r13 r14 r15))

  ;; Standard C calling convention
  (define *ccall*
    '((args         (rdi rsi rdx rcx r8 r9))
      (ret          rax)
      (callee-save  ())))

    ;; (rbp rbx r12 r13 r14 r15)

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

  (define (width x)
    (cond
     ;; convert a negative (and hence known to be two's-complement form) to any unsigned integer that requires the same number of bits
     ((< x 0)
      (width (+ (* 2 (abs x)) 1)))
     ((<= x #xFF) 8)
     ((<= x #xFFFF) 16)
     ((<= x #xFFFFFFFF) 32)
     (else 64)))

  (define (i8? x)
    (and (integer? x)
         (= (width x) 8)))

  (define (i16? x)
    (and (integer? x)
         (= (width x) 16)))

  (define (i32? x)
    (and (integer? x)
         (= (width x) 32)))

  (define (i64? x)
    (and (integer? x)
         (= (width x) 64)))


  ;;
  ;; Arguments passed to a context are constrained to registers r8 ... r15.
  ;; Since we want to minimise the length of pre-colored live ranges, we move the args into temps.
  ;;
  ;; So we insert moves to copy each constrained arg into an unconstrained vreg, and add
  ;; hints so that the allocator can eliminate the move if possible
  ;;

  (define (make-context name params mod)
    (let* ((cxt         (mc-make-cxt name '() '() '()))
           (tmp*        (map (lambda (param hint)
                               (mc-cxt-alloc-vreg cxt param #f hint))
                             params
                             (cc-args *scall*)))
           (arg*        (map (lambda (arg hreg)
                               (mc-cxt-alloc-vreg cxt (gensym 't) hreg #f))
                             params
                             (cc-args *scall*)))
           (blk         (mc-blk-make cxt name)))

      (arch-emit-code x86-64 blk
                      (push.r (hreg rbp))
                      (mov.rr (hreg rsp) (hreg rbp)))

      ;; insert move from each arg into a tmp
      (for-each (lambda (arg tmp)
                  (arch-emit-code x86-64 blk
                                  (mov.rr arg tmp)))
                arg* tmp*)

      (mc-cxt-strt-set! cxt blk)
      (mc-cxt-args-set! cxt arg*)
      (mc-mod-cxts-set! mod (cons cxt (mc-mod-cxts mod)))
      cxt))

  ;; move temps back into callee save regs


  (define (operand-format-x86-64 op)
    (cond
     ((mc-vreg? op)
      (format "~s" (mc-vreg-name op)))
     ((mc-imm? op)
      (format "~s" (mc-imm-value op)))
     ((mc-disp? op)
      (format "~s" (mc-disp-value op)))
     (else (assert-not-reached))))

  (define (vregs-read-x86-64 instr)
    (let* ((ops           (mc-inst-ops instr))
           (reads         ((mc-spec-reads (mc-inst-spec instr)) ops)))
      (cond
       ((x86-64.xor.rr? instr)
        (cond
         ((mc-vreg-equal? (first ops) (second ops))
          (list))
         (else reads)))
       (else reads))))

  (define (vregs-written-x86-64 instr)
    (let* ((ops  (mc-inst-ops instr))
           (writes ((mc-spec-writes (mc-inst-spec instr)) ops)))
      writes))


  ;; Generate x86-64 code for the 'return' instruction
  ;;
  (define (emit-return-x86-64 blk value)

    ;; move return value into %rax
    (cond
     ((tr-constant? value)
      (case (tr-constant-size value)
        ((i32)
         (arch-emit-code x86-64 blk
                         (mov.i64r  (imm i64 0) (hreg rax))
                         (mov.i32r  (imm i32 (tr-constant-value value)) (hreg rax))))
        ((i64)
         (arch-emit-code x86-64 blk
                         (mov.i64r  (imm i64 (tr-constant-value value)) (hreg rax))))
        (else (assert-not-reached))))
     ((tr-temp? value)
      (arch-emit-code x86-64 blk
                      (mov.rr (vreg (tr-temp-name value)) (hreg rax)))))
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
                            (x     '()))
                      (match hreg*
                             (() (cons
                                  (map (lambda (hreg)
                                         (mc-cxt-alloc-vreg (mc-blk-cxt blk) (gensym 't) hreg #f))
                                       (reverse x))
                                  (map (lambda (hreg)
                                         (mc-cxt-alloc-vreg (mc-blk-cxt blk) (gensym 't) hreg #f))
                                       (lset-difference eq? *regs* (list 'rax) x (cc-callee-save cc)))))
                             ((hreg . hreg*)
                              (if (not (null? arg*))
                                  (f (cdr arg*) hreg* (cons hreg x))
                                  (f arg*       hreg* x))))))
           (hargs   (car pair))
           (clobs   (if (null? dst)
                        (cdr pair)
                        (append dst (cdr pair)))))

      ;; Select instructions for each move
      (for-each
       (lambda (reg arg)
         (cond
          ((mc-vreg? arg)
           (arch-emit-code x86-64 blk
                           (mov.rr arg reg)))
          ((mc-imm? arg)
           (arch-emit-code x86-64 blk
                           (mov.i64r arg reg)))
          (else (print arg) (assert-not-reached))))
       hargs args)

      (cons hargs clobs)))


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
                           ((tr-temp? arg)
                            (mc-cxt-alloc-vreg (mc-blk-cxt blk) (tr-temp-name arg) ))
                           ((tr-constant? arg)
                            (mc-imm-make (tr-constant-size arg) (tr-constant-value arg)))
                           (else (assert-not-reached))))
                        args))
           (mdst   (if dst (mc-cxt-alloc-vreg (mc-blk-cxt blk) dst #f #f) #f))
           (mtgt   (cond
                    ((tr-label? tgt)
                     (mc-disp-make (tr-label-name tgt)))
                    ((tr-temp? tgt)
                     (mc-cxt-alloc-vreg (mc-blk-cxt blk) (tr-temp-name tgt)))
                    (else (assert-not-reached))))
           (rax    (mc-cxt-alloc-vreg (mc-blk-cxt blk) (gensym 't) 'rax #f))
           (clob-info  (shuf-args blk margs (if mdst (list rax) '()) *ccall*))
           (hargs  (car clob-info))
           (clobs  (cdr clob-info)))
      (cond
       ((tr-label? tgt)
        (x86-64.call.d blk hargs (append hargs clobs) mtgt))
       ((tr-temp? tgt)
        (x86-64.call.r blk hargs (append hargs clobs) mtgt))
       (else (assert-not-reached)))
      (if mdst
          (arch-emit-code x86-64 blk
                          (mov.rr rax mdst)))))

  (define (emit-scall-x86-64 blk tgt args dst)
    (let* ((margs  (map (lambda (arg)
                          (cond
                           ((tr-temp? arg)
                            (mc-cxt-alloc-vreg (mc-blk-cxt blk) (tr-temp-name arg)))
                           ((tr-constant? arg)
                            (mc-imm-make (tr-constant-size arg) (tr-constant-value arg)))
                           (else (assert-not-reached))))
                        args))
           (mdst   (mc-cxt-alloc-vreg (mc-blk-cxt blk) dst #f #f))
           (mtgt   (cond
                    ((tr-label? tgt)
                     (mc-disp-make (tr-label-name tgt)))
                    ((tr-temp? tgt)
                     (mc-cxt-alloc-vreg (mc-blk-cxt blk) (tr-temp-name tgt)))
                    (else (assert-not-reached))))
           (rax    (mc-cxt-alloc-vreg (mc-blk-cxt blk) (gensym 't) 'rax #f))
           (clob-info  (shuf-args blk margs (list rax) *scall*))
           (hargs  (car clob-info))
           (clobs  (cdr clob-info)))
      ;; emit call
      (cond
       ((tr-label? tgt)
        (x86-64.call.d blk hargs (append hargs clobs) mtgt))
       ((tr-temp? tgt)
        (x86-64.call.r blk hargs (append hargs clobs) mtgt))
       (else (assert-not-reached)))
      ;; Move rax to dst
      (arch-emit-code x86-64 blk
                      (mov.rr rax mdst))))

  (define (generate-bridge-context-x86-64 mod)
    (let* ((cxt  (mc-make-cxt '__scheme_exec '() '() '()))
           (ptr  (mc-cxt-alloc-vreg cxt (gensym 't) 'rsi #f)))

      (mc-cxt-args-set! cxt (list ptr))
      (mc-cxt-strt-set! cxt (mc-blk-make cxt '__scheme_exec))

      (arch-emit-code x86-64 (mc-cxt-strt cxt)
                      ;; prologue
                      (push.r  (hreg rbp))
                      (mov.rr  (hreg rsp) (hreg rbp))
                      ;; jump to __begin
                      (jmp.d   (disp 'begin)))

      (mc-mod-cxts-set! mod (cons cxt (mc-mod-cxts mod)))

      cxt))

  ;; Selects x86-64 instructions for a tree node
  ;;
  (define (emit-stm block stm)
    (match stm
           (($ tr-instr 'return _ value)
            (emit-return-x86-64 block value))
           (($ tr-instr 'call _ tgt args)
            (let ((conv (tr-instr-attr stm 'callconv)))
              (case conv
                ((c)
                 (emit-ccall-x86-64 block tgt args #f))
                (else '()))))
           (($ tr-instr 'assign _ dst ($ tr-instr 'call _ tgt args))
            (let ((conv (tr-instr-attr (tr-assign-value stm) 'callconv)))
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

)
