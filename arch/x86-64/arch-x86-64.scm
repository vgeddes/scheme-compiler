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


      ;; insert move from each arg into a tmp
      (for-each (lambda (arg tmp)
                  (emit-x86-64 blk
                    (mov.rr arg tmp)))
                arg* tmp*)

      ;; allocate space for locals
      (emit-x86-64 blk
        (sub.i32r #f rsp
          (attrs (replace (1 'frame-size-locals)))))

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
  (define (emit-return-x86-64 cxt blk val)
    ;; move return value into %rax
    (cond
     ((mc-imm? val)
      (case (mc-imm-size val)
        ((i64)
         (emit-x86-64 blk
           (mov.i64r val (hreg rax))))
        (else (assert-not-reached))))
     ((mc-vreg? val)
      (emit-x86-64 blk
        (mov.rr val (hreg rax)))))
    ;;  deallocate locals, and issue return
    (let ((stack-args-size (num-stack-args cxt)))
      (emit-x86-64 blk
        (add.i32r #f rsp
          (attrs (replace (list 1 'frame-size-locals))))
        (ret.i16 (imm i16 stack-args-size)))))

  ;;
  ;; Generates code for moving args into the standard argument-passing hregs defined by the given calling convention
  ;;
  ;; returns a pair:
  ;;   list of the argument-passing hregs which are actually used
  ;;   list of regs which are clobbered

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

  (define (partition-args arg* cc)
    (let ((reg* (cc-args cc)))
      (let loop ((reg* reg*) (arg* arg*) (x '()))
        (match reg*
               (() (cons (reverse x) arg*))
               ((reg . reg*)
                (match arg*
                       (()
                        (cons (reverse x) '()))
                       ((arg . arg*)
                        (loop arg* reg* (cons arg x)))))))))

  (define (convert-atom cxt node)
    (cond
     ((tr-temp? node)
      (mc-cxt-alloc-vreg cxt (tr-temp-name arg)))
     ((tr-constant? node)
      (mc-imm-make (tr-constant-size arg) (tr-constant-value arg)))
     ((tr-label? node)
      (mc-disp-make (tr-label-name node)))
     (else (assert-not-reached))))

  (define (num-stack-args cxt)
    (let ((params          (length (mc-cxt-params cxt)))
          (arg-hregs-count (length (cc-args *scall*))))
      (if (>= params arg-hreg-count)
          (- params arg-hreg-count)
          0)))

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
    (let* ((rax         (mc-cxt-alloc-vreg (mc-blk-cxt blk) (gensym 't) 'rax #f))
           (uses/defs   (shuf-args blk margs (list rax) *ccall*))
           (hregs-used  (car uses/defs))
           (hregs-defs  (cdr uses/defs)))
      ;; emit call
      (cond
       ((mc-disp? tgt)
        (emit-x86-64 blk
                     (call.d tgt
                             (attrs (uses hregs-used)
                                    (defs (append hregs-used hregs-defs))))))
       ((mc-vreg? tgt)
        (emit-x86-64 blk
                     (call.r tgt
                             (attrs (uses hregs-used)
                                    (defs (append hregs-used hregs-defs))))))
       (else (assert-not-reached)))
      ;; Move rax to dst
      (emit-x86-64 blk
                   (mov.rr rax dst))))


  (define (emit-scall-x86-64 blk tgt args dst)
    (let* ((rax         (mc-cxt-alloc-vreg (mc-blk-cxt blk) (gensym 't) 'rax #f))
           (uses/defs   (shuf-args blk margs (list rax) *scall*))
           (hregs-used  (car uses/defs))
           (hregs-defs  (cdr uses/defs)))
      ;; emit call
      (cond
       ((mc-disp? tgt)
        (emit-x86-64 blk
          (call.d tgt
            (attrs (uses hregs-used)
                   (defs (append hregs-used hregs-defs))))))
       ((mc-vreg? tgt)
        (emit-x86-64 blk
          (call.r tgt
            (attrs (uses hregs-used)
                   (defs (append hregs-used hregs-defs))))))
       (else (assert-not-reached)))
      ;; Move rax to dst
      (emit-x86-64 blk
        (mov.rr rax dst))))

  (define (emit-tcall-x86-64 blk tgt args)
    ;; determine which args are passed on the stack or in registers
    (let* ((reg/stk            (partition-args args *tcall*))
           (reg-args           (car reg/stk))
           (stk-args           (cdr reg/stk))

           ;; calculate initial size of frame for callee (stack-based-args + return address)
           (callee-frame-size  (* 8 (+ 1 (length args))))
           (rsp                (mc-cxt-alloc-vreg cxt 'rsp        'rsp #f))
           (tmp1               (mc-cxt-alloc-vreg cxt (gensym 't)  #f  #f))
           (tmp2               (mc-cxt-alloc-vreg cxt (gensym 't)  #f  #f))
           (cxt-n-stack-args   (num-stack-args cxt))

           ;; shuffle register-based args into the standard argument-passing regs
           (uses/defs          (shuf-args blk reg-args '() *tcall*))
           (hregs-used         (car uses/defs)))

      ;; Cases:
      ;;   Neither caller and callee have any stack-based args
      ;;     Simply deallocate the space allocated for caller's locals and jump to callee
      ;;   Caller has stack-based args and callee does not
      ;;     setup argument registers, save return addr in tmp, pop caller's frame and then put return addr back on stack
      ;;   Else
      ;;     Then perform the full generalized setup for tail-calls

      (cond
       ((and (null? stk-args)
             (= 0 cxt-n-stack-args)
             (mc-disp? tgt))
        (emit-x86-64 blk
          ;; deallocate locals
          (add.i32r #f rsp
            (attrs (replace (list 1 'frame-size-locals))))
          ;; jmp to target
          (jmp.d tgt
            (attrs (uses hregs-used)))))
       ((and (null? stk-args)
             (= 0 cxt-n-stack-args)
             (mc-temp? tgt))
        (emit-x86-64 blk
          ;; fetch target addr into tmp1
          (mov.mr tgt tmp1
          ;; deallocate caller's locals
          (add.i32r #f rsp
            (attrs (replace (list 1 'frame-size-locals))))
          ;; jump to addr in tmp1
          (jmp.r tmp1
            (attrs (uses hregs-used))))))
       ((and (null? stk-args)
             (mc-disp? tgt))
        (emit-x86-64 blk
          ;; save return addr into tmp1
          (mov.mdr rsp #f
            (attrs (replace (list 2 'frame-return-address-offset))))
          ;; pop frame
          (add.i32r #f rsp
            (attrs (replace (list 1 'frame-size))))
          ;; push tmp1, thus creating an initial frame for callee
          (push.r tmp1)
          ;; jmp to target
          (jmp.d tgt
            (attrs (uses hregs-used)))))
       ((and (null? stk-args)
             (mc-temp? tgt))
        (emit-x86-64 blk
          ;; save return addr into tmp1
          (mov.mdr rsp #f
            (attrs (replace (list 2 'frame-retaddr-offset))))
          ;; fetch target addr into tmp2
          (mov.mr tgt tmp2
          ;; pop frame
          (add.i32r #f rsp
            (attrs (replace (list 1 'frame-size))))
          ;; push tmp1, thus creating an initial frame for callee
          (push.r tmp1)
          ;; jmp to addr in tmp2
          (jmp.r tmp2
            (attrs (uses hregs-used))))))
       (else
        ;; if jumping via a temp copy target addr into tmp1
        (if (mc-vreg? tgt)
            (emit-x86-64 blk
              (mov.mr tgt tmp1)))

        ;; subtract size from rsp to allocate scratch space for frame
        (emit-x86-64 blk
          (sub.i32r (imm i32 callee-frame-size) rsp))

        ;; write stack-based args into the new frame
        (let loop ((arg* stk-args) (offs 8))
          (match arg*
            (() '())
            ((arg . arg*)
             (cond
              ((mc-vreg? arg)
               (emit-x86-64 blk
                 (mov.rmd arg rsp (- callee-frame-size offs)
                   (attrs (offset callee-frame-size)))))
              ((mc-imm? arg)
               (emit-x86-64 blk
                 (mov.i64r imm tmp2)
                 (mov.rmd tmp2 rsp (- callee-frame-size offs))))
              (else (assert-not-reached)))
             (loop arg* (+ offs 8)))))

        ;; write return addr into the new frame
        (emit-x86-64 blk
          (mov.mdr rsp #f tmp2
            (attrs (replace 2 'frame-retaddr-offset)
                   (offset callee-frame-size)))
          (mov.rm tmp2 rsp))

        ;; now copy the prepared frame into the current, active frame. Edgy stuff!
        (let loop ((arg* args) (offs 8))
          (match arg*
            (() #t)
            ((arg . arg*)
             (emit-x86-64 blk
               (mov.mdr rsp (- size offs) tmp2)
               (mov.rmd tmp2 rsp #f
                 (attrs (replace (list 3 'frame-offset offs))
                        (offset  callee-frame-size))))))
          (loop arg* (+ offs 8)))

        (emit-x86-64 blk
          ;; make rsp point to moved return addr
          (mov.mdr rsp #f tmp1
            (attrs (replace (list 2 'frame-offset callee-frame-size))
                   (offset  callee-frame-size))))

        (cond
         ((mc-vreg? tgt)
          (emit-x86-64 blk
            (jmp.r tmp1
                   (attrs (uses hregs-used)))))
         ((mc-disp? tgt)
          (emit-x86-64 blk
            (jmp.d tgt
                   (attrs (uses hregs-used))))))))))


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
      (($ tr-instr 'return _ ($ tr-instr 'call _ tgt args))
       (emit-tcall-x86-64 block tgt args #f))
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
           (else (assert-not-reached)))))
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
