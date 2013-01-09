(declare (unit arch-x86-64)
         (uses globals helpers tree machine spec-x86-64 rules-x86-64))

(module arch-x86-64 *

  (import scheme)
  (import chicken)

  (use matchable)
  (use srfi-1)

  (import globals)
  (import helpers)
  (import tree    (prefix tree tree-))
  (import machine (prefix machine mc-))

  (import spec-x86-64)
  (import rules-x86-64)
  (import arch-syntax)
  (import munch-syntax)

  (define *regs*  '(rax rbx rcx rdx rdi rsi r8 r9 r10 r11 r12 r13 r14 r15))

  ;; Standard C calling convention
  (define *ccall*
    '((param        (rdi rsi rdx rcx r8 r9))
      (ret          (rax))
      (callee-save  (rbp rbx r12 r13 r14 r15))
      (caller-save  (rax rcx rdx rdi rsi r8 r9 r10 r11))))

    ;; (rbp rbx r12 r13 r14 r15)

  ;; Our calling convention (non-TCO)
  (define *scall*
    '((param        (rdi rsi r8 r9 r10 r11 r12 r13 r14 r15))
      (ret          (rax))
      (callee-save  ())
      (caller-save  (rbp rax rbx rcx rdx rdi rsi r8 r9 r10 r11 r12 r13 r14 r15))))

  (define (cc-param cc)
    (match cc
           ((('param x) _ _ _)
            x)))

  (define (cc-ret cc)
    (match cc
           ((_ ('ret x) _ _)
            x)))

  (define (cc-callee-save cc)
    (match cc
           ((_ _ ('callee-save x) _)
            x)))

  (define (cc-caller-save cc)
    (match cc
           ((_ _ _ ('caller-save x))
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

  (define *word-size* 8)
  (define *word-size-bits (* 8 *word-size*))

  ;; (define (frame-size cxt)
  ;;   (let ((frm (mc-cxt-frm cxt)))
  ;;     (* *word-size*
  ;;        (+ 1
  ;;           (length (frm-locals frm))
  ;;           (length (frm-params frm))))))

  ;; (define (frame-size-locals cxt)
  ;;   (* *word-size*
  ;;      (length (frm-locals (mc-cxt-frm cxt)))))

  ;; (define (frame-size-params cxt)
  ;;   (* *word-size*
  ;;      (length (frm-params (mc-cxt-frm cxt)))))

  ;; (define (frame-offset cxt offset add-to-offset)
  ;;   (+ add-to-offset (- (frame-size cxt) offset)))

  ;; (define (frame-ret-addr-offset cxt)
  ;;   #f)

  (define (add-local cxt vreg slot)
    (cxt-stk-locals-set! (cons (cons vreg slot) (cxt-stk-locals cxt))))

  (define (split-reg/stk arg*)
    (split-reg/stk-cc arg* *scall*))

  (define (split-reg/stk-cc arg* cc)
    (let ((reg* (cc-param cc)))
      (let loop ((reg* reg*) (arg* arg*) (x '()))
        (match reg*
          (() (cons (reverse x) arg*))
          ((reg . reg*)
           (match arg*
             (()
              (cons (reverse x) '()))
             ((arg . arg*)
              (loop reg* arg* (cons arg x)))))))))

  ;;
  ;; Arguments passed to a context are constrained to registers r8 ... r15.
  ;; Since we want to minimise the length of pre-colored live ranges, we move the args into temps.
  ;;
  ;; So we insert moves to copy each constrained arg into an unconstrained vreg, and add
  ;; hints so that the allocator can eliminate the move if possible
  ;;
  (define (make-context name params mod)
    (let* ((cxt         (mc-make-cxt name '() '() '() '() '()))
           (reg/stk     (split-reg/stk params))
           (stk-args    (map (lambda (param)
                               (mc-cxt-alloc-vreg cxt param #f #f))
                             (cdr reg/stk)))
           (reg-args    (map (lambda (arg hreg)
                               (mc-cxt-alloc-vreg cxt hreg hreg #f))
                             (car reg/stk)
                             (cc-param *scall*)))
           (tmp*        (map (lambda (param hint)
                               (mc-cxt-alloc-vreg cxt param #f hint))
                             (car reg/stk)
                             (cc-param *scall*)))
           (blk         (mc-blk-make cxt name)))

      (cxt-hreg-params-set! cxt reg-args)
      (cxt-stk-params-set!  cxt stk-args)

      ;; set home location in stack for each stack-homed vreg
      (let loop ((vreg* stk-args) (i 0))
        (cond
         ((not (null? vreg*))
          (mc-vreg-slot-set! (car vreg*) i)
          (loop (cdr vreg*) (+ 1 i)))))

      ;; insert move from each arg into a tmp
      (for-each (lambda (arg tmp)
                  (emit-x86-64 blk
                    (mov.rr arg tmp)))
                reg-args tmp*)

      ;; allocate space for locals
      (emit-x86-64 blk
        (sub.i32r #f (hreg-ref cxt 'rsp)
          (attrs (replace (list 1 'frame-size-locals)))))

      (mc-cxt-strt-set! cxt blk)
      (mc-mod-cxts-set! mod (cons cxt (mc-mod-cxts mod)))
      cxt))

  ;; (define (spill-stack-params cxt)
  ;;   (let ((stk-params (mc-cxt-stk-params cxt)))
  ;;     (cond
  ;;      ((and (mc-inst-is-read? instr vreg) (mc-inst-is-written? instr vreg))
  ;;       ;; replace vreg with tmp in instruction
  ;;       (mc-inst-replace-vreg instr vreg tmp)
  ;;       (mc-blk-insert (mc-inst-blk instr) instr 'before
  ;;                      (emit-x86-64 #f
  ;;                                   (mov.mdr rbp disp vreg)))
  ;;       (mc-blk-insert (mc-inst-blk instr) instr 'after
  ;;                      (emit-x86-64 #f
  ;;                                   (mov.rmd vreg rbp (mc-disp-make (* 8 index)))))))
  ;;      ((mc-inst-is-read? instr vreg)
  ;;       (let ((disp (mc-disp-make (* 8 index))))
  ;;         (mc-inst-replace-vreg instr vreg tmp)
  ;;         (mc-blk-insert (mc-inst-blk instr) instr 'before
  ;;                        (emit-x86-64 #f
  ;;                                     (mov.mdr rbp disp vreg)))))
  ;;      ((mc-inst-is-written? instr vreg)
  ;;       (let ((disp (mc-disp-make (* 8 index))))
  ;;         (mc-inst-replace-vreg instr vreg tmp)
  ;;         (mc-blk-insert (mc-inst-blk instr) instr 'after
  ;;                        (emit-x86-64
  ;;                         (mov.rmd vreg rbp disp)))))
  ;;      (else (assert-not-reached)))

  (define (operand-format-x86-64 op)
    (cond
     ((mc-vreg? op)
      (format "~s" (mc-vreg-name op)))
     ((mc-imm? op)
      (format "~s" (mc-imm-value op)))
     ((mc-disp? op)
      (format "~s" (mc-disp-value op)))
     (else '<>)))

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
    (let ((stack-args-size (* *word-size*
                              (length (mc-cxt-stk-params cxt)))))
      (emit-x86-64 blk
        (add.i32r #f (hreg-ref cxt 'rsp)
          (attrs (replace (list 1 'frame-size-locals))))
        (ret.i16 (imm i16 stack-args-size)))))

  ;;
  ;; Analyzes which hregs are used to pass parameters and which hregs are clobbered (defined)
  ;;
  ;; Returns a pair:
  ;;   hregs which will be used to pass parameters
  ;;   hregs which will be clobbered

  (define (analyze-hregs blk args cc)
    (let* ((uses      (let loop ((arg*   args)
                                  (hreg*  (cc-param cc))
                                  (x     '()))
                           (match arg*
                             (() x)
                             ((arg . arg*)
                              (loop arg* (cdr hreg*) (cons (car hreg*) x)))))))
      (cons
       (map (lambda (hreg)
              (mc-cxt-alloc-vreg (mc-blk-cxt blk) hreg hreg #f))
            uses)
       (map (lambda (hreg)
              (mc-cxt-alloc-vreg (mc-blk-cxt blk) hreg  hreg #f))
            (cc-caller-save cc)))))

  (define (write-moves blk args regs)
    ;; generate each move
    (for-each
     (lambda (arg reg)
       (cond
        ((mc-vreg? arg)
         (emit-x86-64 blk
           (mov.rr arg reg)))
        ((mc-imm? arg)
         (emit-x86-64 blk
           (mov.i64r arg reg)))
        (else (print arg) (assert-not-reached))))
     args regs))

  (define (convert cxt node)
    (cond
     ((tree-temp? node)
      (mc-cxt-alloc-vreg cxt (tree-temp-name node)))
     ((tree-constant? node)
      (mc-imm-make (tree-constant-size node) (tree-constant-value node)))
     ((tree-label? node)
      (mc-disp-make (tree-label-name node)))
     (else (print node) (assert-not-reached))))

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

  (define (emit-ccall-x86-64 cxt blk tgt args dst)
    (let* ((rax         (hreg-ref cxt 'rax))
           (rsp         (hreg-ref cxt 'rsp))
           (tmp1        (mc-cxt-alloc-vreg cxt (gensym 't)  #f  #f))

           ;; split args into register-based args and stack-based args
           (reg/stk     (split-reg/stk-cc args *ccall*))
           (reg-args    (car reg/stk))
           (stk-args    (cdr reg/stk))
           (init-frame-size (* *word-size* (length stk-args)))

           (uses/defs   (analyze-hregs blk args *ccall*))
           (hregs-used  (car uses/defs))
           (hregs-defs  (cdr uses/defs)))

      ;; shuffle args into argument-passing registers
      (write-moves blk reg-args hregs-used)

      ;; setup initial stack frame and write stack-based args
      (cond
       ((not (null? stk-args))
        ;; allocate space
        (emit-x86-64 blk
                     (sub.i32r (imm i32 init-frame-size) rsp))

        ;; write stack-based params
        (let loop ((arg* stk-args) (offs 0))
          (match arg*
            (() '())
            ((arg . arg*)
             (cond
              ((mc-vreg? arg)
               (emit-x86-64 blk
                 (mov.rmd arg rsp (disp (- init-frame-size offs))
                   (attrs (offset init-frame-size)))))
              ((mc-imm? arg)
               (emit-x86-64 blk
                 (mov.i64r arg tmp1)
                 (mov.rmd tmp1 rsp (disp (- init-frame-size offs)))))
              (else (assert-not-reached)))
             (loop arg* (+ offs 8)))))))

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

      ;; Move return value to dst
      (if dst
          (emit-x86-64 blk
            (mov.rr rax dst)))))


  (define (emit-scall-x86-64 cxt blk tgt args dst)
    (let* ((rax         (hreg-ref cxt 'rax))
           (rsp         (hreg-ref cxt 'rsp))
           (tmp1        (mc-cxt-alloc-vreg cxt (gensym 't)  #f  #f))

           ;; split args into register-based args and stack-based args
           (reg/stk     (split-reg/stk-cc args *scall*))
           (reg-args    (car reg/stk))
           (stk-args    (cdr reg/stk))
           (init-frame-size (* *word-size* (length stk-args)))

           (uses/defs   (analyze-hregs blk reg-args *scall*))
           (hregs-used  (car uses/defs))
           (hregs-defs  (cdr uses/defs)))

      ;; shuffle args into argument-passing registers
      (write-moves blk reg-args hregs-used)

      ;; setup initial stack frame and write stack-based args
      (cond
       ((not (null? stk-args))
        ;; allocate space
        (emit-x86-64 blk
          (sub.i32r (imm i32 init-frame-size) rsp))

        ;; write stack-based params
        (let loop ((arg* stk-args) (offs 0))
          (match arg*
            (() '())
            ((arg . arg*)
             (cond
              ((mc-vreg? arg)
               (emit-x86-64 blk
                 (mov.rmd arg rsp (disp (- init-frame-size offs))
                   (attrs (offset init-frame-size)))))
              ((mc-imm? arg)
               (emit-x86-64 blk
                 (mov.i64r arg tmp1)
                 (mov.rmd tmp1 rsp (disp (- init-frame-size offs)))))
              (else (assert-not-reached)))
             (loop arg* (+ offs 8)))))))

      ;; emit call
      (cond
       ((mc-disp? tgt)
        (emit-x86-64 blk
          (call.d tgt
            (attrs (uses   hregs-used)
                   (defs   (append hregs-used hregs-defs))
                   (offset init-frame-size)))))
       ((mc-vreg? tgt)
        (emit-x86-64 blk
          (call.r tgt
            (attrs (uses   hregs-used)
                   (defs   (append hregs-used hregs-defs))
                   (offset init-frame-size)))))
       (else (assert-not-reached)))
      ;; Move return value to dst
      (emit-x86-64 blk
        (mov.rr rax dst))))

  (define (emit-tail-scall-x86-64 cxt blk tgt args)
    ;; determine which args are passed on the stack or in registers
    (let* ((reg/stk            (split-reg/stk-cc args *scall*))
           (reg-args           (car reg/stk))
           (stk-args           (cdr reg/stk))

           ;; calculate initial size of frame for callee (stack-based-args + return address)
           (callee-frame-size  (* *word-size*
                                  (+ 1
                                     (length stk-args))))
           (rsp                (hreg-ref cxt 'rsp))
           (tmp1               (mc-cxt-alloc-vreg cxt (gensym 't)  #f  #f))
           (tmp2               (mc-cxt-alloc-vreg cxt (gensym 't)  #f  #f))
           (n-stack-args       (length (mc-cxt-stk-params cxt)))

           ;; shuffle register-based args into the standard argument-passing regs
           (uses/defs          (analyze-hregs blk reg-args *scall*))
           (hregs-used         (car uses/defs)))

      (write-moves blk reg-args hregs-used)
      ;; Cases:
      ;;   Neither caller and callee have any stack-based args
      ;;     Simply deallocate the space allocated for caller's locals and jump to callee
      ;;   Caller has stack-based args and callee does not
      ;;     setup argument registers, save return addr in tmp, pop caller's frame and then put return addr back on stack
      ;;   Else
      ;;     Then perform the full generalized setup for tail-calls

      (cond
       ((and (null? stk-args)
             (= 0 n-stack-args)
             (mc-disp? tgt))
        (emit-x86-64 blk
          ;; deallocate locals
         (add.i32r #f rsp
            (attrs (replace (list 1 'frame-size-locals))))
          ;; jmp to target
          (jmp.d tgt
            (attrs (uses hregs-used)))))
       ((and (null? stk-args)
             (= 0 n-stack-args)
             (mc-vreg? tgt))
        (emit-x86-64 blk
          ;; fetch target addr into tmp1
          (mov.mr tgt tmp1)
          ;; deallocate caller's locals
          (add.i32r #f rsp
            (attrs (replace (list 1 'frame-size-locals))))
          ;; jump to addr in tmp1
          (jmp.r tmp1
            (attrs (uses hregs-used)))))
       ((and (null? stk-args)
             (mc-disp? tgt))
        (emit-x86-64 blk
          ;; save return addr into tmp1
          (mov.mdr rsp #f
            (attrs (replace (list 2 'frame-ret-addr-offset))))
          ;; pop frame
          (add.i32r #f rsp
            (attrs (replace (list 1 'frame-size))))
          ;; push tmp1, thus creating an initial frame for callee
          (push.r tmp1)
          ;; jmp to target
          (jmp.d tgt
            (attrs (uses hregs-used)))))
       ((and (null? stk-args)
             (mc-vreg? tgt))
        (emit-x86-64 blk
          ;; save return addr into tmp1
          (mov.mdr  #f
            (attrs (replace (list 2 'frame-ret-addr-offset))))
          ;; fetch target addr into tmp2
          (mov.mr tgt tmp2)
          ;; pop frame
          (add.i32r #f rsp
            (attrs (replace (list 1 'frame-size))))
          ;; push tmp1, thus creating an initial frame for callee
          (push.r tmp1)
          ;; jmp to addr in tmp2
          (jmp.r tmp2
            (attrs (uses hregs-used)))))
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
                 (mov.rmd arg rsp (disp (- callee-frame-size offs))
                   (attrs (offset callee-frame-size)))))
              ((mc-imm? arg)
               (emit-x86-64 blk
                 (mov.i64r arg tmp2)
                 (mov.rmd tmp2 rsp (disp (- callee-frame-size offs)))))
              (else (print arg) (assert-not-reached)))
             (loop arg* (+ offs 8)))))

        ;; write return addr into the new frame
        (emit-x86-64 blk
          (mov.mdr rsp #f tmp2
            (attrs (replace (list 2 'frame-ret-addr-offset))
                   (offset  callee-frame-size)))
          (mov.rm tmp2 rsp))

        ;; now copy the prepared frame into the current, active frame. Edgy stuff!
        (let loop ((arg* stk-args) (offs 8))
          (match arg*
            (() #t)
            ((arg . arg*)
             (emit-x86-64 blk
               (mov.mdr rsp (disp (- callee-frame-size offs)) tmp2)
               (mov.rmd tmp2 rsp #f
                 (attrs (replace (list 3 'frame-offset offs))
                        (offset  callee-frame-size))))
             (loop arg* (+ offs 8)))))

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
    (let* ((cxt  (mc-make-cxt '__scheme_exec '() '() '() '() '())))

      (mc-cxt-strt-set! cxt (mc-blk-make cxt '__scheme_exec))

      (emit-x86-64 (mc-cxt-strt cxt)
        (jmp.d   (disp 'begin)))

      (mc-mod-cxts-set! mod (cons cxt (mc-mod-cxts mod)))

      cxt))

  ;; Selects x86-64 instructions for a tree node
  ;;
  (define (emit-stm blk stm)
    (let ((cxt (mc-blk-cxt blk)))
      (tree-match stm
        ((return (scall tgt args))
         (emit-tail-scall-x86-64 cxt blk
                                 (convert cxt tgt)
                                 (map (lambda (v) (convert cxt v)) args)))
        ((return (ccall tgt args))
         (let ((tmp (mc-cxt-alloc-vreg cxt (gensym 't))))
           (emit-ccall-x86-64 cxt blk
                              (convert cxt tgt)
                              (map (lambda (v) (convert cxt v)) args)
                              tmp)
           (emit-return-x86-64 cxt blk tmp)))
        ((return val)
         (emit-return-x86-64 cxt blk
                             (convert cxt val)))
        ((ccall tgt args)
         (emit-ccall-x86-64 cxt blk
                            (convert tgt)
                            (map (lambda (v) (convert cxt v)) args)
                            #f))
        ((assign dst (ccall tgt args))
         (emit-ccall-x86-64 cxt blk
                            (convert cxt tgt)
                            (map (lambda (v) (convert cxt v)) args)
                            (mc-cxt-alloc-vreg cxt dst)))
        ((assign dst (scall tgt args))
         (emit-scall-x86-64 cxt blk
                            (convert cxt tgt)
                            (map (lambda (v) (convert cxt v)) args)
                            (mc-cxt-alloc-vreg cxt dst)))
        (else
         (munch-x86-64 blk stm)))))

  (define <arch-x86-64> (make-arch-descriptor
                           make-context
                           operand-format-x86-64
                           vregs-read-x86-64
                           vregs-written-x86-64
                           generate-bridge-context-x86-64
                           emit-stm))

)
