(declare (unit arch-x86-64)
         (uses globals helpers tree machine spec-x86-64 rules-x86-64))

(module arch-x86-64 *

  (import scheme)
  (import chicken)
  (import extras)

  (use matchable)
  (use srfi-1)
  (use srfi-69)

  (import globals)
  (import helpers)
  (import tree    (prefix tree tree-))
  (import machine (prefix machine mc-))

  (import spec-x86-64)
  (import rules-x86-64)
  (import arch-syntax)
  (import munch-syntax)

  (define-struct cc-spec (param ret callee-save caller-save))

   ;; Standard C calling convention
  (define *ccall-spec*
    '((param        (rdi rsi rdx rcx r8 r9))
      (ret          (rax))
      (callee-save  (rbp rbx r12 r13 r14 r15))
      (caller-save  (rax rcx rdx rdi rsi r8 r9 r10 r11))))

  ;; Our calling convention (non-TCO)
  (define *scall-spec*
    '((param        (rdi rsi r8 r9 r10 r11 r12 r13 r14 r15))
      (ret          (rax))
      (callee-save  ())
      (caller-save  (rbp rax rbx rcx rdx rdi rsi r8 r9 r10 r11 r12 r13 r14 r15))))

  (define *hregs*           #f)
  (define *hreg-map*        (make-hash-table eq? symbol-hash 24))

  (define *word-size*       8)
  (define *word-size-bits*  (* 8 *word-size*))

  (define *scall*           #f)
  (define *ccall*           #f)

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

  (define (init-hreg-table)
    (let ((tbl (make-vector (length *x86-64-registers*))))
      (let loop ((hregs *x86-64-registers*) (i 0))
        (cond
         ((null? hregs) tbl)
         (else
          (match (car hregs)
            ((name (flag* ...))
             (let* ((reserved (and (memq 'reserved flag*) #t))
                    (vreg     (mc-make-vreg (+ 1024 i) name reserved #f #f '())))
               (vector-set! tbl i vreg)
               (hash-table-set! *hreg-map* name (vector-ref tbl i))
               (loop (cdr hregs) (+ i 1))))))))))

  (define (make-cc-table tmpl)
    (define (->hreg name)
      (hash-table-ref *hreg-map* name))
    (make-cc-spec
      (map ->hreg (cadr (list-ref tmpl 0)))
      (map ->hreg (cadr (list-ref tmpl 1)))
      (map ->hreg (cadr (list-ref tmpl 2)))
      (map ->hreg (cadr (list-ref tmpl 3)))))

  (define (init-x86-64)
    (set! *hregs* (init-hreg-table))
    (set! *ccall* (make-cc-table *ccall-spec*))
    (set! *scall* (make-cc-table *scall-spec*))

    ;; set globals (globals.scm)
    (set! *hreg-start-id*  1024)
    (set! *hreg-table*     *hregs*))

  (define (hreg-ref name)
    (hash-table-ref *hreg-map* name))

  (define (build-bitset cxt)
    (let ((size (length (mc-cxt-vrgs-count cxt))))
      #t))

  (define (frame-size-locals cxt)
    (* *word-size*
       (length (mc-cxt-stk-locals cxt))))

  (define (frame-size-params cxt)
    (* *word-size*
       (length (mc-cxt-stk-params cxt))))

  (define (frame-size cxt)
    (+ *word-size* (frame-size-locals cxt) (frame-size-params cxt)))

  (define (frame-offset cxt offset add-to-offset)
    (+ add-to-offset (- (frame-size cxt) offset)))

  (define (frame-ret-addr-offset cxt)
    (frame-size-params cxt))

  (define (add-local cxt vreg slot)
    (cxt-stk-locals-set! (cons (cons vreg slot) (cxt-stk-locals cxt))))

  (define (split-reg/stk arg*)
    (split-reg/stk-cc arg* *scall*))

  (define (split-reg/stk-cc arg* cc)
    (let ((reg* (cc-spec-param cc)))
      (let loop ((reg* reg*) (arg* arg*) (x '()))
        (match reg*
          (() (cons (reverse x) arg*))
          ((reg . reg*)
           (match arg*
             (()
              (cons (reverse x) '()))
             ((arg . arg*)
              (loop reg* arg* (cons arg x)))))))))

  (define (make-vreg-ref-p cxt)
    (let* ((tbl (make-hash-table eq? symbol-hash 40)))
      (lambda (name)
        (let ((vreg (hash-table-ref/default tbl name #f)))
          (if vreg
              vreg
              (let ((vreg (mc-vreg-alloc cxt)))
                (hash-table-set! tbl name vreg)
                vreg))))))

  ;;
  ;; Arguments passed to a context are constrained to registers r8 ... r15.
  ;; Since we want to minimise the length of pre-colored live ranges, we move the args into temps.
  ;;
  ;; So we insert moves to copy each constrained arg into an unconstrained vreg, and add
  ;; hints so that the allocator can eliminate the move if possible
  ;;
  (define (make-context name params mod)
    (let* ((cxt         (mc-make-cxt name '() '() 0 '() '() '()))
           (vreg-ref-p  (make-vreg-ref-p cxt))
           (reg/stk     (split-reg/stk params))
           (stk-args    (map (lambda (param)
                               (vreg-ref-p param))
                             (cdr reg/stk)))
           (reg-args    (map (lambda (arg hreg)
                               hreg)
                             (car reg/stk)
                             (cc-spec-param *scall*)))
           (tmp*        (map (lambda (param)
                               (vreg-ref-p param))
                             (car reg/stk)))
           (blk         (mc-blk-make cxt name)))

      (cxt-hreg-params-set! cxt reg-args)
      (cxt-stk-params-set!  cxt stk-args)

      ;; set home location in stack for each stack-homed vreg
      (let loop ((vreg* stk-args) (i 0))
        (cond
         ((not (null? vreg*))
          (mc-vreg-slot-set! (car vreg*) i)
          (loop (cdr vreg*) (+ 1 i)))))

      (emit-x86-64 blk
        (sub.i32r #f (hreg-ref 'rsp)
          (attrs (replace (list 1 'frame-size-locals)))))

      ;; insert move from each arg into a tmp
      (for-each (lambda (arg tmp)
                  (emit-x86-64 blk
                    (mov.rr arg tmp)))
                reg-args tmp*)

      (mc-cxt-strt-set! cxt blk)
      (mc-mod-cxts-set! mod (cons cxt (mc-mod-cxts mod)))

      (cons cxt vreg-ref-p)))

  (define (operand-format-x86-64 op)
    (cond
     ((mc-vreg? op)
      (if (mc-vreg-hreg op)
          (symbol->string (mc-vreg-hreg op))
          (format "t~s" (mc-vreg-id op))))
     ((mc-imm? op)
      (format "~s" (mc-imm-value op)))
     ((mc-disp? op)
      (format "~s" (mc-disp-value op)))
     (else '<>)))

  (define (filter-reserved vreg*)
    (let loop ((vreg* vreg*) (x '()))
      (if (null? vreg*)
          x
          (let ((vreg (car vreg*)))
            (if (and (mc-vreg-hreg vreg)
                     (mc-vreg-reserved vreg))
                (loop (cdr vreg*) x)
                (loop (cdr vreg*) (cons vreg x)))))))

  (define (vreg-uses-x86-64 instr)
    (let* ((ops   (mc-inst-ops instr))
           (vreg* ((mc-spec-reads (mc-inst-spec instr)) ops)))
      (filter-reserved vreg*)))

  (define (vreg-defs-x86-64 instr)
    (let* ((ops   (mc-inst-ops instr))
           (vreg* ((mc-spec-writes (mc-inst-spec instr)) ops)))
      (filter-reserved vreg*)))

  ;; Generate x86-64 code for the 'return' instruction
  ;;
  (define (emit-return-x86-64 cxt blk val)
    ;; move return value into %rax
    (cond
     ((mc-imm? val)
      (case (mc-imm-size val)
        ((i64)
         (emit-x86-64 blk
           (mov.i64r val (hreg-ref 'rax))))
        (else (assert-not-reached))))
     ((mc-vreg? val)
      (emit-x86-64 blk
        (mov.rr val (hreg-ref 'rax)))))
    ;;  deallocate locals, and issue return
    (let ((stack-args-size (* *word-size*
                              (length (mc-cxt-stk-params cxt)))))
      (emit-x86-64 blk
        (add.i32r #f (hreg-ref 'rsp)
          (attrs (replace (list 1 'frame-size-locals))))
        (ret.i16 (imm i16 stack-args-size)))))

  ;;
  ;; Analyzes which hregs are used to pass parameters and which hregs are clobbered (defined)
  ;;
  ;; Returns a pair:
  ;;   hregs which will be used to pass parameters
  ;;   hregs which will be clobbered

  (define (analyze-hregs cxt args cc)
    (let* ((uses      (let loop ((arg*   args)
                                 (hreg*  (cc-spec-param cc))
                                 (x     '()))
                           (match arg*
                             (() x)
                             ((arg . arg*)
                              (loop arg* (cdr hreg*) (cons (car hreg*) x)))))))
      (cons uses (cc-spec-caller-save cc))))

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

  (define (convert node vreg-ref)
    (cond
     ((tree-temp? node)
      (vreg-ref (tree-temp-name node)))
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
    (let* ((rax         (hreg-ref 'rax))
           (rsp         (hreg-ref 'rsp))
           (tmp1        (mc-vreg-alloc cxt))

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
                   (defs hregs-defs)))))
       (else (assert-not-reached)))

      ;; Move return value to dst
      (if dst
          (emit-x86-64 blk
            (mov.rr rax dst)))))


  (define (emit-scall-x86-64 cxt blk tgt args dst)
    (let* ((rax         (hreg-ref 'rax))
           (rsp         (hreg-ref 'rsp))
           (tmp1        (mc-vreg-alloc cxt))

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
                   (defs   hregs-defs)
                   (offset init-frame-size)))))
       ((mc-vreg? tgt)
        (emit-x86-64 blk
          (call.m  tgt
            (attrs (uses   hregs-used)
                   (defs   hregs-defs)
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
           (rsp                (hreg-ref 'rsp))
           (tmp1               (mc-vreg-alloc cxt))
           (tmp2               (mc-vreg-alloc cxt))
           (n-stack-args       (length (mc-cxt-stk-params cxt)))

           (uses/defs          (analyze-hregs blk reg-args *scall*))
           (hregs-used         (car uses/defs)))

      ;; shuffle register-based args into the standard argument-passing regs
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
    (let* ((cxt  (mc-make-cxt '__scheme_enter '() '() 0 '() '() '())))

      (mc-cxt-strt-set! cxt (mc-blk-make cxt '__scheme_exec))

      (emit-x86-64 (mc-cxt-strt cxt)
        (jmp.d   (disp 'begin)))

      (mc-mod-cxts-set! mod (cons cxt (mc-mod-cxts mod)))

      cxt))

  ;; stack map
  ;;
  ;;
  ;; param1
  ;; param2
  ;; ret
  ;; local1
  ;;

  ;; The post register-allocation rewrite pass
  ;;
  ;; Responsibilities
  ;;   * Add loads/stores for spilled temporaries
  ;;   * Substitute the proper offsets from RSP for accessing stack-based locals and parameters
  ;;

  (define (assign-stack-slot cxt vreg)
    (if (mc-vreg-slot vreg)
        vreg
        (let ((slot (+ (length (mc-cxt-stk-params cxt))
                       1
                       (length (mc-cxt-stk-locals cxt)))))
          (mc-cxt-stk-locals-set! cxt (append (mc-cxt-stk-locals cxt) (list vreg)))
          (mc-vreg-slot-set! vreg slot)
          vreg)))

  (define (use-type instr vreg)
    (let ((r (mc-vreg-use? vreg instr))
          (w (mc-vreg-def? vreg instr)))
      (cond
       ((and r w) 'rw)
       (r 'r)
       (w 'w)
       (else (assert-not-reached)))))

  (define (rewrite cxt asn spd)

    (hash-table-for-each spd
      (lambda (vreg spills)
        (assign-stack-slot cxt vreg)))

    (hash-table-for-each spd
      (lambda (vreg spill*)
        (let loop ((spill* spill*))
          (if (not (null? spill*))
              (match (car spill*)
                ((tmp . instr)
                 (spill-for-use cxt vreg tmp instr)
                 (loop (cdr spill*))))))))

    (write-assigns cxt asn))

  (define (stack-offset cxt vreg)
    (let* ((frm-size    (* *word-size*
                           (+ (length (mc-cxt-stk-params cxt))
                              1
                              (length (mc-cxt-stk-locals cxt)))))
           (slot-offset (* *word-size*
                           (+ 1 (mc-vreg-slot vreg))))
           (offset      (- frm-size slot-offset)))
      (mc-make-disp offset)))

  (define (spill-for-use cxt vreg tmp instr)
   (let ((rsp  (hreg-ref 'rsp))
         (offs (stack-offset cxt vreg)))
     (case (use-type instr vreg)
       ((rw)
        (mc-inst-replace-vreg instr vreg tmp)
        (mc-blk-insert (mc-inst-blk instr) instr 'before
          (emit-x86-64 #f
            (mov.mdr rsp offs tmp)))
        (mc-blk-insert (mc-inst-blk instr) instr 'after
          (emit-x86-64 #f
            (mov.rmd tmp rsp offs))))
       ((r)
        (mc-inst-replace-vreg instr vreg tmp)
        (mc-blk-insert (mc-inst-blk instr) instr 'before
          (emit-x86-64 #f
            (mov.mdr rsp offs tmp))))
       ((w)
        (mc-inst-replace-vreg instr vreg tmp)
          (mc-blk-insert (mc-inst-blk instr) instr 'after
            (emit-x86-64 #f
              (mov.rmd tmp rsp offs))))
       (else (assert-not-reached)))))

  (define (write-assigns cxt asn)
    (blk-for-each
      (lambda (blk)
        (inst-for-each
          (lambda (inst)
            (let loop ((ops (mc-inst-ops inst)))
              (if (not (null? ops))
                  (let ((op (car ops)))
                    (if (and (mc-vreg? op) (not (mc-vreg-hreg op)))
                        (set-car! ops (hash-table-ref/default asn op #f)))
                    (loop (cdr ops)))))
            (rewrite-stack cxt inst))
          blk))
      cxt))

  ;; Selects x86-64 instructions for a tree node
  ;;
  (define (emit-stm cxt blk vreg-ref stm)
    (tree-match stm
      ((return (scall tgt args))
       (emit-tail-scall-x86-64 cxt blk
                               (convert tgt vreg-ref)
                               (map (lambda (v) (convert v vreg-ref)) args)))
      ((return (ccall tgt args))
       (let ((tmp (mc-vreg-alloc cxt)))
         (emit-ccall-x86-64 cxt blk
                            (convert tgt vreg-ref)
                            (map (lambda (v) (convert v vreg-ref)) args)
                            tmp)
         (emit-return-x86-64 cxt blk tmp)))
      ((return val)
       (emit-return-x86-64 cxt blk
                           (convert val vreg-ref)))
      ((ccall tgt args)
       (emit-ccall-x86-64 cxt blk
                          (convert tgt vreg-ref)
                          (map (lambda (v) (convert v vreg-ref)) args)
                          #f))
      ((assign dst (ccall tgt args))
       (emit-ccall-x86-64 cxt blk
                          (convert tgt vreg-ref)
                          (map (lambda (v) (convert v vreg-ref)) args)
                          (vreg-ref dst)))
      ((assign dst (scall tgt args))
       (emit-scall-x86-64 cxt blk
                          (convert tgt vreg-ref)
                          (map (lambda (v) (convert v vreg-ref)) args)
                          (vreg-ref dst)))
      (else
       (munch-x86-64 cxt blk vreg-ref stm))))

  (define (rewrite-stack cxt instr)
    (cond
     ((assq 'replace (mc-inst-attrs instr))
      => (lambda (pair)
           (match (cdr pair)
             ((index func param* ...)
              (replace-action cxt instr index func param*
                (or (assq 'offset (mc-inst-attrs instr)) 0))))))))

  (define (list-set! ls i val)
    (let loop ((x ls) (k 0))
      (if (null? x)
          (assert-not-reached)
          (if (= k i)
              (begin (set-car! x val) ls)
              (loop (cdr x) (+ k 1))))))

  (define (replace-action cxt instr index func param* local-offset)
    (pretty-print (list func index param*))
    (case func
      ((frame-offset)
       (list-set! (mc-inst-ops instr)
                  (- index 1)
                  (mc-make-disp (frame-offset cxt (car param*) local-offset))))
      ((frame-size-locals)
       (list-set! (mc-inst-ops instr)
                  (- index 1)
                  (mc-make-disp (frame-size-locals cxt))))
      (else #f)))

  (define (print-frame-info cxt port)
    (struct-case cxt
      ((cxt name strt vrgs vrgs-count hreg-params stk-params stk-locals)
       (let* ((size               (frame-size cxt))
              (stk-param-offsets  (map (lambda (vreg)
                                         (- size (* (+ 1 (mc-vreg-slot vreg)) *word-size*)))
                                       stk-params))
              (saved-rip-offset   (- size (* (+ 1 (length stk-params)) *word-size*)))
              (stk-local-offsets  (map (lambda (vreg)
                                         (- size (* (+ 1 (mc-vreg-slot vreg)) *word-size*)))
                                       stk-locals)))
         (fprintf port "  # context: ~s\n" name)
         (fprintf port "  #   num-args:  ~a\n" (+ (length hreg-params) (length stk-params)))
         (fprintf port "  #   reg-args:  ~a\n" (map (lambda (v) (mc-vreg-hreg v)) hreg-params))
         (fprintf port "  #   frame: size=~a\n" (frame-size cxt))
         (fprintf port "  #     params: count=~a\n" (length stk-params))
         (fprintf port "  #       rsp + ~a\n" stk-param-offsets)
         (fprintf port "  #     saved-eip:\n")
         (fprintf port "  #       rsp + ~a\n" saved-rip-offset)
         (fprintf port "  #     locals: count=~a\n" (length stk-locals))
         (fprintf port "  #       rsp + ~a\n" stk-local-offsets)))))



  (define <arch-x86-64> (make-arch-impl
                           init-x86-64
                           make-context
                           operand-format-x86-64
                           vreg-uses-x86-64
                           vreg-defs-x86-64
                           generate-bridge-context-x86-64
                           assign-stack-slot
                           rewrite
                           print-frame-info
                           emit-stm))

)
