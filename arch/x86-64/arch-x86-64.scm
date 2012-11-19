  
(declare (unit arch-x86-64)
         (uses machine spec-x86-64 rules-x86-64 tree nodes))

(use matchable)
(use srfi-1)

(include "arch-syntax")

(define *tail-call-hregs-x86-64* '(r8 r9 r10 r11 r12 r13 r14 r15))

;;
;; Arguments passed to a context are constrained to registers r8 ... r15.
;; This could cause allocation conflicts between constrained vregs during register allocation. 
;;
;; So we insert moves to copy each constrained arg into an unconstrained vreg, and add
;; hints so that the allocator can eliminate the move if possible
;;
(define (make-context-x86-64 name params mod)
  (let* ((cxt         (make-mc-context name '() '() '()))
         (args        (map (lambda (param hint)
                             (mc-make-vreg param hint))
                           params
                           *tail-call-hregs-x86-64*))
         (args-fixed  (map (lambda (arg hreg)
                             (mc-context-allocate-vreg cxt (gensym 't) hreg))
                           args
                           *tail-call-hregs-x86-64*))
         (blk         (mc-make-block cxt name)))

    ;; insert move from each arg-fixed into arg
    (for-each (lambda (fixed arg)
                (arch-emit-code x86-64 blk
                  (mov.rr fixed arg)))
              args-fixed args)

    (mc-context-start-set! cxt blk)
    (mc-context-args-set!  cxt args-fixed)
    (mc-module-contexts-set! mod (cons cxt (mc-module-contexts mod)))
    cxt))


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
  (let* ((ops           (mc-instr-ops instr))
        (reads         ((mc-spec-reads (mc-instr-spec instr)) ops)))
  (cond
    ((x86-64.xor.rr? instr)
     (cond
       ((mc-vreg-equal? (first ops) (second ops))
        (list))
       (else reads)))
    (else reads))))

(define (vregs-written-x86-64 instr)
  (let* ((ops  (mc-instr-ops instr))
         (writes ((mc-spec-writes (mc-instr-spec instr)) ops)))
     writes))


;; Generate x86-64 code for the 'return' instruction
;;
(define (emit-return-x86-64 block value)
  ;; move return value into %rax
  (cond
    ((tree-constant? value)
     (case (tree-constant-size value)
       ((i32)
        (arch-emit-code x86-64 block
          (mov.i64r  (imm i64 0) (hreg rax))
          (mov.i32r  (imm i32 (tree-constant-value value)) (hreg rax))))
       ((i64)
        (arch-emit-code x86-64 block      
          (mov.i64r  (imm i64 (tree-constant-value value)) (hreg rax))))
       (else (assert-not-reached))))
    ((tree-temp? value)
     (arch-emit-code x86-64 block
       (mov.rr (vreg (tree-temp-name value)) (hreg rax)))))
  ;; stack frame management
  (arch-emit-code x86-64 block
    (mov.rr (hreg rbp) (hreg rsp))
    (pop.r  (hreg rbp))
    (retnear)))


;;
;; Lower tail calls to x86-64 code 
;;
;; * Only immediates virtual registers are allowed as arguments (i.e. no memory references).
;; * Can only jump to labels or values contained in virtual registers
;;
;; We move each positional arg to a constrained temp (i.e pre-colored to a hardware register)

;; Standard argument passing registers (in order): r8 r9 r10 r11 r12 r13 14 r15
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
;;    jmp  [rip + fib45]  # relative jmp to fib45.
;;
;; For liveness analysis and register allocation purposes, we hint that 'jmp' implicitly uses r8, r9, r10 and r11
;;
(define (emit-tail-call-x86-64 blk tgt args)

  (define (lower-target tgt)
    (cond
      ((tree-label? tgt)
       (mc-make-disp (tree-label-name tgt)))
      ((tree-temp? tgt)
       (mc-context-allocate-vreg (mc-block-cxt blk) (tree-temp-name tgt)))
      (else (assert-not-reached))))

  (let* ((hregs      '(r8 r9 r10 r11 r12 r13 r14 r15))
         (hregs-used  (let f ((arg* args) (hregs hregs) (x '()))
                            (match arg*
                              (() (map (lambda (hreg) (mc-context-allocate-vreg (mc-block-cxt blk) hreg hreg)) (reverse x)))
                              ((arg . arg*)
                               (f arg* (cdr hregs) (cons (car hregs) x))))))
         (moves*      (map (lambda (hreg arg)
                             (tree-make-assign hreg arg))
                           hregs-used
                           args)))
    
    ;; Select instructions for each move
    (for-each (lambda (reg arg)
                (pretty-print (mc-vreg-name reg))
                (cond
                  ((tree-temp? arg)
                   (arch-emit-code x86-64 blk
                     (mov.rr (vreg (tree-temp-name arg)) reg)))
                  ((tree-constant? arg)
                   (arch-emit-code x86-64 blk
                     (mov.i64r (imm i64 (tree-constant-value arg)) reg)))
                  (else (assert-not-reached))))
              hregs-used args)

    ;; Choose instruction for the actual tail call
    (cond
      ((tree-label? tgt)
       (x86-64.jmp.d blk hregs-used (lower-target tgt)))      
      ((tree-temp? tgt)
       (x86-64.jmp.r blk hregs-used (lower-target tgt)))
      (else (assert-not-reached)))))

(define (generate-bridge-context-x86-64 mod)
  (let* ((cxt  (make-mc-context '__scheme_exec '() '() '()))
         (ptr  (mc-context-allocate-vreg cxt (gensym 't) 'rsi)))


    (mc-context-args-set! cxt (list ptr))
    (mc-context-start-set! cxt (mc-make-block cxt '__scheme_exec))
    
    (arch-emit-code x86-64 (mc-context-start cxt)
      ;; prologue
      (push.r  (hreg rbp))
      (mov.rr  (hreg rsp) (hreg rbp))
      ;; set heap_ptr
      (mov.rd ptr (disp 'heap_ptr))
      ;; jump to __begin
      (jmp.d   (disp 'begin)))

    (mc-module-contexts-set! mod (cons cxt (mc-module-contexts mod)))

    cxt))


;; Selects x86-64 instructions for a tree node
;;
(define (emit-statement-x86-64 block tree)
  (match tree
    (($ tree-instr 'return _ value)
     (emit-return-x86-64 block value))
    (($ tree-instr 'call _ target args)
     (case (tree-instr-attr tree 'callconv)
       ((tail)
         (emit-tail-call-x86-64 block target args))
       ((cdecl) '())
       (else (error 'munch-statement "should not reach here"))))
    (($ tree-instr 'assign _ (? symbol?) ($ tree-instr 'call) _ _ _ _ attrs) '())
    (_ (munch-x86-64 block tree))))

(define <arch-x86-64>
        (make-arch-descriptor
           make-context-x86-64
           operand-format-x86-64
           vregs-read-x86-64
           vregs-written-x86-64
           generate-bridge-context-x86-64
           emit-statement-x86-64))

