  
(declare (unit arch-x86-64)
         (uses spec-x86-64 rules-x86-64 tree nodes))

(use matchable)
(use srfi-1)

(include "arch-syntax")

(define (operand-format-x86-64 op)
  (cond
    ((mc-vreg? op)
     (format "%~s" name))
    ((mc-imm? op)
     (format "~s" value))
    ((mc-disp? op)
     (format "~s" value))
    (else (assert-not-reached))))

(define (vregs-read-x86-64 instr)
  (let ((ops  (mc-instr-ops instr))
        (reads ((mc-spec-vregs-read (mc-instr-spec instr)) ops)))
  (cond
    ((x86-64.xor.rr? instr)
     (cond
       ((mc-vreg-equal? (first ops) (second ops))
        (list))
       (else reads)))
    (else reads))))

(define (vregs-written-x86-64 instr)
  (let ((ops  (mc-instr-ops instr))
        (writes ((mc-spec-written (mc-instr-spec instr)) ops)))
     writes))


;; Generate x86-64 code for the 'return' instruction
;;
(define (emit-return-x86-64 block value)
  ;; move return value into %rax
  (cond
    ((tree-constant? value)
     (case (tree-constant-size value)
       ((i64)
        (arch-emit-code x86-64 block
          (xor64.rr    (vreg 'rax) (vreg 'rax)))               
          (mov64.i64r  (imm i64 (tree-constant-value value)) (vreg 'rax)))
       (else (assert-not-reached))))
    ((tree-temp? value)
     (arch-emit-code x86-64 block
       (mov.rr (vreg (tree-temp-name value)) (vreg 'rax)))))
  ;; stack frame management
  (arch-emit-code x86-64 block
    (mov64rr (vreg 'rbp) (vreg 'rsp))
    (pop64r  (vreg 'rbp))
    (retnear)))


;;
;; Lower tail calls to x86-64 code 
;;
;; * Only immediates and virtual registers are allowed as arguments (i.e. no memory references). Memory 
;;   loads will have been performed in an earlier on the control path.
;; * can only jump to labels or values contained in virtual registers
;;
;; The above rules may be revised as we improve instruction selection (i.e merging selection trees for better selection opportunities)
;;
;; We move each positional arg to a constrained temp
;; Each temp will be constrained to a standard argument-passing register
;; Standard argument passing registers (in order): r8 r9 r10 r11 r12 r13 14 r15
;;
;; We create a selection tree for each arg so we can produce efficient code for moving the arg
;; to the constrained temp. This is mostly useful for immediate -> register moves.
;;
;; Example:
;; 
;; lower (app fib45 (t5 t67 3 t34)) => 
;;
;;    movq  t5,  r8
;;    movq  t67, r9
;;    movq  3,   r10
;;    movq  34,  r11
;;    
;;    jmp   fib45(%rip)  # relative jmp to fib45
;;
(define (emit-tail-call-x86-64 block target args)

  (let* ((constrained-temps (let f ((arg* args) (constrained-reg '(r8 r9 r10 r11 r12 r13 r14 r15)) (x '()))
                       (match arg*
                         (() (reverse x))
                         ((arg . arg*)
                          (f arg* (cdr constrained-reg) (cons (car constrained-reg) x))))))
         (arg-tree* (map (lambda (arg constrained-temp)
                           (tree-make-assign constrained-temp arg))
                             args
                             constrained-temps)))
    
    ;; Select instructions for moving args to constrained temps
    (for-each (lambda (arg-tree)
                (munch-x86-64 block arg-tree))
              arg-tree*)
    
    ;; Select instruction for the branch
    (munch-x86-64 block (tree-make-br target))))


(define (generate-bridge-context-x86-64 mod)
  (let ((cxt (mc-make-cxt mod (list 'heap_ptr)))
        (blk (mc-make-block cxt '__scheme_exec'))
        (heap_ptr (mc-context-allocate-vreg cxt 'heap_ptr)))

    (arch-emit-code x86-64 blk
      ;; prologue
      (push.r  (vreg 'rbp)))
      (mov.rr  (vreg 'rsp) (vreg 'rbp))

      ;; set heap_ptr
      (mov.rm heap_ptr (disp 'heap_ptr))))


;; Selects x86-64 instructions for a tree node
;;
(define (emit-statement-x86-64 block tree)
  (match tree
    (($ tree-instr 'return _ value)
     (emit-return-x86-64 block value))
    (($ tree-instr 'call _ target args)
     (case (tree-instr-attr tree 'callconv)
       ((tail)
         (emit-tail-call block target args))
       ((cdecl) '())
       (else (error 'munch-statement "should not reach here"))))
    (($ tree-instr 'assign _ (? symbol?) ($ tree-instr 'call) _ _ _ _ attrs) '())
    (_ (munch-x86-64 block tree))))

(define <arch-x86-64>
        (make-arch-descriptor
           operand-format-x86-64
           vregs-read-x86-64
           vregs-written-86-64
           generate-bridge-x86-64
           emit-statement-x86-64))

