  
(declare (unit munch)
         (uses machine tree nodes))

(use matchable)
(use srfi-1)

(include "munch-syntax")
(include "patterns")

(define (lower-return block value)

    (cond
      ((tree-constant? value)
       (case (tree-constant-size value)
         ((i8)
          (machine-block-append-instr! block (xor64rr (vreg 'rax) (vreg 'rax)))
          (machine-block-append-instr! block (mov64i8r (imm i8 (tree-constant-value value)) (vreg 'rax))))
         ((i32)
          (machine-block-append-instr! block (xor64rr (vreg 'rax) (vreg 'rax)))                
          (machine-block-append-instr! block (mov64i32r (imm i32 (tree-constant-value value)) (vreg 'rax))))
         ((i64)
          (machine-block-append-instr! block (xor64rr (vreg 'rax) (vreg 'rax)))                
          (machine-block-append-instr! block (mov64i64r (imm i64 (tree-constant-value value)) (vreg 'rax))))))
      ((tree-temp? value)
       (machine-block-append-instr! block (mov64rr (vreg (tree-temp-name value)) (vreg 'rax)))))

  ;; stack frame management
  (machine-block-append-instr! block (mov64rr (vreg 'rbp) (vreg 'rsp)))
  (machine-block-append-instr! block (pop64r  (vreg 'rbp)))
  (machine-block-append-instr! block (retnear)))

(define (lower-tail-call block target args)
  ;;
  ;; Lower to our calling convention for x86-64
  ;;
  ;; * only immediates and virtual registers are allowed as arguments (i.e. no memory references). Memory loads will have been performed in an earlier on the control path.
  ;; * can only jump to labels or values contained in registers
  ;;
  ;; The above rules may be revised as we improve instruction selection (i.e merging selection trees for better selection opportunities)
  ;;
  ;; We move each positional arg to a constrained temp
  ;; Each temp will be constrained to a standard argument-passing register
  ;; Standard argument passing registers (in order): rax rbx rcx rdx r8 r9 r10 r11 r12 r13 r14 r15
  ;;
  ;; We create a selection tree for each arg so we can produce efficient code for moving the arg
  ;; to the constrained temp. This is mostly useful for immediate -> register moves.
  ;;
  ;; Example:
  ;; 
  ;; lower (app fib45 (t5 t67 3 t34)) => 
  ;;
  ;;    movq  t5,  t56     # t56 constrained to %rax
  ;;    movq  t67, t98     # t98 constrained to %rbx
  ;;    movq  3,   t99     # t99 constrained to %rcx
  ;;    movq  34,  t102    # t102 constrained to %rdx
  ;;    jmp   fib45(%rip)  # relative jmp to fib45
  ;;
  (let* ((temps     (map (lambda (arg) (gensym 't)) args))
         (arg-tree* (map (lambda (arg temp)
                           (tree-make-assign temp arg))
                         args
                         temps)))
    
    ;; Select instructions for moving args to constrained temps
    (for-each (lambda (arg-tree)
                (munch-tree block arg-tree))
              arg-tree*)
    
    ;; Select instruction for the branch
    (munch-tree block (tree-make-br target))))

(define (lower-amd64-call-conv target args buf)
  ;; Lower to standard amd64 ABI calling convention
  ;; Used when calling C functions. This implemention is currently limited to passing a maximum of 4 integer arguments
  ;;
  ;; in rsi, rdi, rdx, rcx, r8, r9
  ;;
  ;; right now we don't do anything useful with the return value (%rax) of the callee
  ;;
  ;; Save the values of callee-save registers on the stack

  (buf-append! buf
    (list 
      (push64r 'rsi)
      (push64r 'rdi)
      (push64r 'rax)
      (push64r 'rcx)
      (push64r 'rdx)
      (push64r 'r8)
      (push64r 'r9)
      (push64r 'r10)
      (push64r 'r11)))

  ;; copy the first 4 arguments into constrained temps
  (let* ((constrained-temps (map (lambda (arg) (gensym 't)) args))
         (arg-tree*         (map (lambda (arg constrained-temp)
                                  (tree-make-assign constrained-temp arg))
                                args
                                constrained-temps)))

    ;; Select instructions for moving args to constrained temps
    (for-each (lambda (arg-tree)
                (munch-node arg-tree buf))
              arg-tree*))

  ;; Select instruction for the call
  (buf-append! buf)
  
  ;; Pop saved values off stack into callee-save registers
  (buf-append! buf
    (list 
     (pop64r 'r11)
     (pop64r 'r10)
     (pop64r 'r9)
     (pop64r 'r8)
     (pop64r 'rdx)
     (pop64r 'rcx)
     (pop64r 'rax)
     (pop64r 'rdi)
     (pop64r 'rsi))))


;;
;; The __scheme_enter context needs the normal cdecl function prologue as it is the bridge function between C and the scheme program
;;
;;
(define (construct-prologue block)

 ;; prologue
 (machine-block-append-instr! block (push64r (vreg 'rbp)))
 (machine-block-append-instr! block (mov64rr (vreg 'rsp) (vreg 'rbp)))

 ;; set heap_ptr
 (machine-block-append-instr! block (mov64rm (vreg 'rsi) (addr (disp 'heap_ptr)))))


(define (munch-statement block tree)
  (match tree
    (($ tree-instr 'return _ value)
     (lower-return block value))
    (($ tree-instr 'call _ target args)
     (case (tree-instr-attr tree 'callconv)
       ((tail)
         (lower-tail-call block target args))
       ((cdecl)
         '())
        ;;(lower-amd64-call-conv target args buf))
       (else (error 'munch-statement "should not reach here"))))
    (($ tree-instr 'assign _ (? symbol?) ($ tree-instr 'call) _ _ _ _ attrs)
    ;; (lower-amd64-call-conv target args buf attrs))
    '())
    (_ (munch-tree block tree))))


