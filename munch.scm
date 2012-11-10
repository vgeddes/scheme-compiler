  
(declare (unit munch)
         (uses machine nodes))

(use matchable)
(use srfi-1)

(include "munch-syntax")
(include "patterns")


(define (lower-call-conv block target args)
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

(define (lower-return arg buf)
  ;; select instruction for moving the return value into %rax
  (munch-node (make-selection-node 'mov (list arg 'rax)) buf)

  (buf-append! buf
    (list 
     ;; epilogue
     (mov64rr 'rbp 'rsp)
     (pop64r 'rbp)
     (retnear))))

(define (munch-statement block tree)
  (match tree
    (($ tree-instr 'call _ target args)
    ;; (lower-amd64-call-conv target args buf attrs))
     (case (tree-instr-attr tree 'callconv)
       ((tail)
         (lower-call-conv block target args))
       ((cdecl)
         '())
        ;;(lower-amd64-call-conv target args buf))
       (else (error 'munch-statement "should not reach here"))))
    (($ tree-instr 'assign _ (? symbol?) ($ tree-instr 'call) _ _ _ _ attrs)
    ;; (lower-amd64-call-conv target args buf attrs))
    '())
    (($ tree-instr 'return _ value _ _ _ _ _ _)
    ;; (lower-return value buf))
    '())
    (_ (munch-tree block tree))))


