
(use srfi-1)
(use srfi-43)

;; data structures 

(define-struct ssa-context  (args start-block attrs))
(define-struct ssa-type     (code width points-to-type element-type size)) 
(define-struct ssa-node     (type code subcode in block pred succ attrs))

;; code definitions

(define *ssa-type-codes*
  '(void label integer pointer array function))

(define *ssa-code*
  '(block instr atom))
                     
(define *ssa-subcode*
  '(const local global
    add sub mul div and or shl shr br brc call ret ptrtoint inttoptr
    cmp
    phi
    load store
    elementptr))

;; type hierarchy 

;; core types
(define <ssa-void>     (ssa-make-type-void))
(define <ssa-i1>       (ssa-make-type-integer 'i1))
(define <ssa-i8>       (ssa-make-type-integer 'i8))
(define <ssa-i16>      (ssa-make-type-integer 'i16))
(define <ssa-i32>      (ssa-make-type-integer 'i32))
(define <ssa-i64>      (ssa-make-type-integer 'i64))
(define <ssa-ptr-i32>  (ssa-make-type-pointer <ssa-i32>))
(define <ssa-ptr-i64>  (ssa-make-type-pointer <ssa-i64>))
(define <ssa-label>    (ssa-make-type-label))

;; type constructors

(define (ssa-make-type-void)
  (make-ssa-type 'void '() '() '() '() '() '() '()))

(define (ssa-make-type-label)
  (make-ssa-type 'label '() '() '() '() '() '() '()))

(define (ssa-make-type-integer width)
  (make-ssa-type 'integer width '() '() '() '() '() '()))

(define (ssa-make-type-pointer points-to-type)
  (make-ssa-type 'pointer '() points-to-type '() '() '() '() '()))

(define (ssa-make-type-array element-type size)
  (make-ssa-type 'array '() '() element-type size '() '() '()))

(define (ssa-make-type-function return-type param-types arg-count)
  (make-ssa-type 'function '() '() '() '() return-type param-types arg-count))

;; type accessors

(define (ssa-type-integer-width x)
  (ssa-type-width x))

(define (ssa-type-pointer-points-to-type x)
  (ssa-type-points-to-type x))

(define (ssa-type-array-element-width x)
  (ssa-type-element-width x))

(define (ssa-type-array-size x)
  (ssa-type-size x))

;; type predicates

(define (ssa-type-code? x code)
  (eq? (ssa-type-code x) code))

(define (ssa-type-integer? x)
  (ssa-type-code? x 'integer))

(define (ssa-type-pointer? x)
  (ssa-type-code? x 'pointer))

(define (ssa-type-void? x)
  (ssa-type-code? x 'void))

(define (ssa-type-array? x)
  (ssa-type-code? x 'array))


;; node predications (for discriminating between node classes)

(define (ssa-instr? x)
  (eq? (ssa-node-code x) 'instr))

(define (ssa-block? x)
  (eq? (ssa-node-code x) 'block))

(define (ssa-atom? x)
  (eq? (ssa-node-code x) 'atom))

;; operation predicates

(define (ssa-subcode? x subcode)
  (eq? (ssa-node-subcode x) subcode))

(define (ssa-ptrtoint? x)
  (ssa-subcode? x 'ptrtoint))

(define (ssa-inttoptr? x)
  (ssa-subcode? x 'inttoptr))

(define (ssa-load? x)
  (ssa-subcode? x 'load))

(define (ssa-store? x)
  (ssa-subcode? x 'store))

(define (ssa-phi? x)
  (ssa-subcode? x 'phi))

(define (ssa-br? x)
  (ssa-subcode? x 'br))

(define (ssa-brc? x)
  (ssa-subcode? x 'brc))

(define (ssa-add? x)
  (ssa-subcode? x 'add))

(define (ssa-sub? x)
  (ssa-subcode? x 'sub))

(define (ssa-mul? x)
  (ssa-subcode? x 'mul))

(define (ssa-and? x)
  (ssa-subcode? x 'and))

(define (ssa-or? x)
  (ssa-subcode? x 'or))

(define (ssa-shl? x)
  (ssa-subcode? x 'shl))

(define (ssa-shr? x)
  (ssa-subcode? x 'shr))

(define (ssa-cmp? x)
  (ssa-subcode? x 'cmp))

(define (ssa-call? x)
  (ssa-subcode? x 'call))

(define (ssa-ret? x)
  (ssa-subcode? x 'ret))

(define (ssa-elementptr? x)
  (ssa-subcode? x 'elementptr))


;; constructors 

;; constructor for block

(define (ssa-make-block)
  (make-ssa-node <ssa-label> 'block '() '() '() '() '() '()))

(define (ssa-make-instr type code subcode block in attrs)
  (let* ((in   (list->vector in))
         (node (make-ssa-node type code subcode in block '() '() attrs)))
    (for-each
      (lambda (value)
        (ssa-add-use! value node))
      in)
    (cond
     ((and (null? (ssa-block-head block)) (null? (ssa-block-tail block)))
      (ssa-block-head-set! block node)
      (ssa-block-tail-set! block node))
     (else 
      (ssa-instr-next-set! (ssa-block-tail block) node)
      (ssa-instr-prev-set! node (ssa-block-tail block))
      (ssa-block-tail-set! block node))
     (else (assert-not-reached)))))

;; constructors for instuctions

(define (ssa-make-binop block subcode x y)
  (let ((type (infer-type x y)))
    (ssa-make-node type 'instr subcode block (list x y) '())))

(define (ssa-make-load block ptr)
  (let ((type (infer-type ptr)))
    (ssa-make-node type 'instr 'load block (list ptr) '())))

(define (ssa-make-store block value ptr)
  (let ((type 'void))
    (ssa-make-node type 'instr 'store block (list value ptr) '())))

(define (ssa-make-call block callconv type target args)
  (ssa-make-node type 'instr 'call block (cons target args) `((callconv . ,callconv))))

(define (ssa-make-ret block value)
  (let ((type (infer-type value)))
    (ssa-make-node type 'instr 'ret block (list value) '())))

(define (ssa-make-br block target)
  (ssa-make-node 'void 'instr 'br block (list target) '()))

(define (ssa-make-brc block cond block-true block-false)
  (ssa-make-node 'void 'instr 'brc block (list cond block-true block-false) '()))

(define (ssa-make-phi block in)
  (let ((type (infer-type value)))
    (ssa-make-node type 'instr 'phi block in '())))

(define (ssa-make-elementptr block ptr index)
  (let ((type (infer-type ptr)))
    (ssa-make-node type 'instr 'elementptr block (list ptr index) '())))

(define (ssa-make-inttoptr block type value)
  (ssa-make-node type 'instr 'inttoptr block (list value) '()))

(define (ssa-make-ptrtoint block type ptr)
  (ssa-make-node type 'instr 'ptrtoint block (list ptr) '()))

(define (ssa-make-cmp block test x y)
  (ssa-make-node type 'instr 'cmp block (cons target args) `((test . ,test))))

;; constructors for atoms

(define (ssa-make-const value type)
  (ssa-make-node type 'atom 'const '() '() `((value . ,value))))

(define (ssa-make-global name type)
  (ssa-make-node type 'atom 'global '() '() `((name . ,name))))

(define (ssa-make-local name type)
  (ssa-make-node type 'atom 'local '() '() `((name . ,name))))





;; attrs

(define (ssa-node-attr-get x attr)
  (cond
   ((assq attr (ssa-node-attrs x))
    => cdr)
   (else '())))

;; aliases

(define ssa-build-block      ssa-make-block)

(define ssa-build-phi        ssa-make-phi)
(define ssa-build-load       ssa-make-load)
(define ssa-build-store      ssa-make-store)
(define ssa-build-inttoptr   ssa-make-inttoptr)
(define ssa-build-ptrtoint   ssa-make-ptrtoint)
(define ssa-build-elementptr ssa-make-elementptr)
(define ssa-build-call       ssa-make-call)
(define ssa-build-ret        ssa-make-ret)
(define ssa-build-const      ssa-make-const)
(define ssa-build-local      ssa-make-local)
(define ssa-build-global     ssa-make-global)

(define (ssa-build-add block x y)
  (ssa-make-binop block 'add x y))

(define (ssa-build-sub block x y)
  (ssa-make-binop block 'sub x y))

(define (ssa-build-mul block x y)
  (ssa-make-binop block 'mul x y))

(define (ssa-build-div block x y)
  (ssa-make-binop block 'div x y))

(define (ssa-build-and block x y)
  (ssa-make-binop block 'and x y))

(define (ssa-build-or block x y)
  (ssa-make-binop block 'or x y))

(define (ssa-build-xor block x y)
  (ssa-make-binop block 'xor x y))

(define (ssa-build-shr block x y)
  (ssa-make-binop block 'shr x y))

(define (ssa-build-shl block x y)
  (ssa-make-binop block 'shl x y))


;; basic

(define (ssa-instr-block x)
  (assertp ssa-instr? x)
  (vector-ref (ssa-node-in x) 0))

;; ptr<->int casts

(define (ssa-inttoptr-int x)
  (assertp ssa-inttoptr? x)
  (vector-ref (ssa-node-in x) 1))

(define (ssa-ptrtoint-ptr x)
  (assertp ssa-ptrtoint? x)
  (vector-ref (ssa-node-in x) 1))

;; call

(define (ssa-call-target x)
  (assertp ssa-call? x)
  (vector-ref (ssa-node-in x) 1))

(define (ssa-call-args x)
  (assertp ssa-call? x)
  (vector-ref (ssa-node-in x) 2))

;; ref

(define (ssa-ret-value x)
  (assertp ssa-ret? x)
  (vector-ref (ssa-node-in x) 1))

;; load

(define (ssa-load-ptr x)
  (assertp ssa-ret? x)
  (vector-ref (ssa-node-in x) 1))

;; store

(define (ssa-store-value x)
  (assertp ssa-store? x)
  (vector-ref (ssa-node-in x) 1))

(define (ssa-store-ptr x)
  (assertp ssa-store? x)
  (vector-ref (ssa-node-in x) 2))

;; binops

(define (ssa-binop-left x)
  (assertp ssa-instr? x)
  (vector-ref (ssa-node-in x) 1))

(define (ssa-binop-right x)
  (assertp ssa-instr? x)
  (vector-ref (ssa-node-in x) 2))

;; conditional branch 

(define (ssa-brc-cond x)
  (assertp ssa-brc? x)
  (vector-ref (ssa-node-in x) 1))

(define (ssa-brc-true-target x)
  (assertp ssa-brc? x)
  (vector-ref (ssa-node-in x) 2))

(define (ssa-brc-false-target x)
  (assertp ssa-brc? x)
  (vector-ref (ssa-node-in x) 3))

;; unconditional branch 

(define (ssa-br-cond x)
  (assertp ssa-br? x)
  (vector-ref (ssa-node-in x) 1))

(define (ssa-br-target x)
  (assertp ssa-br? x)
  (vector-ref (ssa-node-in x) 2))

;; elementptr

(define (ssa-elementpr-ptr x)
  (assertp ssa-elementptr? x)
  (vector-ref (ssa-node-in x) 1))

(define (ssa-elementpr-offset x)
  (assertp ssa-elementptr? x)
  (vector-ref (ssa-node-in x) 2))

;; function

(define (ssa-function-name x)
  (assertp ssa-function? x)
  (ssa-node-attr-get x 'name))

(define (ssa-function-start x)
  (assertp ssa-function? x)
  (vector-ref (ssa-node-in x) 0))


;; block

(define (ssa-block-name x)
  (assertp ssa-block? x)
  (ssa-block-name x))

(define (ssa-block-head x)
  (assertp ssa-block? x)
  (ssa-node-head x))

(define (ssa-block-tail x)
  (assertp ssa-block? x)
  (ssa-node-tail x))

(define (ssa-block-pred x)
  (assertp ssa-block? x)
  (ssa-node-pred x))

(define (ssa-block-succ x)
  (assertp ssa-block? x)
  (ssa-node-succ x))

;; instr

(define (ssa-instr-prev x)
  (assertp ssa-instr? x)
  (ssa-node-pred x))

(define (ssa-instr-next x)
  (assertp ssa-instr? x)
  (ssa-node-succ x))

;; constant

(define (ssa-const-value x)
  (assertp ssa-const? x)
  (first (ssa-node-attrs x)))

;; local variable (function parameter)

(define (ssa-local-name x)
  (assertp ssa-local? x)
  (first (ssa-node-attrs x)))

;; global variable (compile-time static variable)

(define (ssa-global-name x)
  (assertp ssa-global? x)
  (first (ssa-node-attrs x)))


(define-syntax assertp
  (syntax-rules ()
    ((assertp pred x)
     (assert (pred x) "invalid type"))))
  

;; block traversal

(define (ssa-for-each-block f context)
  (let ((start ((ssa-context-start-block context))))
    (let walk ((x start))
      (begin
        (f x)
        (for-each walk (ssa-block-succ x))))))

(define (ssa-for-each-block-succ f block)
  (for-each f (ssa-block-succ block)))

(define (ssa-for-each-block-pred f block)
  (f (ssa-block-pred block)))


;; instruction traversal operations

(define (ssa-fold-instr f nil block)
  (let ((head ((ssa-block-head block))))
    (let walk ((x head) (nil nil))
      (cond
       ((null? x) nil)
       (else (walk (ssa-instr-next x) (f x nil)))))))

(define (ssa-for-each-instr f block)
  (fold-instr (lambda (instr nil)
                (f instr))
              block))

;; def-use traversal

(define (ssa-add-use! node x)
  (ssa-node-uses-set! node (cons x (ssa-node-uses node))))

(define (ssa-remove-use! node x)
  (ssa-node-uses-set! node (delete eq? x (ssa-node-uses node))))

(define (ssa-for-each-use f instr)
  (for-each f (ssa-node-uses instr)))

(define (ssa-for-each-def f user)
  (vector-for-each f (ssa-node-in user)))

;; deletion

(define (ssa-delete-instr instr)
  (let ((block (ssa-instr-block instr))
        (next  (ssa-instr-next instr))
        (prev  (ssa-instr-prev instr)))
    (cond
     ((not (null? next))
      (ssa-instr-next-set! prev next))
    (cond
     ((not (null? prev))
      (ssa-instr-prev-set! next prev)))
    (cond
     ((null? prev)
      (ssa-block-head-set! next)))
    (cond
     ((null? next)
      (ssa-block-tail-set! prev))))))

;; Replaces all uses of `value` with `x`. 
(define (ssa-replace-all-uses-with! value x)
  (ssa-for-each-use
   (lambda (user)
     (ssa-node-in-set!
      (vector-map (lambda (x)
                    (cond
                     ((eq? x from)
                      (ssa-add-use! to user)
                      to)
                     (else from)))
                  (ssa-node-in user))))
   value)
  (ssa-node-uses-set! value '()))
  
;; Replaces all uses of `value` in `user` with `x`. 
(define (ssa-replace-uses-of! user from to)
  (ssa-node-in-set!
   (vector-map (lambda (x)
                 (cond
                  ((eq? x from)
                   (ssa-remove-use! from user)
                   (ssa-add-use! to user)
                   to)
                  (else from)))
               (ssa-node-in user))))   

;; Replaces `instr` with `x`
(define (ssa-replace-instr! instr x)
  (ssa-replace-all-uses-with! instr value)
  (ssa-delete-instr! instr))


;; infer types

(define ssa-infer-types (