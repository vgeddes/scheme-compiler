
(use srfi-1)
(use srfi-43)

;; data structures 

(define-struct ssa-context  (args start-block attrs))
(define-struct ssa-node     (type code subcode in1 in2 in3 block pred succ attrs))

(define-syntax ssa-node-with-attrs
  (lambda (e r c)
    (define defaults '(type code subcode in1 in2 in3 block pred succ attrs))
    (match e
      (('ssa-node-with-attrs pair* ...)
       `(make-ssa-node  ,@(map (lambda (attr)
                                 (cond
                                  ((assq (car attr) pair*)
                                   => cdr)
                                  (else '())))
                               defaults))))))

 
;; code definitions

(define *ssa-code*
  '(fun block instr var const))
                     
(define *ssa-subcode*
  '(const local global
    add sub mul div and or shl shr br brc call ret ptrtoint inttoptr
    cmp
    phi
    load store
    elementptr))

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

(define (ssa-make-binop block subcode type x y)
  (ssa-node-with-attrs
   (type type)
   (code    'instr)
   (subcode  subcode)
   (in1  x)
   (in2  y)
   (block block)))
   
(define (ssa-make-load block ptr)
  (ssa-node-with-attrs
   (type  (ssa-type-pointer-points-to-type ptr))
   (code    'instr)
   (subcode 'load)
   (in1   ptr)
   (block block)))

(define (ssa-make-store block value ptr)
  (ssa-node-with-attrs
   (type  (ssa-type-void-get))
   (code    'instr)
   (subcode 'store)
   (in1   value)
   (in2   ptr)
   (block block)))

(define (ssa-make-call block callconv type fun args)
  (ssa-node-with-attrs
   (type  type)
   (code 'instr)
   (subcode 'call)
   (in1   fun)
   (in2   args)
   (block block)
   (attrs `((callconv . ,callconv)))))

(define (ssa-make-ret block value)
  (ssa-node-with-attrs
   (type  (ssa-type-void-get))
   (code    'instr)
   (subcode 'ret)
   (in1   value)
   (block block)))

(define (ssa-make-br block label)
 (ssa-node-with-attrs
   (type  (ssa-type-void-get))
   (code 'instr)
   (subcode 'br)
   (in1   label)
   (block block)))

(define (ssa-make-brc block cond labelx labely)
 (ssa-node-with-attrs
   (type  (ssa-type-void-get))
   (code 'instr)
   (subcode 'brc)
   (in1   cond)
   (in2   labelx)
   (in3   labely)
   (block block)))

(define (ssa-make-phi block in)
 (ssa-node-with-attrs
   (type  (ssa-node-type (car in)))
   (code    'instr)
   (subcode 'phi)
   (in1   in)
   (block block)))

(define (ssa-make-elementptr block ptr index)
 (ssa-node-with-attrs
   (type  (ssa-node-type ptr))
   (code    'instr)
   (subcode 'elementptr)
   (in1   ptr)
   (in2   index)
   (block block)))

(define (ssa-make-inttoptr block type value)
 (ssa-node-with-attrs
   (type  type)
   (code    'instr)
   (subcode 'inttoptr)
   (in1   value)
   (block block)))

(define (ssa-make-ptrtoint block type ptr)
 (ssa-node-with-attrs
   (type  type)
   (code    'instr)
   (subcode 'ptrtoint)
   (in1   ptr)
   (block block)))

(define (ssa-make-cmp block test x y)
 (ssa-node-with-attrs
   (type   <ssa-i1>)
   (code    'instr)
   (subcode 'cmp)
   (in1   test)
   (in2   x)
   (in3   y)
   (block block)))

;; constructors for atoms

(define (ssa-make-global block name type)
  (ssa-node-with-attrs
   (type     type)
   (code    'var)
   (subcode 'global)
   (in1      name)))

(define (ssa-make-local block name type)
  (ssa-node-with-attrs
   (type     type)
   (code    'var)
   (subcode 'local)
   (in1      name)))


;; attrs

(define (ssa-node-attr-get x attr)
  (cond
   ((assq attr (ssa-node-attrs x))
    => cdr)
   (else '())))

;; builders

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
  (cond
   ((and (ssa-constant? x) (ssa-constant? y))
    (ssa-fold-add x y))
   (else (ssa-make-binop block 'add (ssa-node-type x) x y))))

(define (ssa-build-sub block x y)
  (cond
   ((and (ssa-constant? x) (ssa-constant? y))
    (ssa-fold-sub x y))
   (else (ssa-make-binop block 'sub (ssa-node-type x) x y))))

(define (ssa-build-mul block x y)
  (cond
   ((and (ssa-constant? x) (ssa-constant? y))
    (ssa-fold-mul x y))
   (else (ssa-make-binop block 'mul (ssa-node-type x) x y))))

(define (ssa-build-and block x y)
  (cond
   ((and (ssa-constant? x) (ssa-constant? y))
    (ssa-fold-and x y))
  (else (ssa-make-binop block 'and (ssa-node-type x) x y))))

(define (ssa-build-or block x y)
 (cond
  ((and (ssa-constant? x) (ssa-constant? y))
   (ssa-fold-or x y))  
  (else (ssa-make-binop block 'or (ssa-node-type x) x y))))

(define (ssa-build-xor block x y)
 (cond
  ((and (ssa-constant? x) (ssa-constant? y))
   (ssa-fold-xor x y))    
  (else (ssa-make-binop block 'xor (ssa-node-type x) x y))))

(define (ssa-build-shr block x y)
  (cond
   ((and (ssa-constant? x) (ssa-constant? y))
    (ssa-fold-shr x y))    
   (else (ssa-make-binop block 'shr (ssa-node-type x) x y))))

(define (ssa-build-shl block x y)
  (cond
   ((and (ssa-constant? x) (ssa-constant? y))
    (ssa-fold-shl x y))    
   (else (ssa-make-binop block 'shl (ssa-node-type x) x y))))

;; folders

(define (ssa-fold-add x y)
  (let ((value (+ (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-sub x y)
  (let ((value (- (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-mul x y)
  (let ((value (* (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-and x y)
  (let ((value (bitwise-and (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-or x y)
  (let ((value (bitwise-ior (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-xor x y)
  (let ((value (bitwise-xor (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-shr x y)
  (let ((value (arithmetic-shift (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-shl x y)
  (let ((value (arithmetic-shift (ssa-const-value x) (- 0 (ssa-const-value y)))))
    (ssa-constant-get (ssa-node-type x) value)))


;; accessors

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

