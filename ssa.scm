
(declare (unit ssa)
         (uses ssa-types ssa-const ssa-transforms utils))

(use srfi-1)
(use matchable)

(import-for-syntax matchable)

(include "struct-syntax")

;; data structures

(define-struct ssa-module   (globals functions))

(define-struct ssa-node     (type code subcode in1 in2 in3 block pred succ attrs))

(define-syntax ssa-node-with-attrs
  (lambda (e r c)
    (define defaults '(type code subcode in1 in2 in3 block pred succ attrs))
    (match e
      (('ssa-node-with-attrs pair* ...)
       `(make-ssa-node ,@(map (lambda (field)
                                 (cond
                                  ((assq field pair*)
                                   => cadr)
                                  (else ''())))
                               defaults))))))

 
;; code definitions

(define *ssa-code*
  '(fun block instr arg global const))
                     
(define *ssa-subcode*
  '(const
    add sub mul div and or shl shr br brc call ret ptrtoint inttoptr
    cmp
    phi
    load store
    elementptr))

;; node predications 

(define (ssa-instr? x)
  (eq? (ssa-node-code x) 'instr))

(define (ssa-block? x)
  (eq? (ssa-node-code x) 'block))

(define (ssa-function? x)
  (eq? (ssa-node-code x) 'fun))

(define (ssa-global? x)
  (eq? (ssa-node-code x) 'global))

(define (ssa-arg? x)
  (eq? (ssa-node-code x) 'arg))

(define (ssa-const? x)
  (eq? (ssa-node-code x) 'const))

;; instr predicates

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

;; constructor for module

(define (ssa-make-module)
  (make-ssa-module '() '()))
  

;; constructor for block

(define (ssa-make-block name fun)
  (let ((node
         (ssa-node-with-attrs
          (type <ssa-label>)
          (code    'block)
          (in3      fun)
          (attrs   `((name . ,name))))))
    (ssa-function-add-block! fun node)
    node))

;; constructors for functions

(define (ssa-make-function type name mod)
  (let ((node
         (ssa-node-with-attrs
          (type  type)
          (code  'fun)
          (attrs `((name . ,name))))))
    (ssa-module-add-function! mod fun)
    node))


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
   (type  <ssa-void>)
   (code    'instr)
   (subcode 'store)
   (in1   ptr)
   (in2   value)
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
   (type  <ssa-void>)
   (code    'instr)
   (subcode 'ret)
   (in1   value)
   (block block)))

(define (ssa-make-br block label)
 (ssa-node-with-attrs
   (type  <ssa-void>)
   (code 'instr)
   (subcode 'br)
   (in1   label)
   (block block)))

(define (ssa-make-brc block cond labelx labely)
 (ssa-node-with-attrs
   (type  <ssa-void>)
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

(define (ssa-make-arg type fun)
  (let ((node
         (ssa-node-with-attrs
          (type     type)
          (code    'arg))))
    (ssa-function-add-arg! fun arg)
    node))

(define (ssa-make-global type name is-constant initializer mod)
  (let ((node
         (ssa-node-with-attrs
          (type     type)
          (code    'var)
          (subcode 'global)
          (in1      name)
          (in2      initializer)
          (attrs    `((is-constant . ,is-constant))))))
    (ssa-module-add-global! mod node)
    node))

;; constructor for const
(define (ssa-make-const type value)
  (ssa-node-with-attrs
   (type     type)
   (code    'const)
   (in1      value)))
 

;; attrs

(define (ssa-node-attr x attr)
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


;; module

(define (ssa-module-add-global! mod global)
  (ssa-module-globals-set! mod (cons global (ssa-module-globals mod))))

(define (ssa-module-add-function! mod fun)
  (ssa-module-functions-set! mod (cons fun (ssa-module-functions mod))))

;; basic

(define (ssa-instr-block x)
  (assertp ssa-instr? x)
  (ssa-node-block x))

;; ptr<->int casts

(define (ssa-inttoptr-int x)
  (assertp ssa-inttoptr? x)
  (ssa-node-in1 x))

(define (ssa-ptrtoint-ptr x)
  (assertp ssa-ptrtoint? x)
  (ssa-node-in1 x))

;; call

(define (ssa-call-fun x)
  (assertp ssa-call? x)
  (ssa-node-in1 x))

(define (ssa-call-args x)
  (assertp ssa-call? x)
  (ssa-node-in2 x))

;; ret

(define (ssa-ret-value x)
  (assertp ssa-ret? x)
  (ssa-node-in1 x))

;; load

(define (ssa-load-ptr x)
  (assertp ssa-ret? x)
  (ssa-node-in1 x))

;; store

(define (ssa-store-value x)
  (assertp ssa-store? x)
  (ssa-node-in2 x))

(define (ssa-store-ptr x)
  (assertp ssa-store? x)
  (ssa-node-in1 x))

;; binops

(define (ssa-binop-left x)
  (assertp ssa-instr? x)
  (ssa-node-in1 x))

(define (ssa-binop-right x)
  (assertp ssa-instr? x)
  (ssa-node-in2 x))

;; conditional branch 

(define (ssa-brc-cond x)
  (assertp ssa-brc? x)
  (ssa-node-in1 x))

(define (ssa-brc-labelx x)
  (assertp ssa-brc? x)
  (ssa-node-in2 x))

(define (ssa-brc-labely x)
  (assertp ssa-brc? x)
  (ssa-node-in3 x))

;; unconditional branch 

(define (ssa-br-label x)
  (assertp ssa-br? x)
  (ssa-node-in1 x))

;; elementptr

(define (ssa-elementpr-ptr x)
  (assertp ssa-elementptr? x)
  (ssa-node-in1 x))

(define (ssa-elementpr-offset x)
  (assertp ssa-elementptr? x)
  (ssa-node-in2 x))

;; function

(define (ssa-function-name x)
  (assertp ssa-function? x)
  (ssa-node-attr x 'name))

(define (ssa-function-entry x)
  (assertp ssa-function? x)
  (ssa-node-in1 x))

(define (ssa-function-entry-set! x entry)
  (assertp ssa-function? x)
  (ssa-node-in1-set! x entry))

(define (ssa-function-args x)
  (assertp ssa-function? x)
  (ssa-node-in2 x))

(define (ssa-function-args-set! x args)
  (assertp ssa-function? x)
  (ssa-node-in2-set! x args))

(define (ssa-function-add-arg! x block)
   (assertp ssa-function? x)
   (ssa-function-args-set! x (cons args (ssa-function-args x))))

(define (ssa-function-blocks x)
  (assertp ssa-function? x)
  (ssa-node-in3 x))

(define (ssa-function-blocks-set! x blocks)
  (assertp ssa-function? x)
  (ssa-node-in3-set! x blocks))

(define (ssa-function-add-block! x block)
   (assertp ssa-function? x)
   (ssa-function-blocks-set! x (cons block (ssa-function-blocks x))))

;; block

(define (ssa-block-name x)
  (assertp ssa-block? x)
  (ssa-node-attr x 'name))

(define (ssa-block-head x)
  (assertp ssa-block? x)
  (ssa-node-in1 x))

(define (ssa-block-head-set! x head)
  (assertp ssa-block? x)
  (ssa-node-in1-set! x head))

(define (ssa-block-tail x)
  (assertp ssa-block? x)
  (ssa-node-in2 x))

(define (ssa-block-tail-set! x tail)
  (assertp ssa-block? x)
  (ssa-node-in2-set! x tail))

(define (ssa-block-function x)
  (assertp ssa-block? x)
  (ssa-node-block x))

(define (ssa-block-function-set! x fun)
  (assertp ssa-block? x)
  (ssa-node-block-set! x fun))

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


;; function argument

;; global variable (compile-time static variable)

(define (ssa-global-name x)
  (assertp ssa-global? x)
  (ssa-node-attr x 'name))


(define-syntax assertp
  (syntax-rules ()
    ((assertp pred x)
     (assert (pred x) "invalid type"))))