
(declare (unit ssa)
         (uses ssa-types ssa-const ssa-transforms utils))

(use srfi-1)
(use srfi-13)
(use matchable)

(import-for-syntax matchable)

(include "struct-syntax")


(define-syntax assertp
  (syntax-rules ()
    ((assertp pred x)
     (assert (pred x) "invalid type"))))

;; data structures


(define-struct ssa-module (globals functions symtab))

(define-struct ssa-node   (tag type op in1 in2 in3 block next prev users attrs))

(define-syntax ssa-node-with-attrs
  (lambda (e r c)
    (define defaults '(tag type op in1 in2 in3 block next prev users attrs))
    (match e
      (('ssa-node-with-attrs pair* ...)
       `(make-ssa-node ,@(map (lambda (field)
                                 (cond
                                  ((assq field pair*)
                                   => cadr)
                                  (else ''())))
                               defaults))))))
 
;; code definitions

(define *ssa-species*
  '(function block instr arg global const))

;; tag predicates 

(define (ssa-instr? x)
  (eq? (ssa-node-tag x) 'instr))

(define (ssa-block? x)
  (eq? (ssa-node-tag x) 'block))

(define (ssa-function? x)
  (eq? (ssa-node-tag x) 'function))

(define (ssa-global? x)
  (eq? (ssa-node-tag x) 'global))

(define (ssa-arg? x)
  (eq? (ssa-node-tag x) 'arg))

(define (ssa-constant? x)
  (eq? (ssa-node-tag x) 'const))

;; instr predicates

(define (ssa-instr-is-a? x op)
  (and (ssa-instr? x)
       (eq? (ssa-node-op x) op)))

(define (ssa-load? x)
  (ssa-instr-is-a? x <ssa-op-load>))

(define (ssa-store? x)
  (ssa-instr-is-a? x <ssa-op-store>))

(define (ssa-phi? x)
  (ssa-instr-is-a? x <ssa-op-phi>))

(define (ssa-br? x)
  (ssa-instr-is-a? x <ssa-op-br>))

(define (ssa-brc? x)
  (ssa-instr-is-a? x <ssa-op-brc>))

(define (ssa-add? x)
  (ssa-instr-is-a? x <ssa-op-add>))

(define (ssa-sub? x)
  (ssa-instr-is-a? x <ssa-op-sub>))

(define (ssa-mul? x)
  (ssa-instr-is-a? x <ssa-op-mul>))

(define (ssa-and? x)
  (ssa-instr-is-a? x <ssa-op-and>))

(define (ssa-or? x)
  (ssa-instr-is-a? x <ssa-op-or>))

(define (ssa-shl? x)
  (ssa-instr-is-a? x <ssa-op-shl>))

(define (ssa-shr? x)
  (ssa-instr-is-a? x <ssa-op-shr>))

(define (ssa-cmp? x)
  (ssa-instr-is-a? x <ssa-op-cmp>))

(define (ssa-cast? x)
  (ssa-instr-is-a? x <ssa-op-cast>))

(define (ssa-call? x)
  (ssa-instr-is-a? x <ssa-op-call>))

(define (ssa-ret? x)
  (ssa-instr-is-a? x <ssa-op-ret>))

(define (ssa-elementptr? x)
  (ssa-instr-is-a? x <ssa-op-elementptr>))


;; constructors

;; constructor for module

(define (ssa-make-module)
  (make-ssa-module '() '() '()))
  

;; constructor for block

(define (ssa-make-block name fun)
  (let ((node
         (ssa-node-with-attrs
          (type <ssa-label>)
          (code    'block)
          (block    fun)
          (attrs   `((name . ,name))))))
    (ssa-function-add-block! fun node)
    node))

;; constructors for functions

(define (ssa-make-function type name mod)
  (let ((node
         (ssa-node-with-attrs
          (type   type)
          (code  'fun)
          (attrs `((name . ,name))))))
    (ssa-module-add-function! mod node)
    node))


;; constructors for instuctions

(define (ssa-make-binop block subcode type x y)
  (let ((node
         (ssa-node-with-attrs
          (type     type)
          (code    'instr)
          (subcode  subcode)
          (in1  x)
          (in2  y)
          (block block))))
    (ssa-block-add-instr! block node)))

(define (ssa-make-load block ptr)
  (let ((node
         (ssa-node-with-attrs
          (type  (ssa-type-pointer-points-to-type (ssa-node-type ptr)))
          (code    'instr)
          (subcode 'load)
          (in1   ptr)
          (block block))))
    (ssa-block-add-instr! block node)))

(define (ssa-make-store block value ptr)
  (let ((node
         (ssa-node-with-attrs
          (type  <ssa-void>)
          (code    'instr)
          (subcode 'store)
          (in1   ptr)
          (in2   value)
          (block block))))
    (ssa-block-add-instr! block node)))

(define (ssa-make-call block callconv fun args)
  (let ((node
         (ssa-node-with-attrs
          (type  (ssa-type-function-return-type (ssa-node-type fun)))
          (code 'instr)
          (subcode 'call)
          (in1   fun)
          (in2   args)
          (block block)
          (attrs `((callconv . ,callconv))))))
    (ssa-block-add-instr! block node)))

(define (ssa-make-ret block value)
  (let ((node
         (ssa-node-with-attrs
          (type  <ssa-void>)
          (code    'instr)
          (subcode 'ret)
          (in1   value)
          (block block))))
    (ssa-block-add-instr! block node)))

(define (ssa-make-br block label)
  (let ((node
         (ssa-node-with-attrs
          (type     <ssa-void>)
          (code    'instr)
          (subcode 'br)
          (in1      label)
          (block    block))))
    (ssa-block-add-instr! block node)))

(define (ssa-make-brc block cond labelx labely)
  (let ((node
         (ssa-node-with-attrs
          (type     <ssa-void>)
          (code    'instr)
          (subcode 'brc)
          (in1      cond)
          (in2      labelx)
          (in3      labely)
          (block    block))))
    (ssa-block-add-instr! block node)))

(define (ssa-make-phi block in)
  (let ((node
         (ssa-node-with-attrs
          (type  (ssa-node-type (car in)))
          (code    'instr)
          (subcode 'phi)
          (in1   in)
          (block block))))
    (ssa-block-add-instr! block node)))

(define (ssa-make-elementptr block ptr index)
  (let ((node
         (ssa-node-with-attrs
          (type  (ssa-node-type ptr))
          (code    'instr)
          (subcode 'elementptr)
          (in1   ptr)
          (in2   index)
          (block block))))
    (ssa-block-add-instr! block node)))

(define (ssa-make-cast block type value)
  (let ((node
         (ssa-node-with-attrs
          (type     type)
          (code    'instr)
          (subcode 'cast)
          (in1      value)
          (block    block))))
    (ssa-block-add-instr! block node)))

(define (ssa-make-cmp block test x y)
  (let ((node
         (ssa-node-with-attrs
          (type   <ssa-i1>)
          (code    'instr)
          (subcode 'cmp)
          (in1   test)
          (in2   x)
          (in3   y)
          (block block))))
    (ssa-block-add-instr! block node)))

;; constructors for atoms

(define (ssa-make-arg type fun)
  (let ((node
         (ssa-node-with-attrs
          (type     type)
          (code    'arg))))
    (ssa-function-add-arg! fun node)
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
 

;; node attributes

(define (ssa-node-attr x attr)
  (cond
   ((assq attr (ssa-node-attrs x))
    => cdr)
   (else #f)))

(define (ssa-node-attr-set! x attr value)
  (cond
   ((assq attr (ssa-node-attrs x))
    => (lambda (cell)
         (set-cdr! cell value)))
   (else
    (ssa-node-attrs-set! x (cons (cons attr value) (ssa-node-attrs x))))))
  
;; builders

(define ssa-build-block      ssa-make-block)

(define ssa-build-phi        ssa-make-phi)
(define ssa-build-load       ssa-make-load)
(define ssa-build-store      ssa-make-store)
(define ssa-build-elementptr ssa-make-elementptr)
(define ssa-build-call       ssa-make-call)
(define ssa-build-ret        ssa-make-ret)
(define ssa-build-cmp        ssa-make-cmp)
(define ssa-build-br         ssa-make-br)
(define ssa-build-brc        ssa-make-brc)

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
    (print  (ssa-constant-get (ssa-node-type x) value))
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
    (print  (ssa-constant-get (ssa-node-type x) value))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-shl x y)
  (let ((value (arithmetic-shift (ssa-const-value x) (- 0 (ssa-const-value y)))))
    (print  (ssa-constant-get (ssa-node-type x) value))
    (ssa-constant-get (ssa-node-type x) value)))


;; formatting

(define (ssa-temp-name node)
  (cond
   ((ssa-node-attr node 'temp-name)
    => cdr)
   (else
    (ssa-node-attr-set! node 'temp-name (gensym 't))
    (ssa-node-attr node 'temp-name))))

(define (ssa-format-value x)
  (cond
   ((ssa-function? x)
    (format "@~a" (ssa-function-name x)))
   ((ssa-global? x)
    (format "@~a" (ssa-global-name x)))
   ((ssa-block? x)
    (format "%~a" (ssa-block-name x)))
   ((ssa-instr? x)
    (format "%~a" (ssa-temp-name x)))
   ((ssa-arg? x)
    (format "%~a" (ssa-temp-name x)))
   ((ssa-constant? x)
    (format "~a" (ssa-constant-value x)))
   (else (assert-not-reached))))

;; module

(define (ssa-module-add-global! mod globl)
  (ssa-module-symtab-set! mod (cons (cons (ssa-global-name global) global) (ssa-module-symtab mod)))
  (ssa-module-globals-set! mod (cons globl (ssa-module-globals mod))))

(define (ssa-module-add-function! mod fun)
  (ssa-module-symtab-set! mod (cons (cons (ssa-function-name fun) fun) (ssa-module-symtab mod)))
  (ssa-module-functions-set! mod (cons fun (ssa-module-functions mod))))

(define (ssa-module-value-get mod name)
  (cond
   ((assq name (ssa-module-symtab mod))
    => cdr)
   (else '())))



;; polymorphic instructions

(define (ssa-instr-iterate-uses f x)
  (ssa-op-iterate-uses (ssa-node-op x) f x))

(define (ssa-instr-replace-uses f x)
  (ssa-op-replace-uses (ssa-node-op x) f x))

(define (ssa-instr-list-uses f x)
  (ssa-op-list-uses (ssa-node-op x) x))

(define (ssa-instr-format x)
  (ssa-op-format (ssa-node-op x) x))

(define (ssa-instr-constant-fold x)
  (ssa-op-constant-fold (ssa-node-op x) x))

;; basic

(define (ssa-instr-block x)
  (assertp ssa-instr? x)
  (ssa-node-block x))

(define (ssa-instr-block-set! x v)
  (assertp ssa-instr? x)
  (ssa-node-block-set! x v))

(define (ssa-instr-prev x)
  (assertp ssa-instr? x)
  (ssa-node-prev x))

(define (ssa-instr-prev-set! x prev)
  (assertp ssa-instr? x)
  (ssa-node-prev-set! x prev))

(define (ssa-instr-next x)
  (assertp ssa-instr? x)
  (ssa-node-next x))

(define (ssa-instr-next-set! x next)
  (assertp ssa-instr? x)
  (ssa-node-next-set! x next))


;; binops

(define (ssa-binop-left x)
  (assertp ssa-instr? x)
  (ssa-node-in1 x))

(define (ssa-binop-left-set! x v)
  (assertp ssa-instr? x)
  (ssa-node-in1-set! x v))

(define (ssa-binop-right x)
  (assertp ssa-instr? x)
  (ssa-node-in2 x))

(define (ssa-binop-right-set! x v)
  (assertp ssa-instr? x)
  (ssa-node-in2-set! x v))

(define (ssa-binop-iterate-uses f x)
  (f (ssa-binop-left x))
  (f (ssa-binop-right x)))

(define (ssa-binop-replace-uses f x)
  (ssa-binop-left-set!  (f (ssa-binop-left x)))
  (ssa-binop-right-set! (f (ssa-binop-right x))))

(define (ssa-binop-list-uses x)
  (list (ssa-binop-left x)
        (ssa-binop-right x)))

(define (ssa-format-binop fmt node)
  (format fmt
    (ssa-format-type  (ssa-node-type (ssa-binop-left node)))
    (ssa-format-value (ssa-binop-left node))
    (ssa-format-value (ssa-binop-right node))))

(define (ssa-format-add node)
  (ssa-format-binop "add ~a ~a, ~a" node))

(define (ssa-format-sub node)
  (ssa-format-binop "sub ~a ~a, ~a" node))

(define (ssa-format-mul node)
  (ssa-format-binop "mul ~a ~a, ~a" node))

(define (ssa-format-and node)
  (ssa-format-binop "and ~a ~a, ~a" node))

(define (ssa-format-or node)
  (ssa-format-binop "or ~a ~a, ~a" node)) 

(define (ssa-format-xor node)
  (ssa-format-binop "xor ~a ~a, ~a" node)) 

(define (ssa-format-shl node)
  (ssa-format-binop "shl ~a ~a, ~a" node)) 

(define (ssa-format-shr node)
  (ssa-format-binop "shr ~a ~a, ~a" node)) 

;; cast

(define (ssa-cast-type x)
  (assertp ssa-cast? x)
  (ssa-node-type x))

(define (ssa-cast-value x)
  (assertp ssa-cast? x)
  (ssa-node-in1 x))

(define (ssa-cast-value-set! x v)
  (assertp ssa-cast? x)
  (ssa-node-in1-set! x v))

(define (ssa-cast-iterate-uses f x)
  (f (ssa-cast-value x)))

(define (ssa-cast-replace-uses f x)
  (ssa-cast-value-set! x (f (ssa-cast-value x))))

(define (ssa-cast-list-uses x)
  (list (ssa-cast-value x)))

(define (ssa-format-cast node)
  (format "cast ~a ~a"
          (ssa-format-type  (ssa-node-type node))
          (ssa-format-value (ssa-cast-value node))))
  
  
;; call

(define (ssa-call-fun x)
  (assertp ssa-call? x)
  (ssa-node-in1 x))

(define (ssa-call-fun-set! x v)
  (assertp ssa-call? x)
  (ssa-node-in1-set! x v))

(define (ssa-call-args x)
  (assertp ssa-call? x)
  (ssa-node-in2 x))

(define (ssa-call-args-set! x v)
  (assertp ssa-call? x)
  (ssa-node-in2 x v))

(define (ssa-call-iterate-uses f x)
  (f (ssa-call-fun x))
  (for-each f (ssa-call-args x)))

(define (ssa-call-replace-uses f x)
  (ssa-call-fun-set! x (f (ssa-call-fun x)))
  (ssa-call-args-set! x (map f (ssa-call-args x))))

(define (ssa-call-list-uses f x)
  (cons (ssa-call-fun x) (ssa-call-args x)))

(define (ssa-format-call node)
  (format "call ~a ~a (~a)"
          (ssa-format-type  (ssa-node-type node))
          (ssa-format-value (ssa-call-fun node))
          (string-join
           (map (lambda (arg)
                  (ssa-format-value arg))
                (ssa-call-args node))
           ", ")))
          
;; ret

(define (ssa-ret-value x)
  (assertp ssa-ret? x)
  (ssa-node-in1 x))

(define (ssa-ret-value-set! x)
  (assertp ssa-ret? x)
  (ssa-node-in1 x))

(define (ssa-ret-iterate-uses f x)
  (f (ssa-ret-value x)))

(define (ssa-ret-replace-uses f x)
  (ssa-ret-value-set! x (f (ssa-ret-value x))))

(define (ssa-ret-list-uses x)
  (list (ssa-ret-value x)))

(define (ssa-format-ret node)
  (format "ret ~a"
          (ssa-format-value (ssa-ret-value node))))

;; load

(define (ssa-load-ptr x)
  (assertp ssa-load? x)
  (ssa-node-in1 x))

(define (ssa-load-ptr-set! x v)
  (assertp ssa-load? x)
  (ssa-node-in1-set! x v))

(define (ssa-load-iterate-uses f x)
  (f (ssa-load-ptr x)))

(define (ssa-load-replace-uses f x)
  (ssa-load-ptr-set! x (f (ssa-load-ptr x))))

(define (ssa-load-list-uses x)
  (list (ssa-load-ptr x)))

(define (ssa-format-load node)
  (format "load ~a ~a"
          (ssa-format-type  (ssa-node-type node))
          (ssa-format-value (ssa-load-ptr node))))

;; store

(define (ssa-store-value x)
  (assertp ssa-store? x)
  (ssa-node-in2 x))

(define (ssa-store-value-set! x v)
  (assertp ssa-store? x)
  (ssa-node-in2-set! x v))

(define (ssa-store-ptr x)
  (assertp ssa-store? x)
  (ssa-node-in1 x))

(define (ssa-store-ptr-set! x v)
  (assertp ssa-store? x)
  (ssa-node-in1 x v))

(define (ssa-store-iterate-uses f x)
  (f (ssa-store-value x))
  (f (ssa-store-ptr   x)))

(define (ssa-store-replace-uses f x)
  (ssa-store-value-set! x (f (ssa-store-value x)))
  (ssa-store-ptr-set!   x (f (ssa-store-ptr   x))))

(define (ssa-store-list-uses x)
  (list (ssa-store-value x)
        (ssa-store-ptr x)))

(define (ssa-format-store node)
  (format "store ~a ~a"
          (ssa-format-type  (ssa-node-type (ssa-store-value node)))
          (ssa-format-value (ssa-store-ptr  node))))

;; conditional branch 

(define (ssa-brc-cond x)
  (assertp ssa-brc? x)
  (ssa-node-in1 x))

(define (ssa-brc-cond-set! x v)
  (assertp ssa-brc? x)
  (ssa-node-in1-set! x v))

(define (ssa-brc-labelx x)
  (assertp ssa-brc? x)
  (ssa-node-in2 x))

(define (ssa-brc-labelx-set! x v)
  (assertp ssa-brc? x)
  (ssa-node-in2-set! x v))

(define (ssa-brc-labely x)
  (assertp ssa-brc? x)
  (ssa-node-in3 x))

(define (ssa-brc-labely-set! x v)
  (assertp ssa-brc? x)
  (ssa-node-in3-set! x v))

(define (ssa-brc-iterate-uses f x)
  (f (ssa-brc-cond x))
  (f (ssa-brc-labelx x))
  (f (ssa-brc-labely x)))

(define (ssa-brc-replace-uses f x)
  (ssa-brc-cond-set!   x (f (ssa-brc-cond x)))
  (ssa-brc-labelx-set! x (f (ssa-brc-labelx x)))
  (ssa-brc-labely-set! x (f (ssa-brc-labely x))))

(define (ssa-brc-list-uses x)
  (list (ssa-brc-cond x)
        (ssa-brc-labelx x)
        (ssa-brc-labely x)))

(define (ssa-format-brc node)
  (format "brc ~a ~a, ~a, ~a"
          (ssa-format-type  (ssa-node-type (ssa-brc-cond x)))
          (ssa-format-value (ssa-brc-cond x))
          (ssa-format-value (ssa-brc-labelx x))
          (ssa-format-value (ssa-brc-labely x))))

;; unconditional branch 

(define (ssa-br-label x)
  (assertp ssa-br? x)
  (ssa-node-in1 x))

(define (ssa-br-label-set! x v)
  (assertp ssa-br? x)
  (ssa-node-in1-set! x v))

(define (ssa-br-iterate-uses f x)
  (f (ssa-br-label x)))

(define (ssa-br-replace-uses f x)
  (ssa-br-label-set! x (f (ssa-br-label x))))

(define (ssa-br-list-uses x)
  (list (ssa-br-label x)))

(define (ssa-format-br node)
  (format "br ~a"
          (ssa-format-value (ssa-br-label x))))

;; elementptr

(define (ssa-elementpr-ptr x)
  (assertp ssa-elementptr? x)
  (ssa-node-in1 x))

(define (ssa-elementpr-ptr-set! x v)
  (assertp ssa-elementptr? x)
  (ssa-node-in1-set! x v))

(define (ssa-elementpr-offset x)
  (assertp ssa-elementptr? x)
  (ssa-node-in2 x))

(define (ssa-elementpr-offset-set! x v)
  (assertp ssa-elementptr? x)
  (ssa-node-in2-set! x v))

(define (ssa-elementptr-iterate-uses f x)
  (f (ssa-elementptr-ptr x))
  (f (ssa-elementptr-offset x)))

(define (ssa-elementptr-replace-uses f x)
  (ssa-elementptr-ptr-set!    x (f (ssa-elementptr-ptr x)))
  (ssa-elementptr-offset-set! x (f (ssa-elementptr-offset x))))
  
(define (ssa-elementptr-list-uses x)
  (list (ssa-elementptr-ptr x)
        (ssa-elementptr-offset x)))

(define (ssa-format-elementptr node)
  (format "elementptr ~a ~a, ~a"
          (ssa-format-type (ssa-node-type (ssa-elementptr-ptr x)))
          (ssa-format-value (ssa-elementptr-ptr x))
          (ssa-format-value (ssa-elementptr-offset node))))

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

(define (ssa-function-add-arg! x arg)
   (assertp ssa-function? x)
   (ssa-function-args-set! x (cons arg (ssa-function-args x))))

(define (ssa-function-blocks x)
  (assertp ssa-function? x)
  (ssa-node-in3 x))

(define (ssa-function-blocks-set! x blocks)
  (assertp ssa-function? x)
  (ssa-node-in3-set! x blocks))

(define (ssa-function-add-block! x block)
   (assertp ssa-function? x)
   (ssa-function-blocks-set! x (cons block (ssa-function-blocks x))))

(define (ssa-function-is-declaration? x)
  (assertp ssa-function? x)
  (ssa-node-attr x 'is-declaration))

(define (ssa-function-is-definition? x)
  (assertp ssa-function? x)
  (ssa-node-attr x 'is-definition))

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

(define (ssa-block-pred-set! x pred)
  (assertp ssa-block? x)
  (ssa-node-pred-set! x pred))

(define (ssa-block-succ x)
  (assertp ssa-block? x)
  (ssa-node-succ x))

(define (ssa-block-succ-set! x succ)
  (assertp ssa-block? x)
  (ssa-node-succ-set! x succ))

(define (ssa-block-add-succ! x block)
   (assertp ssa-block? x)
   (ssa-block-succ-set! x (cons block (ssa-block-succ x))))

(define (ssa-block-add-instr! x instr)
  (cond
   ((and (null? (ssa-block-head x)) (null? (ssa-block-tail x)))
    (ssa-block-head-set! x instr)
    (ssa-block-tail-set! x instr))
   (else
    (ssa-instr-next-set! (ssa-block-tail x) instr)
    (ssa-instr-prev-set! instr (ssa-block-tail x))
    (ssa-block-tail-set! x instr)))
  instr)

;; global variable (compile-time static variable)

(define (ssa-global-name x)
  (assertp ssa-global? x)
  (ssa-node-attr x 'name))



