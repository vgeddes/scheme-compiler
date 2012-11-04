
(declare (unit ssa)
         (uses extras ssa-types ssa-ops ssa-const ssa-transforms utils))

(use srfi-1)
(use srfi-13)
(use matchable)

(import-for-syntax matchable)

(include "struct-syntax")
;;(include "ssa-ops.scm")

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

(define *ssa-tag*
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

(define (ssa-ptrtoint? x)
  (ssa-instr-is-a? x <ssa-op-ptrtoint>))

(define (ssa-inttoptr? x)
  (ssa-instr-is-a? x <ssa-op-inttoptr>))

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
          (tag     'block)
          (block    fun)
          (attrs   `((name . ,name))))))
    (ssa-function-add-block! fun node)
    node))

;; constructors for functions

(define (ssa-make-function type name mod)
  (let ((node
         (ssa-node-with-attrs
          (type   (ssa-type-pointer-get type))
          (tag   'function)
          (attrs `((name . ,name))))))
    (ssa-module-add-function! mod node)
    node))


;; constructors for instructions

(define (ssa-make-binop block op type x y)
  (let ((node
         (ssa-node-with-attrs
          (type     type)
          (tag     'instr)
          (op       op)
          (in1      x)
          (in2      y)
          (block block))))
    (ssa-add-user! x node)
    (ssa-add-user! y node)
    (ssa-block-add-instr! block node)))

(define (ssa-make-load block ptr)
  (let ((node
         (ssa-node-with-attrs
          (type  (ssa-type-pointer-points-to-type (ssa-node-type ptr)))
          (tag  'instr)
          (op    <ssa-op-load>)
          (in1   ptr)
          (block block))))
    (ssa-add-user! ptr node)
    (ssa-block-add-instr! block node)))

(define (ssa-make-store block value ptr)
  (let ((node
         (ssa-node-with-attrs
          (type  <ssa-void>)
          (tag    'instr)
          (op    <ssa-op-store>)
          (in1   ptr)
          (in2   value)
          (block block))))
    (ssa-add-user! value node)
    (ssa-add-user! ptr   node)
    (ssa-block-add-instr! block node)))

(define (ssa-make-call block callconv target args)
  (let ((node
         (ssa-node-with-attrs
          (type   (ssa-type-function-return-type (ssa-type-pointer-points-to-type (ssa-node-type target))))
          (tag   'instr)
          (op    <ssa-op-call>)
          (in1    target)
          (in2    args)
          (block  block)
          (attrs `((callconv . ,callconv))))))
    (ssa-add-user! target node)
    (for-each (lambda (arg)
                (ssa-add-user! arg node))
              args)
    (ssa-block-add-instr! block node)))

(define (ssa-make-ret block value)
  (let ((node
         (ssa-node-with-attrs
          (type  <ssa-void>)
          (tag   'instr)
          (op    <ssa-op-ret>)
          (in1   value)
          (block block))))
    (ssa-block-add-instr! block node)))

(define (ssa-make-br block label)
  (let ((node
         (ssa-node-with-attrs
          (type     <ssa-void>)
          (tag     'instr)
          (op       <ssa-op-br>)
          (in1      label)
          (block    block))))
    (ssa-add-user! label node)
    (ssa-block-add-instr! block node)))

(define (ssa-make-brc block cond labelx labely)
  (let ((node
         (ssa-node-with-attrs
          (type     <ssa-void>)
          (tag     'instr)
          (op       <ssa-op-brc>)
          (in1      cond)
          (in2      labelx)
          (in3      labely)
          (block    block))))
    (ssa-add-user! cond   node)
    (ssa-add-user! labelx node)
    (ssa-add-user! labely node)
    (ssa-block-add-instr! block node)))

(define (ssa-make-phi block in)
  (let ((node
         (ssa-node-with-attrs
          (type  (ssa-node-type (car in)))
          (tag   'instr)
          (op    <ssa-op-phi>)
          (in1   in)
          (block block))))
    (ssa-block-add-instr! block node)))

(define (ssa-make-elementptr block ptr index)
  (let ((node
         (ssa-node-with-attrs
          (type  (ssa-node-type ptr))
          (tag  'instr)
          (op    <ssa-op-elementptr>)
          (in1   ptr)
          (in2   index)
          (block block))))
    (ssa-add-user! ptr   node)
    (ssa-add-user! index node)
    (ssa-block-add-instr! block node)))

(define (ssa-make-cast block type value)
  (let ((node
         (ssa-node-with-attrs
          (type     type)
          (tag     'instr)
          (op       <ssa-op-cast>)
          (in1      value)
          (block    block))))
    (ssa-add-user! value node)
    (ssa-block-add-instr! block node)))

(define (ssa-make-ptrtoint block type value)
  (let ((node
         (ssa-node-with-attrs
          (type     type)
          (tag     'instr)
          (op       <ssa-op-ptrtoint>)
          (in1      value)
          (block    block))))
    (ssa-add-user! value node)
    (ssa-block-add-instr! block node)))

(define (ssa-make-inttoptr block type value)
  (let ((node
         (ssa-node-with-attrs
          (type     type)
          (tag     'instr)
          (op       <ssa-op-inttoptr>)
          (in1      value)
          (block    block))))
    (ssa-add-user! value node)
    (ssa-block-add-instr! block node)))

(define (ssa-make-cmp block test x y)
  (let ((node
         (ssa-node-with-attrs
          (type   <ssa-i1>)
          (tag   'instr)
          (op     <ssa-op-cmp>)
          (in1    test)
          (in2    x)
          (in3    y)
          (block  block))))
    (ssa-add-user! x node)
    (ssa-add-user! y node)
    (ssa-block-add-instr! block node)))

;; constructors for atoms

(define (ssa-make-arg type fun)
  (let ((node
         (ssa-node-with-attrs
          (type     type)
          (tag     'arg))))
    (ssa-function-add-arg! fun node)
    node))

(define (ssa-make-global type name is-constant initializer mod)
  (let ((node
         (ssa-node-with-attrs
          (type   type)
          (tag   'global)
          (in1    name)
          (in2    initializer)
          (attrs `((is-constant . ,is-constant))))))
    (ssa-module-add-global! mod node)
    node))

;; constructor for const
(define (ssa-make-const type value)
  (ssa-node-with-attrs
   (type  type)
   (tag  'const)
   (in1   value)))

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
(define ssa-build-cast       ssa-make-cast)
(define ssa-build-ptrtoint   ssa-make-ptrtoint)
(define ssa-build-inttoptr   ssa-make-inttoptr)
(define ssa-build-call       ssa-make-call)
(define ssa-build-ret        ssa-make-ret)
(define ssa-build-cmp        ssa-make-cmp)
(define ssa-build-br         ssa-make-br)
(define ssa-build-brc        ssa-make-brc)

(define (ssa-build-add block x y)
  (cond
   ((and (ssa-constant? x) (ssa-constant? y))
    (ssa-fold-add x y))
   (else (ssa-make-binop block <ssa-op-add> (ssa-node-type x) x y))))

(define (ssa-build-sub block x y)
  (cond
   ((and (ssa-constant? x) (ssa-constant? y))
    (ssa-fold-sub x y))
   (else (ssa-make-binop block <ssa-op-sub> (ssa-node-type x) x y))))

(define (ssa-build-mul block x y)
  (cond
   ((and (ssa-constant? x) (ssa-constant? y))
    (ssa-fold-mul x y))
   (else (ssa-make-binop block <ssa-op-mul> (ssa-node-type x) x y))))

(define (ssa-build-and block x y)
  (cond
   ((and (ssa-constant? x) (ssa-constant? y))
    (ssa-fold-and x y))
  (else (ssa-make-binop block <ssa-op-and> (ssa-node-type x) x y))))

(define (ssa-build-or block x y)
 (cond
  ((and (ssa-constant? x) (ssa-constant? y))
   (ssa-fold-or x y))  
  (else (ssa-make-binop block <ssa-op-or> (ssa-node-type x) x y))))

(define (ssa-build-xor block x y)
 (cond
  ((and (ssa-constant? x) (ssa-constant? y))
   (ssa-fold-xor x y))    
  (else (ssa-make-binop block <ssa-op-xor> (ssa-node-type x) x y))))

(define (ssa-build-shr block x y)
  (cond
   ((and (ssa-constant? x) (ssa-constant? y))
    (ssa-fold-shr x y))    
   (else (ssa-make-binop block <ssa-op-shr> (ssa-node-type x) x y))))

(define (ssa-build-shl block x y)
  (cond
   ((and (ssa-constant? x) (ssa-constant? y))
    (ssa-fold-shl x y))    
   (else (ssa-make-binop block <ssa-op-shl> (ssa-node-type x) x y))))

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

(define (ssa-module-print mod port)
  (for-each
   (lambda (fun)
     (ssa-function-print fun port)
     (fprintf port "\n"))
   (ssa-module-functions mod)))

;; polymorphic instructions

(define (ssa-instr-iterate-uses f x)
  ((ssa-op-iterate-uses (ssa-node-op x)) f x))

(define (ssa-instr-replace-uses f x)
  ((ssa-op-replace-uses (ssa-node-op x)) f x))

(define (ssa-instr-list-uses f x)
  ((ssa-op-list-uses (ssa-node-op x)) x))

(define (ssa-instr-print x port)
  (cond
   ((ssa-type-void? (ssa-node-type x))
    (fprintf port "  ~a\n"
             ((ssa-op-format (ssa-node-op x)) x)))
   (else
    (fprintf port "  ~a = ~a\n"
             (ssa-format-value x)
             ((ssa-op-format (ssa-node-op x)) x)))))

(define (ssa-instr-constant-fold x)
  ((ssa-op-constant-fold (ssa-node-op x)) x))

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


;; function

(define (ssa-function-return-type x)
  (assertp ssa-function? x)
  (ssa-type-function-return-type (ssa-type-pointer-points-to-type (ssa-node-type x))))

(define (ssa-function-param-types x)
  (assertp ssa-function? x)
  (ssa-type-function-param-types (ssa-type-pointer-points-to-type (ssa-node-type x))))

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
  (and (ssa-node-attr x 'is-definition)
       (not (null? (ssa-function-entry x)))))
  
(define (ssa-function-print x port)

  (define (format-param-list params param-types)
    (string-join
     (map (lambda (param param-type)
            (format "~a ~a" (ssa-format-type param-type) (ssa-format-value param)))
          params param-types)
     ", "))
  
  (define (print-declaration name params ret-type param-types port)
    (fprintf port "~a\n" (ssa-format-type ret-type))
    (fprintf port "@~a (~a)\n" name (format-param-list params param-types)))     
  
  (define (print-body x port)
    (fprintf port "{\n")
    (ssa-for-each-block
     (lambda (block)
       (ssa-block-print block port))
     x)
    (fprintf port "}\n"))
  (cond
   ((ssa-function-is-definition? x)
    (print-declaration
     (ssa-function-name x)
     (ssa-function-args x)
     (ssa-function-return-type x)
     (ssa-function-param-types x)
     port)
    (print-body x port))
   ((ssa-function-is-declaration? x)
    (print-declaration
     (ssa-function-name x)
     (ssa-function-args x)
     (ssa-function-return-type x)
     (ssa-function-param-types x)
     port))
   (else (assert-not-reached))))
 

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
  (ssa-node-prev x))

(define (ssa-block-pred-set! x pred)
  (assertp ssa-block? x)
  (ssa-node-prev-set! x pred))

(define (ssa-block-succ x)
  (assertp ssa-block? x)
  (ssa-node-next x))

(define (ssa-block-succ-set! x succ)
  (assertp ssa-block? x)
  (ssa-node-next-set! x succ))

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

(define (ssa-block-print x port)
  (fprintf port "~a:\n" (ssa-block-name x))
  (ssa-for-each-instr
   (lambda (instr)
     (ssa-instr-print instr port))
   x))

;; global variable (compile-time static variable)

(define (ssa-global-name x)
  (assertp ssa-global? x)
  (ssa-node-attr x 'name))



