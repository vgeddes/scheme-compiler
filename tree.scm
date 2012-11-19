(declare (unit tree)
         (uses extras utils))

(use srfi-1)
(use srfi-13)
(use matchable)

(import-for-syntax matchable)

(include "struct-syntax")

(define-syntax assertp
  (syntax-rules ()
    ((assertp pred x)
     (assert (pred x) "invalid type"))))

;; Selection language

(define-struct tree-module   (functions))

(define-struct tree-function (name args entry module))

(define-struct tree-block    (name head tail pred succ function))

(define-struct tree-instr    (op mode in1 in2 in3 next prev block attrs))

(define-struct tree-temp     (name))

(define-struct tree-label    (name))

(define-struct tree-constant (size value))

(define-struct tree-data     (size name))


(define *tree-tag*
  '(function block instr temp label const))

(define *tree-type*
  '(i8 i16 i32 i64))

(define tree-make-function make-tree-function)
(define tree-make-block    make-tree-block)
(define tree-make-temp     make-tree-temp)
(define tree-make-label    make-tree-label)
(define tree-make-constant make-tree-constant)

;; constant pool

(define *tree-i1-pool*  '())
(define *tree-i8-pool*  '())
(define *tree-i16-pool* '())
(define *tree-i32-pool* '())
(define *tree-i64-pool* '())

(define (tree-constant-get type value)
  (cond
   ((eq? type 'i1)
    (cond
     ((assq value *tree-i1-pool*) => cdr)
     (else
      (let ((const (tree-make-constant type value)))
        (set! *tree-i1-pool* (cons (cons value const) *tree-i1-pool*))
        const))))
   ((eq? type 'i8)
    (cond
     ((assq value *tree-i8-pool*) => cdr)
     (else
      (let ((const (tree-make-constant type value)))
        (set! *tree-i8-pool* (cons (cons value const) *tree-i8-pool*))
        const))))     
   ((eq? type 'i16)
    (cond
     ((assq value *tree-i16-pool*) => cdr)
     (else
      (let ((const (tree-make-constant type value)))
        (set! *tree-i16-pool* (cons (cons value const) *tree-i16-pool*))
        const))))          
   ((eq? type 'i32)
    (cond
     ((assq value *tree-i32-pool*) => cdr)
     (else
      (let ((const (tree-make-constant type value)))
        (set! *tree-i32-pool* (cons (cons value const) *tree-i32-pool*))
        const))))          
   ((eq? type 'i64)
    (cond
     ((assq value *tree-i64-pool*) => cdr)
     (else
      (let ((const (tree-make-constant type value)))
        (set! *tree-i64-pool* (cons (cons value const) *tree-i64-pool*))
        const))))))

;; temp pool

(define *tree-temp-pool* '())

(define (tree-temp-get name)
  (cond
    ((assq name *tree-temp-pool*) => cdr)
     (else 
      (let ((temp (tree-make-temp name)))
        (set! *tree-temp-pool* (cons (cons name temp) *tree-temp-pool*))
        temp))))

;; label pool

(define *tree-label-pool* '())

(define (tree-label-get name)
  (cond
    ((assq name *tree-label-pool*) => cdr)
     (else 
      (let ((label (tree-make-label name)))
        (set! *tree-label-pool* (cons (cons name label) *tree-label-pool*))
        label))))

;; base constructor

(define-syntax tree-make-instr
  (lambda (e r c)
    (define defaults '(op mode in1 in2 in3 next prev block attrs))
    (match e
      (('tree-make-instr pair* ...)
       `(make-tree-instr ,@(map (lambda (field)
                                 (cond
                                  ((assq field pair*)
                                   => cadr)
                                  (else ''())))
                               defaults))))))

;; constructors for instructions


(define (tree-make-assign name value)
  (let ((node
          (tree-make-instr
           (op  'assign)
           (in1  name)
           (in2  value))))
     node))

(define (tree-make-binop op mode x y)
  (let ((node
          (tree-make-instr
           (op    op)
           (mode  mode)
           (in1   x)
           (in2   y))))
     node))

(define (tree-make-load mode addr)
  (let ((node
         (tree-make-instr
          (op   'load)
          (mode  mode)
          (in1   addr))))
    node))

(define (tree-make-store mode value addr)
  (let ((node
         (tree-make-instr
          (op   'store)
          (mode  mode)
          (in1   value)
          (in2   addr))))
    node))

(define (tree-make-call callconv target args)
  (let ((node
         (tree-make-instr
          (op    'call)
          (in1    target)
          (in2    args)
          (attrs `((callconv . ,callconv))))))
    node))

(define (tree-make-return value)
  (let ((node
         (tree-make-instr
          (op    'return)
          (in1   value))))
    node))

(define (tree-make-br label)
  (let ((node
         (tree-make-instr
          (op      'br)
          (in1      label))))
    node))

(define (tree-make-brc cond labelx labely)
  (let ((node
         (tree-make-instr
          (op      'brc)
          (in1      cond)
          (in2      labelx)
          (in3      labely))))
    node))

(define (tree-make-cmp test x y)
  (let ((node
         (tree-make-instr
          (op    'cmp)
          (mode  'i64)
          (in1    test)
          (in2    x)
          (in3    y))))
    node))


;; node attributes

(define (tree-instr-attr x attr)
  (cond
   ((assq attr (tree-instr-attrs x))
    => cdr)
   (else #f)))

(define (tree-instr-attr-set! x attr value)
  (cond
   ((assq attr (tree-instr-attrs x))
    => (lambda (cell)
         (set-cdr! cell value)))
   (else
    (tree-instr-attrs-set! x (cons (cons attr value) (tree-instr-attrs x))))))

;; builders



(define tree-build-block      tree-make-block)
(define tree-build-load       tree-make-load)
(define tree-build-store      tree-make-store)
(define tree-build-call       tree-make-call)
(define tree-build-return     tree-make-return)
(define tree-build-cmp        tree-make-cmp)
(define tree-build-br         tree-make-br)
(define tree-build-brc        tree-make-brc)
(define tree-build-assign     tree-make-assign)

(define (tree-build-add mode x y)
  (tree-make-binop 'add mode x y))

(define (tree-build-sub mode x y)
  (tree-make-binop 'sub mode x y))

(define (tree-build-mul mode x y)
  (tree-make-binop 'mul mode x y))

(define (tree-build-and mode x y)
  (tree-make-binop 'and mode x y))

(define (tree-build-ior mode x y)
  (tree-make-binop 'ior mode x y))

(define (tree-build-xor mode x y)
  (tree-make-binop 'xor mode x y))

(define (tree-build-shr mode x y)
  (tree-make-binop 'shr mode x y))

(define (tree-build-shl mode x y)
  (tree-make-binop 'shl mode x y))


;; OPS


;; basic

(define (tree-instr-print x port)
  (fprintf port "    ~a\n" (tree-instr-format x)))

;; module

(define (tree-make-module)
  (make-tree-module '()))

(define (tree-module-add-function! mod fun)
  (tree-module-functions-set! mod (cons fun (tree-module-functions mod))))

(define (tree-module-print mod port)
  (for-each
   (lambda (fun)
     (tree-function-print fun port)
     (fprintf port "\n"))
   (tree-module-functions mod)))


;; function

(define (tree-function-add-arg! x arg)
   (assertp tree-function? x)
   (tree-function-args-set! x (cons arg (tree-function-args x))))
  
(define (tree-function-print x port)

  (define (format-arg-list args)
    (string-join
     (map (lambda (arg)
            (format "~a" (tree-instr-format arg)))
          args)
     " "))
  
  (define (print-declaration name args port)
    (fprintf port "(~a (~a)\n" name (format-arg-list args)))
  
  (define (print-body x port)
    (tree-for-each-block
     (lambda (block)
       (tree-block-print block port))
     x)
         (fprintf port ")"))


    (print-declaration
      (tree-function-name x)
      (tree-function-args x)
      port)
    (print-body x port))
 

;; block

(define (tree-block-add-succ! x block)
   (assertp tree-block? x)
   (tree-block-succ-set! x (cons block (tree-block-succ x))))

(define (tree-block-add-statement! x stm)
  (cond
   ((and (null? (tree-block-head x)) (null? (tree-block-tail x)))
    (tree-block-head-set! x stm)
    (tree-block-tail-set! x stm))
   (else
    (tree-instr-next-set! (tree-block-tail x) stm) 
    (tree-instr-prev-set! stm (tree-block-tail x))
    (tree-block-tail-set! x stm)))
  x)

(define (tree-block-print x port)
  (fprintf port "  (~a\n" (tree-block-name x))
  (tree-for-each-statement
   (lambda (instr)
     (tree-instr-print instr port))
   x))

;; binops

(define (tree-binop-left x)
  (assertp tree-instr? x)
  (tree-instr-in1 x))

(define (tree-binop-left-set! x v)
  (assertp tree-instr? x)
  (tree-instr-in1-set! x v))

(define (tree-binop-right x)
  (assertp tree-instr? x)
  (tree-instr-in2 x))

(define (tree-binop-right-set! x v)
  (assertp tree-instr? x)
  (tree-instr-in2-set! x v))

(define (tree-binop-format node)
  (format "(~a ~a ~a ~a)"
    (tree-instr-op node)
    (tree-instr-mode node)
    (tree-instr-format (tree-binop-left node))
    (tree-instr-format (tree-binop-right node))))

  
;; call

(define (tree-call-target x)
  (tree-instr-in1 x))

(define (tree-call-target-set! x v)
  (tree-instr-in1-set! x v))

(define (tree-call-args x)
  (tree-instr-in2 x))

(define (tree-call-args-set! x v)
  (tree-instr-in2 x v))

(define (tree-call-format node)
  (sprintf "(call ~a (~a))"
          (tree-instr-format (tree-call-target node))
          (string-join
            (map (lambda (arg)
                  (tree-instr-format arg))
                  (tree-call-args node))
           " ")))
          
;; ret

(define (tree-ret-value x)
  (tree-instr-in1 x))

(define (tree-ret-value-set! x)
  (tree-instr-in1 x))

(define (tree-ret-format node)
  (format "(ret ~a)"
          (tree-instr-format (tree-ret-value node))))

;; load

(define (tree-load-addr x)
  (tree-instr-in1 x))

(define (tree-load-addr-set! x v)
  (tree-instr-in1-set! x v))


(define (tree-load-format node)
  (format "(load ~a ~a)"
          (tree-instr-mode node)
          (tree-instr-format (tree-load-addr node))))

;; store

(define (tree-store-value x)
  (tree-instr-in1 x))

(define (tree-store-value-set! x v)
  (tree-instr-in1-set! x v))

(define (tree-store-addr x)
  (tree-instr-in2 x))

(define (tree-store-addr-set! x v)
  (tree-instr-in2 x v))

(define (tree-store-format node)
  (format "(store ~a ~a ~a)"
          (tree-instr-mode node)
          (tree-instr-format (tree-store-value node))
          (tree-instr-format (tree-store-addr  node))))

;; conditional branch 

(define (tree-brc-cond x)
  (tree-instr-in1 x))

(define (tree-brc-cond-set! x v)
  (tree-instr-in1-set! x v))

(define (tree-brc-labelx x)
  (tree-instr-in2 x))

(define (tree-brc-labelx-set! x v)
  (tree-instr-in2-set! x v))

(define (tree-brc-labely x)
  (tree-instr-in3 x))

(define (tree-brc-labely-set! x v)
  (tree-instr-in3-set! x v))

(define (tree-brc-format node)
  (format "(brc ~a ~a ~a)"
          (tree-instr-format (tree-brc-cond node))
          (tree-instr-format (tree-brc-labelx node))
          (tree-instr-format (tree-brc-labely node))))

;; unconditional branch 

(define (tree-br-label x)
  (tree-instr-in1 x))

(define (tree-br-label-set! x v)
  (tree-instr-in1-set! x v))

(define (tree-br-format node)
  (format "(br ~a)"
          (tree-instr-format (tree-br-label x))))

;; cmp

(define (tree-cmp-test x)
  (tree-instr-in1 x))

(define (tree-cmp-test-set! x test)
  (tree-instr-in1-set! x test))

(define (tree-cmp-x x)
  (tree-instr-in2 x))

(define (tree-cmp-x-set! x v)
  (tree-instr-in2-set! x v))

(define (tree-cmp-y x)
  (tree-instr-in3 x))

(define (tree-cmp-y-set! x v)
  (tree-instr-in3-set! x v))

(define (tree-cmp-format x)
  (format "(cmp ~a ~a ~a ~a)"
          (tree-instr-mode x)
          (tree-cmp-test x)
          (tree-instr-format (tree-cmp-x x))
          (tree-instr-format (tree-cmp-y x))))

;; assign

(define (tree-assign-name x)
 (tree-instr-in1 x))

(define (tree-assign-value x)
 (tree-instr-in2 x))

(define (tree-assign-format x)
  (format "(assign ~a ~a)"
          (tree-assign-name x)
          (tree-instr-format (tree-assign-value x))))

;; function traversal

(define (tree-for-each-function f mod)
  (let ((funcs (tree-module-functions f)))
    (for-each f funcs)))

;; block traversal

(define (tree-for-each-block f fun)
  (define (visit-block block f)
    (let ((succ (tree-block-succ block)))
      (f block)
      (for-each (lambda (succ)
                  (visit-block succ f))
                succ)))
    (visit-block (tree-function-entry fun) f))

(define (tree-for-each-block-succ f block)
  (for-each f (tree-block-succ block)))

(define (tree-for-each-block-pred f block)
  (f (tree-block-pred block)))

;; instruction traversal operations

(define (tree-for-each-statement f block)
  (let ((head (tree-block-head block)))
    (let walk ((x head))
      (cond
       ((not (null? x)) 
        (f x)
        (walk (tree-instr-next x)))))))

;; formatting

(define (tree-instr-format x)
  (cond
   ;; atoms
   ((tree-constant? x)
    (format "(~a ~a)" (tree-constant-size x) (tree-constant-value x)))
   ((tree-temp? x)
    (format "$~a" (tree-temp-name x)))
   ((tree-label? x)
    (format "@~a" (tree-label-name x)))
   ((tree-instr? x)
    (case (tree-instr-op x)
      ((add sub mul and ior shl shr)
       (tree-binop-format  x))
      ((call)
       (tree-call-format x))
      ((return)
       (tree-ret-format x))
      ((br)
       (tree-br-format x))
      ((brc)
       (tree-brc-format x))
      ((load)
       (tree-load-format x))
      ((store)
       (tree-store-format x))
      ((cmp)
       (tree-cmp-format x))
      ((assign)
       (tree-assign-format x))))
   (else (assert-not-reached))))



