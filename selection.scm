(declare (unit sl)
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

(define-struct sl-module   (functions))

(define-struct sl-function (name args entry module))

(define-struct sl-block    (name head tail pred succ function))

(define-struct sl-instr    (op mode in1 in2 in3 next prev block attrs))

(define-struct sl-temp     (name))

(define-struct sl-label    (name))

(define-struct sl-constant (size value))

(define-struct sl-data     (size name))


(define *sl-tag*
  '(function block instr temp label const))

(define *sl-type*
  '(i8 i16 i32 i64))

(define sl-make-function make-sl-function)
(define sl-make-block    make-sl-block)
(define sl-make-temp     make-sl-temp)
(define sl-make-label    make-sl-label)
(define sl-make-constant make-sl-constant)

;; constant pool

(define *sl-i1-pool*  '())
(define *sl-i8-pool*  '())
(define *sl-i16-pool* '())
(define *sl-i32-pool* '())
(define *sl-i64-pool* '())

(define (sl-constant-get type value)
  (cond
   ((eq? type 'i1)
    (cond
     ((assq value *sl-i1-pool*) => cdr)
     (else
      (let ((const (sl-make-constant type value)))
        (set! *sl-i1-pool* (cons (cons value const) *sl-i1-pool*))
        const))))
   ((eq? type 'i8)
    (cond
     ((assq value *sl-i8-pool*) => cdr)
     (else
      (let ((const (sl-make-constant type value)))
        (set! *sl-i8-pool* (cons (cons value const) *sl-i8-pool*))
        const))))     
   ((eq? type 'i16)
    (cond
     ((assq value *sl-i16-pool*) => cdr)
     (else
      (let ((const (sl-make-constant type value)))
        (set! *sl-i16-pool* (cons (cons value const) *sl-i16-pool*))
        const))))          
   ((eq? type 'i32)
    (cond
     ((assq value *sl-i32-pool*) => cdr)
     (else
      (let ((const (sl-make-constant type value)))
        (set! *sl-i32-pool* (cons (cons value const) *sl-i32-pool*))
        const))))          
   ((eq? type 'i64)
    (cond
     ((assq value *sl-i64-pool*) => cdr)
     (else
      (let ((const (sl-make-constant type value)))
        (set! *sl-i64-pool* (cons (cons value const) *sl-i64-pool*))
        const))))))

;; temp pool

(define *sl-temp-pool* '())

(define (sl-temp-get name)
  (cond
    ((assq name *sl-temp-pool*) => cdr)
     (else 
      (let ((temp (sl-make-temp name)))
        (set! *sl-temp-pool* (cons (cons name temp) *sl-temp-pool*))
        temp))))

;; label pool

(define *sl-label-pool* '())

(define (sl-label-get name)
  (cond
    ((assq name *sl-label-pool*) => cdr)
     (else 
      (let ((label (sl-make-label name)))
        (set! *sl-label-pool* (cons (cons name label) *sl-label-pool*))
        label))))

;; base constructor

(define-syntax sl-make-instr
  (lambda (e r c)
    (define defaults '(op mode in1 in2 in3 next prev block attrs))
    (match e
      (('sl-make-instr pair* ...)
       `(make-sl-instr ,@(map (lambda (field)
                                 (cond
                                  ((assq field pair*)
                                   => cadr)
                                  (else ''())))
                               defaults))))))

;; constructors for instructions


(define (sl-make-assign name value)
  (let ((node
          (sl-make-instr
           (op  'assign)
           (in1  name)
           (in2  value))))
     node))

(define (sl-make-binop op mode x y)
  (let ((node
          (sl-make-instr
           (op    op)
           (mode  mode)
           (in1   x)
           (in2   y))))
     node))

(define (sl-make-load mode addr)
  (let ((node
         (sl-make-instr
          (op   'load)
          (mode  mode)
          (in1   addr))))
    node))

(define (sl-make-store mode value addr)
  (let ((node
         (sl-make-instr
          (op   'store)
          (mode  mode)
          (in1   value)
          (in2   addr))))
    node))

(define (sl-make-call callconv target args)
  (let ((node
         (sl-make-instr
          (op    'call)
          (in1    target)
          (in2    args)
          (attrs `((callconv . ,callconv))))))
    node))

(define (sl-make-return value)
  (let ((node
         (sl-make-instr
          (op    'return)
          (in1   value))))
    node))

(define (sl-make-br label)
  (let ((node
         (sl-make-instr
          (op      'br)
          (in1      label))))
    node))

(define (sl-make-brc cond labelx labely)
  (let ((node
         (sl-make-instr
          (op      'brc)
          (in1      cond)
          (in2      labelx)
          (in3      labely))))
    node))

(define (sl-make-cmp test x y)
  (let ((node
         (sl-make-instr
          (op    'cmp)
          (mode  'i32)
          (in1    test)
          (in2    x)
          (in3    y))))
    node))


;; node attributes

(define (sl-instr-attr x attr)
  (cond
   ((assq attr (sl-instr-attrs x))
    => cdr)
   (else #f)))

(define (sl-instr-attr-set! x attr value)
  (cond
   ((assq attr (sl-instr-attrs x))
    => (lambda (cell)
         (set-cdr! cell value)))
   (else
    (sl-instr-attrs-set! x (cons (cons attr value) (sl-instr-attrs x))))))

;; builders



(define sl-build-block      sl-make-block)
(define sl-build-load       sl-make-load)
(define sl-build-store      sl-make-store)
(define sl-build-call       sl-make-call)
(define sl-build-return     sl-make-return)
(define sl-build-cmp        sl-make-cmp)
(define sl-build-br         sl-make-br)
(define sl-build-brc        sl-make-brc)
(define sl-build-assign     sl-make-assign)

(define (sl-build-add mode x y)
  (sl-make-binop 'add mode x y))

(define (sl-build-sub mode x y)
  (sl-make-binop 'sub mode x y))

(define (sl-build-mul mode x y)
  (sl-make-binop 'mul mode x y))

(define (sl-build-and mode x y)
  (sl-make-binop 'and mode x y))

(define (sl-build-ior mode x y)
  (sl-make-binop 'ior mode x y))

(define (sl-build-xor mode x y)
  (sl-make-binop 'xor mode x y))

(define (sl-build-shr mode x y)
  (sl-make-binop 'shr mode x y))

(define (sl-build-shl mode x y)
  (sl-make-binop 'shl mode x y))


;; OPS


;; basic

(define (sl-instr-print x port)
  (fprintf port "    ~a\n" (sl-instr-format x)))

;; module

(define (sl-make-module)
  (make-sl-module '()))

(define (sl-module-add-function! mod fun)
  (sl-module-functions-set! mod (cons fun (sl-module-functions mod))))

(define (sl-module-print mod port)
  (for-each
   (lambda (fun)
     (sl-function-print fun port)
     (fprintf port "\n"))
   (sl-module-functions mod)))


;; function

(define (sl-function-add-arg! x arg)
   (assertp sl-function? x)
   (sl-function-args-set! x (cons arg (sl-function-args x))))
  
(define (sl-function-print x port)

  (define (format-arg-list args)
    (string-join
     (map (lambda (arg)
            (format "~a" (sl-instr-format arg)))
          args)
     " "))
  
  (define (print-declaration name args port)
    (fprintf port "(~a (~a)\n" name (format-arg-list args)))
  
  (define (print-body x port)
    (sl-for-each-block
     (lambda (block)
       (sl-block-print block port))
     x)
         (fprintf port ")"))


    (print-declaration
      (sl-function-name x)
      (sl-function-args x)
      port)
    (print-body x port))
 

;; block

(define (sl-block-add-succ! x block)
   (assertp sl-block? x)
   (sl-block-succ-set! x (cons block (sl-block-succ x))))

(define (sl-block-add-statement! x stm)
  (cond
   ((and (null? (sl-block-head x)) (null? (sl-block-tail x)))
    (sl-block-head-set! x stm)
    (sl-block-tail-set! x stm))
   (else
    (sl-instr-next-set! (sl-block-tail x) stm) 
    (sl-instr-prev-set! stm (sl-block-tail x))
    (sl-block-tail-set! x stm)))
  x)

(define (sl-block-print x port)
  (fprintf port "  (~a\n" (sl-block-name x))
  (sl-for-each-statement
   (lambda (instr)
     (sl-instr-print instr port))
   x))

;; binops

(define (sl-binop-left x)
  (assertp sl-instr? x)
  (sl-instr-in1 x))

(define (sl-binop-left-set! x v)
  (assertp sl-instr? x)
  (sl-instr-in1-set! x v))

(define (sl-binop-right x)
  (assertp sl-instr? x)
  (sl-instr-in2 x))

(define (sl-binop-right-set! x v)
  (assertp sl-instr? x)
  (sl-instr-in2-set! x v))

(define (sl-binop-format node)
  (format "(~a ~a ~a ~a)"
    (sl-instr-op node)
    (sl-instr-mode node)
    (sl-instr-format (sl-binop-left node))
    (sl-instr-format (sl-binop-right node))))

  
;; call

(define (sl-call-target x)
  (sl-instr-in1 x))

(define (sl-call-target-set! x v)
  (sl-instr-in1-set! x v))

(define (sl-call-args x)
  (sl-instr-in2 x))

(define (sl-call-args-set! x v)
  (sl-instr-in2 x v))

(define (sl-call-format node)
  (sprintf "(call ~a ~a (~a))"
          (sl-instr-mode node)
          (sl-instr-format (sl-call-target node))
          (string-join
            (map (lambda (arg)
                  (sl-instr-format arg))
                  (sl-call-args node))
           " ")))
          
;; ret

(define (sl-ret-value x)
  (sl-instr-in1 x))

(define (sl-ret-value-set! x)
  (sl-instr-in1 x))

(define (sl-ret-format node)
  (format "(ret ~a)"
          (sl-instr-format (sl-ret-value node))))

;; load

(define (sl-load-addr x)
  (sl-instr-in1 x))

(define (sl-load-addr-set! x v)
  (sl-instr-in1-set! x v))


(define (sl-load-format node)
  (format "(load ~a ~a)"
          (sl-instr-mode node)
          (sl-instr-format (sl-load-addr node))))

;; store

(define (sl-store-value x)
  (sl-instr-in1 x))

(define (sl-store-value-set! x v)
  (sl-instr-in1-set! x v))

(define (sl-store-addr x)
  (sl-instr-in2 x))

(define (sl-store-addr-set! x v)
  (sl-instr-in2 x v))

(define (sl-store-format node)
  (format "(store ~a ~a ~a)"
          (sl-instr-mode node)
          (sl-instr-format (sl-store-value node))
          (sl-instr-format (sl-store-addr  node))))

;; conditional branch 

(define (sl-brc-cond x)
  (sl-instr-in1 x))

(define (sl-brc-cond-set! x v)
  (sl-instr-in1-set! x v))

(define (sl-brc-labelx x)
  (sl-instr-in2 x))

(define (sl-brc-labelx-set! x v)
  (sl-instr-in2-set! x v))

(define (sl-brc-labely x)
  (sl-instr-in3 x))

(define (sl-brc-labely-set! x v)
  (sl-instr-in3-set! x v))

(define (sl-brc-format node)
  (format "(brc ~a ~a ~a)"
          (sl-instr-format (sl-brc-cond node))
          (sl-instr-format (sl-brc-labelx node))
          (sl-instr-format (sl-brc-labely node))))

;; unconditional branch 

(define (sl-br-label x)
  (sl-instr-in1 x))

(define (sl-br-label-set! x v)
  (sl-instr-in1-set! x v))

(define (sl-br-format node)
  (format "(br ~a)"
          (sl-instr-format (sl-br-label x))))

;; cmp

(define (sl-cmp-test x)
  (sl-instr-in1 x))

(define (sl-cmp-test-set! x test)
  (sl-instr-in1-set! x test))

(define (sl-cmp-x x)
  (sl-instr-in2 x))

(define (sl-cmp-x-set! x v)
  (sl-instr-in2-set! x v))

(define (sl-cmp-y x)
  (sl-instr-in3 x))

(define (sl-cmp-y-set! x v)
  (sl-instr-in3-set! x v))

(define (sl-cmp-format x)
  (format "(cmp ~a ~a ~a ~a)"
          (sl-instr-mode x)
          (sl-cmp-test x)
          (sl-instr-format (sl-cmp-x x))
          (sl-instr-format (sl-cmp-y x))))

;; assign

(define (sl-assign-name x)
 (sl-instr-in1 x))

(define (sl-assign-value x)
 (sl-instr-in2 x))

(define (sl-assign-format x)
  (format "(assign ~a ~a)"
          (sl-assign-name x)
          (sl-instr-format (sl-assign-value x))))

;; function traversal

(define (sl-for-each-function f mod)
  (let ((funcs (sl-module-functions f)))
    (for-each f funcs)))

;; block traversal

(define (sl-for-each-block f fun)
  (define (visit-block block f)
    (let ((succ (sl-block-succ block)))
      (f block)
      (for-each (lambda (succ)
                  (visit-block succ f))
                succ)))
    (visit-block (sl-function-entry fun) f))

(define (sl-for-each-block-succ f block)
  (for-each f (sl-block-succ block)))

(define (sl-for-each-block-pred f block)
  (f (sl-block-pred block)))

;; instruction traversal operations

(define (sl-for-each-statement f block)
  (let ((head (sl-block-head block)))
    (let walk ((x head))
      (cond
       ((not (null? x)) 
        (f x)
        (walk (sl-instr-next x)))))))

;; formatting

(define (sl-instr-format x)
  (cond
   ;; atoms
   ((sl-constant? x)
    (format "(~a ~a)" (sl-constant-size x) (sl-constant-value x)))
   ((sl-temp? x)
    (format "$~a" (sl-temp-name x)))
   ((sl-label? x)
    (format "@~a" (sl-label-name x)))
   ((sl-instr? x)
    (case (sl-instr-op x)
      ((add sub mul and ior shl shr)
       (sl-binop-format  x))
      ((call)
       (sl-call-format x))
      ((ret)
       (sl-ret-format x))
      ((br)
       (sl-br-format x))
      ((brc)
       (sl-brc-format x))
      ((load)
       (sl-load-format x))
      ((store)
       (sl-store-format x))
      ((cmp)
       (sl-cmp-format x))
      ((assign)
       (sl-assign-format x))))
   (else (assert-not-reached))))



