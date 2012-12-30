(declare (unit tree)
         (uses helpers))

(module tree *

  (import scheme)
  (import chicken)
  (import srfi-1)
  (import srfi-13)
  (import matchable)
  (import-for-syntax matchable)
  (import helpers)

  (define-syntax assertp
    (syntax-rules ()
      ((assertp pred x)
       (assert (pred x) "invalid type"))))

  ;; Selection language

  (define-struct module   (functions))

  (define-struct function (name args entry module))

  (define-struct block    (name head tail pred succ function))

  (define-struct instr    (op mode in1 in2 in3 next prev block attrs))

  (define-struct temp     (name))

  (define-struct label    (name))

  (define-struct constant (size value))

  (define-struct data     (size name))


  (define *tag*
    '(function block instr temp label const))

  (define *type*
    '(i8 i16 i32 i64))

  ;; constant pool

  (define *i1-pool*  '())
  (define *i8-pool*  '())
  (define *i16-pool* '())
  (define *i32-pool* '())
  (define *i64-pool* '())

  (define (constant-get type value)
    (cond
     ((eq? type 'i1)
      (cond
       ((assq value *i1-pool*) => cdr)
       (else
        (let ((const (make-constant type value)))
          (set! *i1-pool* (cons (cons value const) *i1-pool*))
          const))))
     ((eq? type 'i8)
      (cond
       ((assq value *i8-pool*) => cdr)
       (else
        (let ((const (make-constant type value)))
          (set! *i8-pool* (cons (cons value const) *i8-pool*))
          const))))
     ((eq? type 'i16)
      (cond
       ((assq value *i16-pool*) => cdr)
       (else
        (let ((const (make-constant type value)))
          (set! *i16-pool* (cons (cons value const) *i16-pool*))
          const))))
     ((eq? type 'i32)
      (cond
       ((assq value *i32-pool*) => cdr)
       (else
        (let ((const (make-constant type value)))
          (set! *i32-pool* (cons (cons value const) *i32-pool*))
          const))))
     ((eq? type 'i64)
      (cond
       ((assq value *i64-pool*) => cdr)
       (else
        (let ((const (make-constant type value)))
          (set! *i64-pool* (cons (cons value const) *i64-pool*))
          const))))))

  ;; temp pool

  (define *temp-pool* '())

  (define (temp-get name)
    (cond
     ((assq name *temp-pool*) => cdr)
     (else
      (let ((temp (make-temp name)))
        (set! *temp-pool* (cons (cons name temp) *temp-pool*))
        temp))))

  ;; label pool

  (define *label-pool* '())

  (define (label-get name)
    (cond
     ((assq name *label-pool*) => cdr)
     (else
      (let ((label (make-label name)))
        (set! *label-pool* (cons (cons name label) *label-pool*))
        label))))

  ;; base constructor

  (define-syntax make-instr-internal
    (lambda (e r c)
      (define defaults '(op mode in1 in2 in3 next prev block attrs))
      (match e
        (('make-instr-internal pair* ...)
         `(make-instr ,@(map (lambda (field)
                               (cond
                                ((assq field pair*)
                                 => cadr)
                                (else ''())))
                             defaults))))))

  ;; constructors for instructions


  (define (make-assign name value)
    (let ((node
           (make-instr-internal
            (op  'assign)
            (in1  name)
            (in2  value))))
      node))

  (define (make-binop op mode x y)
    (let ((node
           (make-instr-internal
            (op    op)
            (mode  mode)
            (in1   x)
            (in2   y))))
      node))

  (define (make-load mode addr)
    (let ((node
           (make-instr-internal
            (op   'load)
            (mode  mode)
            (in1   addr))))
      node))

  (define (make-store mode value addr)
    (let ((node
           (make-instr-internal
            (op   'store)
            (mode  mode)
            (in1   value)
            (in2   addr))))
      node))

  (define (make-call callconv target args)
    (let ((node
           (make-instr-internal
            (op    'call)
            (in1    target)
            (in2    args)
            (attrs `((callconv . ,callconv))))))
      node))

  (define (make-return value)
    (let ((node
           (make-instr-internal
            (op    'return)
            (in1   value))))
      node))

  (define (make-br label)
    (let ((node
           (make-instr-internal
            (op      'br)
            (in1      label))))
      node))

  (define (make-brc cond labelx labely)
    (let ((node
           (make-instr-internal
            (op      'brc)
            (in1      cond)
            (in2      labelx)
            (in3      labely))))
      node))

  (define (make-cmp test x y)
    (let ((node
           (make-instr-internal
            (op    'cmp)
            (mode  'i64)
            (in1    test)
            (in2    x)
            (in3    y))))
      node))


  ;; node attributes

  (define (instr-attr x attr)
    (cond
     ((assq attr (instr-attrs x))
      => cdr)
     (else #f)))

  (define (instr-attr-set! x attr value)
    (cond
     ((assq attr (instr-attrs x))
      => (lambda (cell)
           (set-cdr! cell value)))
     (else
      (instr-attrs-set! x (cons (cons attr value) (instr-attrs x))))))

  ;; builders

  (define build-block      make-block)
  (define build-load       make-load)
  (define build-store      make-store)
  (define build-call       make-call)
  (define build-return     make-return)
  (define build-cmp        make-cmp)
  (define build-br         make-br)
  (define build-brc        make-brc)
  (define build-assign     make-assign)

  (define (build-add mode x y)
    (make-binop 'add mode x y))

  (define (build-sub mode x y)
    (make-binop 'sub mode x y))

  (define (build-mul mode x y)
    (make-binop 'mul mode x y))

  (define (build-and mode x y)
    (make-binop 'and mode x y))

  (define (build-ior mode x y)
    (make-binop 'ior mode x y))

  (define (build-xor mode x y)
    (make-binop 'xor mode x y))

  (define (build-shr mode x y)
    (make-binop 'shr mode x y))

  (define (build-shl mode x y)
    (make-binop 'shl mode x y))


  ;; OPS


  ;; basic

  (define (instr-print x port)
    (fprintf port "    ~a\n" (instr-format x)))

  ;; module

  (define (module-add-function! mod fun)
    (module-functions-set! mod (cons fun (module-functions mod))))

  (define (module-print mod port)
    (for-each
     (lambda (fun)
       (function-print fun port)
       (fprintf port "\n"))
     (module-functions mod)))


  ;; function

  (define (function-add-arg! x arg)
    (assertp function? x)
    (function-args-set! x (cons arg (function-args x))))

  (define (function-print x port)

    (define (format-arg-list args)
      (string-join
       (map (lambda (arg)
              (format "~a" (instr-format arg)))
            args)
       " "))

    (define (print-declaration name args port)
      (fprintf port "(~a (~a)\n" name (format-arg-list args)))

    (define (print-body x port)
      (for-each-block
       (lambda (block)
         (block-print block port))
       x)
      (fprintf port ")"))


    (print-declaration
     (function-name x)
     (function-args x)
     port)
    (print-body x port))


  ;; block

  (define (block-add-succ! x block)
    (assertp block? x)
    (block-succ-set! x (cons block (block-succ x))))

  (define (block-add-statement! x stm)
    (cond
     ((and (null? (block-head x)) (null? (block-tail x)))
      (block-head-set! x stm)
      (block-tail-set! x stm))
     (else
      (instr-next-set! (block-tail x) stm)
      (instr-prev-set! stm (block-tail x))
      (block-tail-set! x stm)))
    x)

  (define (block-print x port)
    (fprintf port "  (~a\n" (block-name x))
    (for-each-statement
     (lambda (instr)
       (instr-print instr port))
     x))

  ;; binops

  (define (binop-left x)
    (assertp instr? x)
    (instr-in1 x))

  (define (binop-left-set! x v)
    (assertp instr? x)
    (instr-in1-set! x v))

  (define (binop-right x)
    (assertp instr? x)
    (instr-in2 x))

  (define (binop-right-set! x v)
    (assertp instr? x)
    (instr-in2-set! x v))

  (define (binop-format node)
    (format "(~a ~a ~a ~a)"
            (instr-op node)
            (instr-mode node)
            (instr-format (binop-left node))
            (instr-format (binop-right node))))


  ;; call

  (define (call-target x)
    (instr-in1 x))

  (define (call-target-set! x v)
    (instr-in1-set! x v))

  (define (call-args x)
    (instr-in2 x))

  (define (call-args-set! x v)
    (instr-in2 x v))

  (define (call-format node)
    (sprintf "(call ~a (~a))"
             (instr-format (call-target node))
             (string-join
              (map (lambda (arg)
                     (instr-format arg))
                   (call-args node))
              " ")))

  ;; ret

  (define (ret-value x)
    (instr-in1 x))

  (define (ret-value-set! x)
    (instr-in1 x))

  (define (ret-format node)
    (format "(ret ~a)"
            (instr-format (ret-value node))))

  ;; load

  (define (load-addr x)
    (instr-in1 x))

  (define (load-addr-set! x v)
    (instr-in1-set! x v))


  (define (load-format node)
    (format "(load ~a ~a)"
            (instr-mode node)
            (instr-format (load-addr node))))

  ;; store

  (define (store-value x)
    (instr-in1 x))

  (define (store-value-set! x v)
    (instr-in1-set! x v))

  (define (store-addr x)
    (instr-in2 x))

  (define (store-addr-set! x v)
    (instr-in2 x v))

  (define (store-format node)
    (format "(store ~a ~a ~a)"
            (instr-mode node)
            (instr-format (store-value node))
            (instr-format (store-addr  node))))

  ;; conditional branch

  (define (brc-cond x)
    (instr-in1 x))

  (define (brc-cond-set! x v)
    (instr-in1-set! x v))

  (define (brc-labelx x)
    (instr-in2 x))

  (define (brc-labelx-set! x v)
    (instr-in2-set! x v))

  (define (brc-labely x)
    (instr-in3 x))

  (define (brc-labely-set! x v)
    (instr-in3-set! x v))

  (define (brc-format node)
    (format "(brc ~a ~a ~a)"
            (instr-format (brc-cond node))
            (instr-format (brc-labelx node))
            (instr-format (brc-labely node))))

  ;; unconditional branch

  (define (br-label x)
    (instr-in1 x))

  (define (br-label-set! x v)
    (instr-in1-set! x v))

  (define (br-format node)
    (format "(br ~a)"
            (instr-format (br-label node))))

  ;; cmp

  (define (cmp-test x)
    (instr-in1 x))

  (define (cmp-test-set! x test)
    (instr-in1-set! x test))

  (define (cmp-x x)
    (instr-in2 x))

  (define (cmp-x-set! x v)
    (instr-in2-set! x v))

  (define (cmp-y x)
    (instr-in3 x))

  (define (cmp-y-set! x v)
    (instr-in3-set! x v))

  (define (cmp-format x)
    (format "(cmp ~a ~a ~a ~a)"
            (instr-mode x)
            (cmp-test x)
            (instr-format (cmp-x x))
            (instr-format (cmp-y x))))

  ;; assign

  (define (assign-name x)
    (instr-in1 x))

  (define (assign-value x)
    (instr-in2 x))

  (define (assign-format x)
    (format "(assign ~a ~a)"
            (assign-name x)
            (instr-format (assign-value x))))

  ;; function traversal

  (define (for-each-function f mod)
    (let ((funcs (module-functions f)))
      (for-each f funcs)))

  ;; block traversal

  (define (for-each-block f fun)
    (define (visit-block block f)
      (let ((succ (block-succ block)))
        (f block)
        (for-each (lambda (succ)
                    (visit-block succ f))
                  succ)))
    (visit-block (function-entry fun) f))

  (define (for-each-block-succ f block)
    (for-each f (block-succ block)))

  (define (for-each-block-pred f block)
    (f (block-pred block)))

  ;; instruction traversal operations

  (define (for-each-statement f block)
    (let ((head (block-head block)))
      (let walk ((x head))
        (cond
         ((not (null? x))
          (f x)
          (walk (instr-next x)))))))

  ;; formatting

  (define (instr-format x)
    (cond
     ;; atoms
     ((constant? x)
      (format "(~a ~a)" (constant-size x) (constant-value x)))
     ((temp? x)
      (format "$~a" (temp-name x)))
     ((label? x)
      (format "@~a" (label-name x)))
     ((instr? x)
      (case (instr-op x)
        ((add sub mul and ior shl shr)
         (binop-format  x))
        ((call)
         (call-format x))
        ((return)
         (ret-format x))
        ((br)
         (br-format x))
        ((brc)
         (brc-format x))
        ((load)
         (load-format x))
        ((store)
         (store-format x))
        ((cmp)
         (cmp-format x))
        ((assign)
         (assign-format x))))
     (else (assert-not-reached))))


  )
