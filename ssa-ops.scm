(declare (unit ssa-ops)
         (uses ssa))

(use srfi-1)
(use srfi-13)
(use matchable)

(include "struct-syntax")

(define-struct ssa-op (name print foldable constant-fold iterate-uses replace-uses list-uses))

;; folders

(define (ssa-fold-add? x y)
  (and (ssa-constant? x) (ssa-constant? y)))

(define (ssa-fold-add x y)
  (let ((value (+ (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-sub? x y)
  (and (ssa-constant? x) (ssa-constant? y)))

(define (ssa-fold-sub x y)
  (let ((value (- (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-mul? x y)
  (and (ssa-constant? x) (ssa-constant? y)))

(define (ssa-fold-mul x y)
  (let ((value (* (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-and? x y)
  (and (ssa-constant? x) (ssa-constant? y)))

(define (ssa-fold-and x y)
  (let ((value (bitwise-and (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-or? x y)
  (and (ssa-constant? x) (ssa-constant? y)))

(define (ssa-fold-or x y)
  (let ((value (bitwise-ior (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-xor? x y)
  (and (ssa-constant? x) (ssa-constant? y)))

(define (ssa-fold-xor x y)
  (let ((value (bitwise-xor (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-shr? x y)
  (and (ssa-constant? x) (ssa-constant? y)))

(define (ssa-fold-shr x y)
  (let ((value (arithmetic-shift (ssa-const-value x) (ssa-const-value y))))
    (ssa-constant-get (ssa-node-type x) value)))

(define (ssa-fold-shl? x y)
  (and (ssa-constant? x) (ssa-constant? y)))

(define (ssa-fold-shl x y)
  (let ((value (arithmetic-shift (ssa-const-value x) (- 0 (ssa-const-value y)))))
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

;; ptrtoint

(define (ssa-ptrtoint-type x)
  (assertp ssa-ptrtoint? x)
  (ssa-node-type x))

(define (ssa-ptrtoint-value x)
  (assertp ssa-ptrtoint? x)
  (ssa-node-in1 x))

(define (ssa-ptrtoint-value-set! x v)
  (assertp ssa-ptrtoint? x)
  (ssa-node-in1-set! x v))

(define (ssa-ptrtoint-iterate-uses f x)
  (f (ssa-ptrtoint-value x)))

(define (ssa-ptrtoint-replace-uses f x)
  (ssa-ptrtoint-value-set! x (f (ssa-ptrtoint-value x))))

(define (ssa-ptrtoint-list-uses x)
  (list (ssa-ptrtoint-value x)))

(define (ssa-format-ptrtoint node)
  (format "ptrtoint ~a ~a"
          (ssa-format-type  (ssa-node-type node))
          (ssa-format-value (ssa-ptrtoint-value node))))

;; inttoptr

(define (ssa-inttoptr-type x)
  (assertp ssa-inttoptr? x)
  (ssa-node-type x))

(define (ssa-inttoptr-value x)
  (assertp ssa-inttoptr? x)
  (ssa-node-in1 x))

(define (ssa-inttoptr-value-set! x v)
  (assertp ssa-inttoptr? x)
  (ssa-node-in1-set! x v))

(define (ssa-inttoptr-iterate-uses f x)
  (f (ssa-inttoptr-value x)))

(define (ssa-inttoptr-replace-uses f x)
  (ssa-inttoptr-value-set! x (f (ssa-inttoptr-value x))))

(define (ssa-inttoptr-list-uses x)
  (list (ssa-inttoptr-value x)))

(define (ssa-format-inttoptr node)
  (format "inttoptr ~a ~a"
          (ssa-format-type  (ssa-node-type node))
          (ssa-format-value (ssa-inttoptr-value node))))
  
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

(define (ssa-format-elementptr x)
  (format "elementptr ~a ~a, ~a"
          (ssa-format-type (ssa-node-type (ssa-elementptr-ptr x)))
          (ssa-format-value (ssa-elementptr-ptr x))
          (ssa-format-value (ssa-elementptr-offset x))))


;; cmp

(define (ssa-cmp-test x)
  (assertp ssa-cmp? x)
  (ssa-node-in1 x))

(define (ssa-cmp-test-set! x test)
  (assertp ssa-cmp? x)
  (ssa-node-in1-set! x test))

(define (ssa-cmp-x x)
  (assertp ssa-cmp? x)
  (ssa-node-in2 x))

(define (ssa-cmp-x-set! x v)
  (assertp ssa-cmp? x)
  (ssa-node-in2-set! x v))

(define (ssa-cmp-y x)
  (assertp ssa-cmp? x)
  (ssa-node-in3 y))

(define (ssa-cmp-y-set! x v)
  (assertp ssa-cmp? x)
  (ssa-node-in3-set! x v))

(define (ssa-cmp-iterate-uses f x)
  (f (ssa-cmp-x x))
  (f (ssa-cmp-y x)))

(define (ssa-cmp-replace-uses f x)
  (ssa-cmp-x-set! x (f (ssa-cmp-x x)))
  (ssa-cmp-y-set! x (f (ssa-cmp-y x))))

(define (ssa-cmp-list-uses f x)
  (list (ssa-cmp-x x) (ssa-cmp-y x)))

(define (ssa-format-cmp x)
  (format "cmp ~a ~a ~a, ~a"
          (ssa-cmp-test x)
          (ssa-format-type (ssa-node-type x)) 
          (ssa-format-value (ssa-cmp-x x))
          (ssa-format-value (ssa-cmp-y x))))

;; phi

(define (ssa-phi-args x)
  (assertp ssa-phi? x)
  (ssa-node-in1 x))

(define (ssa-phi-args-set! x args)
  (assertp ssa-phi? x)
  (ssa-node-in1-set! x args))

(define (ssa-phi-iterate-uses f x)
  (for-each f (ssa-phi-args x)))

(define (ssa-phi-replace-uses f x)
  (ssa-phi-args-set! x (map f (ssa-phi-args x))))

(define (ssa-phi-list-uses x)
  (ssa-phi-args x))

(define (ssa-format-phi x)
  (format "phi ~a ~a"
          (ssa-format-type (ssa-node-type x)) 
          (string-join (ssa-phi-args x) ", ")))


;; binops

(define <ssa-op-add>
  (make-ssa-op
   'add
   ssa-format-add
   ssa-fold-add?
   ssa-fold-add
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-sub>
  (make-ssa-op
   'sub
   ssa-format-sub
   ssa-fold-sub?
   ssa-fold-sub
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-mul>
  (make-ssa-op
   'mul
   ssa-format-mul
   ssa-fold-mul?
   ssa-fold-mul
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-and>
  (make-ssa-op
   'and
   ssa-format-and
   ssa-fold-and?
   ssa-fold-and
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-or>
  (make-ssa-op
   'or
   ssa-format-or
   ssa-fold-or?
   ssa-fold-or
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-xor>
  (make-ssa-op
   'xor
   ssa-format-xor
   ssa-fold-xor?
   ssa-fold-xor
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-shl>
  (make-ssa-op
   'shl
   ssa-format-shl
   ssa-fold-shl?
   ssa-fold-shl
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

(define <ssa-op-shr>
  (make-ssa-op
   'shr
   ssa-format-shr
   ssa-fold-shr?
   ssa-fold-shr
   ssa-binop-iterate-uses
   ssa-binop-replace-uses
   ssa-binop-list-uses))

;; store

(define <ssa-op-store>
  (make-ssa-op
   'store
   ssa-format-store
   '()
   '()
   ssa-store-iterate-uses
   ssa-store-replace-uses
   ssa-store-list-uses))

;; load

(define <ssa-op-load>
  (make-ssa-op
   'load
   ssa-format-load
   '()
   '()
   ssa-load-iterate-uses
   ssa-load-replace-uses
   ssa-load-list-uses))

;; ret

(define <ssa-op-ret>
  (make-ssa-op
   'ret
   ssa-format-ret
   '()
   '()
   ssa-ret-iterate-uses
   ssa-ret-replace-uses
   ssa-ret-list-uses))

;; call

(define <ssa-op-call>
  (make-ssa-op
   'call
   ssa-format-call
   '()
   '()
   ssa-call-iterate-uses
   ssa-call-replace-uses
   ssa-call-list-uses))

;; br

(define <ssa-op-br>
  (make-ssa-op
   'br
   ssa-format-br
   '()
   '()
   ssa-br-iterate-uses
   ssa-br-replace-uses
   ssa-br-list-uses))

;; brc

(define <ssa-op-brc>
  (make-ssa-op
   'brc
   ssa-format-brc
   '()
   '()
   ssa-brc-iterate-uses
   ssa-brc-replace-uses
   ssa-brc-list-uses))

;; cmp

(define <ssa-op-cmp>
  (make-ssa-op
   'cmp
   ssa-format-cmp
   '()
   '()
   ssa-cmp-iterate-uses
   ssa-cmp-replace-uses
   ssa-cmp-list-uses))

;; phi

(define <ssa-op-phi>
  (make-ssa-op
   'phi
   ssa-format-phi
   '()
   '()
   ssa-phi-iterate-uses
   ssa-phi-replace-uses
   ssa-phi-list-uses))

;; elementptr

(define <ssa-op-elementptr>
  (make-ssa-op
   'elementptr
   ssa-format-elementptr
   '()
   '()
   ssa-elementptr-iterate-uses
   ssa-elementptr-replace-uses
   ssa-elementptr-list-uses))

;; cast

(define <ssa-op-cast>
  (make-ssa-op
   'cast
   ssa-format-cast
   '()
   '()
   ssa-cast-iterate-uses
   ssa-cast-replace-uses
   ssa-cast-list-uses))

;; ptrtoint

(define <ssa-op-ptrtoint>
  (make-ssa-op
   'ptrtoint
   ssa-format-ptrtoint
   '()
   '()
   ssa-ptrtoint-iterate-uses
   ssa-ptrtoint-replace-uses
   ssa-ptrtoint-list-uses))

;; inttoptr

(define <ssa-op-inttoptr>
  (make-ssa-op
   'inttoptr
   ssa-format-inttoptr
   '()
   '()
   ssa-inttoptr-iterate-uses
   ssa-inttoptr-replace-uses
   ssa-inttoptr-list-uses))

;; phi

(define <ssa-op-phi>
  (make-ssa-op
   'phi
   ssa-format-phi
   '()
   '()
   ssa-phi-iterate-uses
   ssa-phi-replace-uses
   ssa-phi-list-uses))

