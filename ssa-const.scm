
(declare (unit ssa-const)
         (uses utils))


(define-syntax assertp
  (syntax-rules ()
    ((assertp pred x)
     (assert (pred x) "invalid type"))))

;; constant pool

(define *ssa-i1-pool*  '())
(define *ssa-i8-pool*  '())
(define *ssa-i16-pool* '())
(define *ssa-i32-pool* '())
(define *ssa-i64-pool* '())

;; singleton constructor

(define (ssa-constant-get type value)
  (cond
   ((eq? type <ssa-i1>)
    (cond
     ((assq value *ssa-i1-pool*) => cdr)
     (else
      (let ((const (ssa-make-const type value)))
        (set! *ssa-i1-pool* (cons (cons value const) *ssa-i1-pool*))
        const))))
   ((eq? type <ssa-i8>)
    (cond
     ((assq value *ssa-i8-pool*) => cdr)
     (else
      (let ((const (ssa-make-const type value)))
        (set! *ssa-i8-pool* (cons (cons value const) *ssa-i8-pool*))
        const))))     
   ((eq? type <ssa-i16>)
    (cond
     ((assq value *ssa-i16-pool*) => cdr)
     (else
      (let ((const (ssa-make-const type value)))
        (set! *ssa-i16-pool* (cons (cons value const) *ssa-i16-pool*))
        const))))          
   ((eq? type <ssa-i32>)
    (cond
     ((assq value *ssa-i32-pool*) => cdr)
     (else
      (let ((const (ssa-make-const type value)))
        (set! *ssa-i32-pool* (cons (cons value const) *ssa-i32-pool*))
        const))))          
   ((eq? type <ssa-i64>)
    (cond
     ((assq value *ssa-i64-pool*) => cdr)
     (else
      (let ((const (ssa-make-const type value)))
        (set! *ssa-i64-pool* (cons (cons value const) *ssa-i64-pool*))
        const))))))



;; accessor

(define (ssa-const-value x)
  (assertp ssa-constant? x)
  (ssa-node-in1 x))