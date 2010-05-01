
(declare (unit ssa-types)
         (uses utils))

(define-struct ssa-type (code width points-to-type return-type param-types arg-count))

(define *ssa-type-codes*
  '(void label integer pointer array function))

;; raw type constructors

(define (ssa-make-type-void)
  (make-ssa-type 'void '() '() '() '() '()))

(define (ssa-make-type-label)
  (make-ssa-type 'label '() '() '() '() '()))

(define (ssa-make-type-integer width)
  (make-ssa-type 'integer width '() '() '() '()))

(define (ssa-make-type-pointer points-to-type)
  (make-ssa-type 'pointer '() points-to-type '() '() '()))

(define (ssa-make-type-function return-type param-types arg-count)
  (make-ssa-type 'function '() '() return-type param-types arg-count))

;; singleton constructors

;; core types

(define <ssa-void>     (ssa-make-type-void))
(define <ssa-i1>       (ssa-make-type-integer 1))
(define <ssa-i8>       (ssa-make-type-integer 8))
(define <ssa-i16>      (ssa-make-type-integer 16))
(define <ssa-i32>      (ssa-make-type-integer 32))
(define <ssa-i64>      (ssa-make-type-integer 64))
(define <ssa-ptr-i32>  (ssa-make-type-pointer <ssa-i32>))
(define <ssa-ptr-i64>  (ssa-make-type-pointer <ssa-i64>))
(define <ssa-label>    (ssa-make-type-label))

(define (ssa-type-void-get) <ssa-void>)

(define (ssa-type-label-get) <ssa-label>)

(define (ssa-type-integer-get width)
  (case width
    ((1)  <ssa-i1>)
    ((8)  <ssa-i8>)
    ((16) <ssa-i16>)
    ((32) <ssa-i32>)
    ((64) <ssa-i64>)
    (else (error 'ssa-type-integer-get "unsupported integer width" width))))

(define *ssa-pointers* '())

(define (ssa-type-pointer-get points-to-type)
  (cond
   ((assq points-to-type *ssa-pointers*)
    => cdr)
   (else
    (let ((type (ssa-make-type-pointer points-to-type)))
      (set! *ssa-pointers* (cons (cons points-to-type type) *ssa-pointers*))
      type))))

(define *ssa-functions* '())

(define (ssa-type-function-get return-type param-types arg-count)
  (cond
   ((find (lambda (cell)
            (match (car cell)
              ((rt pt count)
               (and (eq?       rt    return-type)
                    (lset= eq? pt    param-types)
                    (eq?       count arg-count))))) 
          *ssa-functions*)
    => cdr)
   (else
    (let ((type (ssa-make-type-function return-type param-types arg-count)))
      (set! *ssa-functions* (cons (list return-type param-types arg-count) *ssa-functions*))
      type))))

;; type accessors

(define (ssa-type-integer-width x)
  (ssa-type-width x))

(define (ssa-type-pointer-points-to-type x)
  (ssa-type-points-to-type x))

;; type predicates

(define (ssa-type-code? x code)
  (eq? (ssa-type-code x) code))

(define (ssa-type-integer? x)
  (ssa-type-code? x 'integer))

(define (ssa-type-pointer? x)
  (ssa-type-code? x 'pointer))

(define (ssa-type-void? x)
  (ssa-type-code? x 'void))
