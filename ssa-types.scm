
(declare (unit ssa-types)
         (uses utils))

(use matchable)
(use srfi-69)

(include "struct-syntax")

(define-struct ssa-type (code width points-to-type return-type param-types))

(define *ssa-type-codes*
  '(void label integer pointer function))

;; raw type constructors

(define (ssa-make-type-void)
  (make-ssa-type 'void '() '() '() '()))

(define (ssa-make-type-label)
  (make-ssa-type 'label '() '() '() '()))

(define (ssa-make-type-integer width)
  (make-ssa-type 'integer width '() '() '()))

(define (ssa-make-type-pointer points-to-type)
  (make-ssa-type 'pointer '() points-to-type '() '()))

(define (ssa-make-type-function return-type param-types)
  (make-ssa-type 'function '() '() return-type param-types))

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

;; 0800 99 01 23 / 800 654 25 93

(define (ssa-type-pointer-get points-to-type)
  (cond
   ((assq points-to-type *ssa-pointers*)
    => cdr)
   (else
    (let ((type (ssa-make-type-pointer points-to-type)))
      (set! *ssa-pointers* (cons (cons points-to-type type) *ssa-pointers*))
      type))))

(define *ssa-functions* '())
  
(define (ssa-type-function-get return-type param-types)
  (define (func-equal? record)
    (match record
      (((rt pt) . _)
       (and (eq?       rt return-type)
            (lset= eq? pt param-types)
            (= (length pt) (length param-types)))))) 
  (define (add-record! rt pt type)
    (let ((record (cons (list return-type param-types) type)))
      (set! *ssa-functions* (cons record *ssa-functions*))
      type))
  (cond
   ((find func-equal? *ssa-functions*)
    => cdr)
   (else
    (let ((type (ssa-make-type-function return-type param-types)))
      (add-record! return-type param-types type)))))


                                              
;; type accessors

(define (ssa-type-integer-width x)
  (ssa-type-width x))

(define (ssa-type-pointer-points-to-type x)
  (ssa-type-points-to-type x))

(define (ssa-type-function-return-type x)
  (ssa-type-return-type x))

(define (ssa-type-function-param-types x)
  (ssa-type-param-types x))

(define (ssa-type-function-pointer-return-type x)
  (ssa-type-function-return-type
   (ssa-type-pointer-points-to-type x)))

(define (ssa-type-function-pointer-param-types x)
  (ssa-type-function-param-types
   (ssa-type-pointer-points-to-type x)))
  
;; type predicates

(define (ssa-type-code? x code)
  (eq? (ssa-type-code x) code))

(define (ssa-type-integer? x)
  (ssa-type-code? x 'integer))

(define (ssa-type-pointer? x)
  (ssa-type-code? x 'pointer))

(define (ssa-type-void? x)
  (ssa-type-code? x 'void))

(define (ssa-type-label? x)
  (ssa-type-code? x 'label))

(define (ssa-type-function? x)
  (ssa-type-code? x 'function))

(define (ssa-type-function-pointer? x)
  (and (ssa-type-pointer? x)
       (ssa-type-function? (ssa-type-pointer-points-to-type x))))


;; formatting

(define (ssa-format-type x)
  (cond
   ((ssa-type-void? x) "void")
   ((ssa-type-integer? x)
    (case (ssa-type-integer-width x)
      ((1)   "i1")
      ((8)   "i8")
      ((16) "i16")
      ((32) "i32")
      ((64) "i64")))
   ((ssa-type-label? x) "label")
   ((ssa-type-function? x)
    (sprintf "~a (~a)"
             (ssa-format-type (ssa-type-function-return-type x))
             (string-join
              (map (lambda (type)
                     (ssa-format-type type))
                   (ssa-type-function-param-types x))
              ", ")))
   ((ssa-type-pointer? x)
    (format "~a*" (ssa-format-type (ssa-type-pointer-points-to-type x))))
   (else (assert-not-reached 'ssa-format-type x))))
      
      