
(declare (unit utils))

;; boxed type - call by reference

(define (box v)
  (list v))

(define (box-set! box v)
  (set-car! box v))

(define (box-ref box)
  (car box))

;; key-value table 
 
(define (make-table)
  (box '()))

(define (table-get table key)
  (cond
   ((assq key (box-ref table)) => cdr)
   (else '())))

(define (table-set table key value)
  (box-set! table
    (cons (cons key value)
          (box-ref table))))

;; A generator function which produces integers which increase by 1 every time.

(define (make-count-generator)
  (let ((k 0))
    (lambda ()
      (let ((ret k))
        (set! k (+ k 1))
          ret))))

;; size tests  for immediates

(define (width x)
  (cond
   ;; convert a negative (and hence known to be two's-complement form) to any unsigned integer that requires the same number of bits 
   ((< x 0)
    (width (+ (* 2 (abs x)) 1)))
   ((<= x #xFF) 8)
   ((<= x #xFFFF) 16)
   ((<= x #xFFFFFFFF) 32)
   (else 64)))

(define (i8? x)
  (and (integer? x)
       (= (width x) 8)))

(define (i16? x)
  (and (integer? x)
       (= (width x) 16)))

(define (i32? x)
  (and (integer? x)
       (= (width x) 32)))

(define (i64? x)
  (and (integer? x)
       (= (width x) 64)))