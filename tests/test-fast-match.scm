
(include "fast-match-syntax")

;; rule precedence #1

(define (test-fast-match-1 data) 
  (fast-match data
    (('cmple ('i8 9))
     0)
    (('cmple ('i8 '(x y)))
     1)
    (('cmple (y 1))
     2)
    (('cmple op1)
     3)
    (_ 4)))

(assert 
 (eq? 
  (test-fast-match-1 '(cmple (i8 9)))
  0))

(assert 
 (eq? 
  (test-fast-match-1 '(cmple (i8 (x y))))
  1))

(assert 
 (eq? 
  (test-fast-match-1 '(cmple (i8 1)))
  2))

(assert 
 (eq? 
  (test-fast-match-1 '(cmple (i8 7)))
  3))

(assert 
 (eq? 
  (test-fast-match-1 7)
  4))

;; rule precedence #2

(define (test-fast-match-2 data) 
  (fast-match data
    (('cmple op1)
     0)
    (('cmple ('i8 9))
     1)
    (('cmple ('i8 '(x y)))
     2)
    (('cmple (y 1))
     3)
    (_ 4)))

(assert 
 (eq? 
  (test-fast-match-2 '(cmple (i8 9)))
  0))

(assert 
 (eq? 
  (test-fast-match-2 '(cmple (i8 (x y))))
  0))

(assert 
 (eq? 
  (test-fast-match-2 '(cmple (i8 1)))
  0))

(assert 
 (eq? 
  (test-fast-match-2 '(cmple (i8 7)))
  0))

(assert 
 (eq? 
  (test-fast-match-2 7)
  4))

;; rule precedence #3

(define (test-fast-match-3 data) 
  (fast-match data
    ((x . 3)
     0)
    ((3 . x)
     1)))

(assert 
 (eq? 
  (test-fast-match-3 '(9 . 3))
  0))

(assert 
 (eq? 
  (test-fast-match-3 '(3 . 3))
  0))

(assert 
 (eq? 
  (test-fast-match-3 '(3 . 9))
  1))

;; bindings

(define (test-fast-match-4 data) 
  (fast-match data
    ((x . 3)
     x)
    ((3 . x)
     x)))

(assert 
 (eq? 
  (test-fast-match-4 '(9 . 3))
  9))

(assert 
 (eq? 
  (test-fast-match-4 '(3 . 4))
  4))

;; (as ...) patterns

(define (test-fast-match-5 data) 
  (fast-match data
    ((as x 7)
     x)
    ((as x (1 . y))
     (cons x y))))

(assert 
 (eq? 
  (test-fast-match-5 7)
  7))

(assert 
 (equal? 
  (test-fast-match-5 '(1 . 3))
  '((1 . 3) . 3)))

;; vectors & structures

(define-record point x y)

(define (test-fast-match-6 data) 
  (fast-match data
    (#(1 2 x 3)
     x)
    (($ point (1 x) 3)
     x)))

(assert 
 (eq? 
  (test-fast-match-6 '#(1 2 foo 3))
  'foo))

(assert 
 (eq? 
  (test-fast-match-6 (make-point (list 1 'bar) 3))
  'bar))

