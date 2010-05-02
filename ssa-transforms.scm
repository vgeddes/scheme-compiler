
(declare (unit ssa-transforms)
         (uses ssa))

;; function traversal

(define (ssa-for-each-function f mod)
  (let ((funcs (ssa-module-functions f)))
    (for-each f funcs)))

;; block traversal

(define (ssa-for-each-block f fun)
  (let ((entry ((ssa-function-entry fun))))
    (let walk ((x entry))
      (begin
        (f x)
        (ssa-for-each-block-succ walk x)))))

(define (ssa-for-each-block-succ f block)
  (for-each f (ssa-block-succ block)))

(define (ssa-for-each-block-pred f block)
  (f (ssa-block-pred block)))


;; instruction traversal operations

(define (ssa-fold-instr f nil block)
  (let ((head ((ssa-block-head block))))
    (let walk ((x head) (nil nil))
      (cond
       ((null? x) nil)
       (else (walk (ssa-instr-next x) (f x nil)))))))

(define (ssa-for-each-instr f block)
  (ssa-fold-instr (lambda (instr nil)
                    (f instr))
                  '()
                  block))

;; def-use traversal

(define (ssa-add-use! node x)
  (ssa-node-uses-set! node (cons x (ssa-node-uses node))))

(define (ssa-remove-use! node x)
  (ssa-node-uses-set! node (delete eq? x (ssa-node-uses node))))

(define (ssa-for-each-use f instr)
  (for-each f (ssa-node-uses instr)))

(define (ssa-for-each-def f user)
  (vector-for-each f (ssa-node-in user)))

;; deletion

(define (ssa-delete-instr instr)
  (let ((block (ssa-instr-block instr))
        (next  (ssa-instr-next instr))
        (prev  (ssa-instr-prev instr)))
    (cond
     ((not (null? next))
      (ssa-instr-next-set! prev next))
    (cond
     ((not (null? prev))
      (ssa-instr-prev-set! next prev)))
    (cond
     ((null? prev)
      (ssa-block-head-set! next)))
    (cond
     ((null? next)
      (ssa-block-tail-set! prev))))))

;; Replaces all uses of `value` with `x`. 
(define (ssa-replace-all-uses-with! value x)
  (ssa-for-each-use
   (lambda (user)
     (ssa-node-in-set!
      (vector-map (lambda (x)
                    (cond
                     ((eq? x from)
                      (ssa-add-use! to user)
                      to)
                     (else from)))
                  (ssa-node-in user))))
   value)
  (ssa-node-uses-set! value '()))
  
;; Replaces all uses of `value` in `user` with `x`. 
(define (ssa-replace-uses-of! user from to)
  (ssa-node-in-set!
   (vector-map (lambda (x)
                 (cond
                  ((eq? x from)
                   (ssa-remove-use! from user)
                   (ssa-add-use! to user)
                   to)
                  (else from)))
               (ssa-node-in user))))   

;; Replaces `instr` with `x`
(define (ssa-replace-instr! instr x)
  (ssa-replace-all-uses-with! instr value)
  (ssa-delete-instr! instr))

