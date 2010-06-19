
(declare (unit ssa-transforms)
         (uses ssa))

;; function traversal

(define (ssa-for-each-function f mod)
  (let ((funcs (ssa-module-functions f)))
    (for-each f funcs)))

;; block traversal

(define (ssa-for-each-block f fun)
  (define (visit-block block f)
    (let ((succ (ssa-block-succ block)))
      (f block)
      (for-each (lambda (succ)
                  (visit-block succ f))
                succ)))
    (visit-block (ssa-function-entry fun) f))

(define (ssa-for-each-block-succ f block)
  (for-each f (ssa-block-succ block)))

(define (ssa-for-each-block-pred f block)
  (f (ssa-block-pred block)))

;; instruction traversal operations

(define (ssa-for-each-instr f block)
  (let ((head (ssa-block-head block)))
    (let walk ((x head))
      (cond
       ((not (null? x)) 
        (f x)
        (walk (ssa-instr-next x)))))))

;; def-use traversal

(define (ssa-add-user! node x)
  (ssa-node-users-set! node (cons x (ssa-node-users node))))

(define (ssa-remove-user! node x)
  (ssa-node-users-set! node (delete eq? x (ssa-node-users node))))

(define (ssa-for-each-user f instr)
  (for-each f (ssa-node-users instr)))

;; deletion

(define (ssa-delete-instr instr)
  (let ((block (ssa-instr-block instr))
        (next  (ssa-instr-next instr))
        (prev  (ssa-instr-prev instr)))
    (cond
     ((and (not (null? prev)) (not (null? next)))
      (ssa-instr-prev-set! next prev)
      (ssa-instr-next-set! prev next))
     ((and (null? prev) (not (null? next)))
      (ssa-instr-prev-set! next '()))
     ((and (null? next) (not (null? prev)))
      (ssa-instr-next-set! prev '())))
    (cond
     ((null? prev)
      (ssa-block-head-set! block next)))
    (cond
     ((null? next)
      (ssa-block-tail-set! block prev)))))

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
   