  
(declare (unit munch)
         (uses arch nodes))

(use matchable)
(use srfi-1)

(include "munch-syntax")
(include "patterns")

;; closures

(define mem
  (lambda operands
    (match operands
      ((base-reg disp)
       (make-x86-memref base-reg disp #f #f))
      ((base-reg disp offset-reg scale)
       (make-x86-memref base-reg disp offset-reg scale)))))

(define (buf-append! buf data)
  (box-set! buf (append (box-ref buf) data)))

(define (munch-rest node buf)
  (match node
    (#('call ('label label) args)
     (buf-append! buf (list (jmp64rel32 label))))
    (#('call (? symbol? var) args)
     (buf-append! buf (list (jmp64r var))))
    (_ (error 'munch "unknown selection node" node))))

(define (munch-expr node buf)
  (munch-node node buf))

