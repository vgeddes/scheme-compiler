
(declare (uses convert class))

(use matchable)
(include "class-syntax")

(define make-sexp
  (lambda (node)
    (match-object node
     ((<if> test conseq altern)
      `(if ,(make-sexp test)
           ,(make-sexp conseq)
           ,(make-sexp altern)))
     ((<lambda> args body free-vars)
      `(lambda ,args ,free-vars ,(make-sexp body)))
     ((<comb> args)
      (map make-sexp args))
     ((<constant> value) value)
     ((<variable> name) name)
     ((<record> values name cexp)
      `(record ,(map make-sexp values) ,(make-sexp name) ,(make-sexp cexp)))
     ((<select> index record name cexp)
      `(select
        ,index
        ,(make-sexp record)
        ,(make-sexp name)
        ,(make-sexp cexp)))
     ((<label> name args body)
      `(label ,name ,args ,(make-sexp body))) 
     (else (error 'make-sexp "not an AST node" node)))))

(define code
  (expand '(let ((fib (lambda (n f)
                        (if (<= n 2)
                            1
                            (+ (f (- n 2) f) (f (- n 1) f))))))
             (fib 23 fib))))

(define ast
  (convert-source code))

(define ast-1
  (cps-convert (alpha-convert ast '())
               (let ((a (gensym)))
                 (make <lambda> (list a) (make <variable> a)))))


(annotate-free-vars ast-1)

(define cc-1 (closure-convert ast-1))

(print (flatten cc-1))

(pretty-print (map make-sexp (flatten cc-1)))

