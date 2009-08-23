
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
     ((<lambda> name args body free-vars)
      `(lambda ,name ,args ,free-vars ,(make-sexp body)))
     ((<fix> defs body)
      `(fix ,(map make-sexp defs) ,(make-sexp body)))
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

(define code-1
  (expand '(let ((fib (lambda (n f)
                        (if (<= n 2)
                            1
                            (+ (f (- n 2) f) (f (- n 1) f))))))
             (fib 23 fib))))


(define code-2
  (convert-source code-1))

(define code-3
  (make-normal-lambda code-2))

(define code-4
  (alpha-convert code-3 (list)))

(pretty-print (make-sexp code-4))

(print)

(define code-5
  (let ((cn (gensym 'c))
        (tn (gensym 't)))
    (make <fix>
      (list (make <lambda> cn (list tn) (make <variable> tn)))
      (cps-convert code-4 (make <variable> cn)))))

(pretty-print (make-sexp code-5))

(print)
   
(annotate-free-vars code-5)

(define code-6 (closure-convert code-5))

(pretty-print (make-sexp code-6))

;; (print (flatten code-5))

;; (pretty-print (map make-sexp (flatten code-5)))

