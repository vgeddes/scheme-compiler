
(declare (uses convert class))

(use matchable)
(include "class-syntax")


(define write-sexp
  (lambda (node)
    (match-object node
     ((<if> test conseq altern)
      `(if ,(write-sexp test)
           ,(write-sexp conseq)
           ,(write-sexp altern)))
     ((<lambda> name args body free-vars)
      `(lambda ,name ,args ,free-vars ,(write-sexp body)))
     ((<fix> defs body)
      `(fix ,(map write-sexp defs) ,(write-sexp body)))
     ((<comb> args)
      (map write-sexp args))
     ((<app> name args)
      `(app ,(write-sexp name) ,(map write-sexp args)))
     ((<prim> name args result cexp)
      `(prim ,(write-sexp name) ,(map write-sexp args) ,(write-sexp result) ,(write-sexp cexp)))
     ((<constant> value) value)
     ((<variable> name) name)
     ((<record> values name cexp)
      `(record ,(map write-sexp values) ,(write-sexp name) ,(write-sexp cexp)))
     ((<select> index record name cexp)
      `(select
        ,index
        ,(write-sexp record)
        ,(write-sexp name)
        ,(write-sexp cexp)))
     ((<label> name args body)
      `(label ,name ,args ,(write-sexp body))) 
     (else (error 'write-sexp "not an AST node" node)))))

(define test-code-0
  '(let ((fib (lambda (n f)
                (if (<= n 2)
                    1
                    (+ (f (- n 2) f) (f (- n 1) f))))))
     (fib 23 fib)))

(define test-code-1
  '(f (g (h (i))) (j 1) (k 2)))

(define (print-node node)
  (pretty-print (write-sexp node)))

(define pipeline
  (list
    expand
    convert-source
    alpha-convert
    cps-convert
    identify-primitives
    basic-lambda-lift
    closure-convert
    flatten))

(define (compile source)  
  (let f ((pass pipeline) (input source))
    (if (null? pass)
        input
        (f (cdr pass) ((car pass) input)))))



(print-node (compile test-code-0))