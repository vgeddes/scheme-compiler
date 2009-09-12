
(declare (uses pass class))

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
     ((<null>)
      `())
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
     ((<label> name)
      name)
     ((<code> entry labels)
      `(code ,(map write-sexp labels)))
     ((<fun> label args blocks notes)
      `(function ,label ,args
              ,(map (lambda (block)
                     `(,(car block) ,(map write-sexp (cdr block))))
                    blocks)))
     ;; ((<rtl/add> x y z)
     ;;  `(add ,x ,y ,z))
     ;; ((<rtl/sub> x y z)
     ;;  `(sub ,x ,y ,z))
     ;; ((<rtl/mul> x y z)
     ;;  `(mul ,x ,y ,z))
     ;; ((<rtl/div> x y z)
     ;;  `(div ,x ,y ,z))
     ;; ((<rtl/cmpeq> x y z)
     ;;  `(cmpeq ,x ,y ,z))
     ;; ((<rtl/cmpgt> x y z)
     ;;  `(cmpgt ,x ,y ,z))
     ;; ((<rtl/cmpge> x y z)
     ;;  `(cmpge ,x ,y ,z))
     ;; ((<rtl/cmplt> x y z)
     ;;  `(cmplt ,x ,y ,z))
     ;; ((<rtl/cmple> x y z)
     ;;  `(cmple ,x ,y ,z))
     ;; ((<rtl/if> x y z)
     ;;  `(if ,x ,y ,z))
     ;; ((<rtl/and> x y z)
     ;;  `(and ,x ,y ,z))
     ;; ((<rtl/or> x y z)
     ;;  `(or ,x ,y ,z))
     ;; ((<rtl/shr> x y z)
     ;;  `(shr ,x ,y ,z))
     ;; ((<rtl/shl> x y z)
     ;;  `(shl ,x ,y ,z))
     ;; ((<rtl/ld> m x)
     ;;  `(ld ,m ,x))
     ;; ((<rtl/st> x m)
     ;;  `(st ,x ,m))
     ;; ((<rtl/mov> x y)
     ;;  `(mov ,x ,y))
     ;; ((<rtl/goto> label args)
     ;;  `(goto ,label ,args))
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
    flatten
    ))

(define (compile pipeline source)  
  (let f ((pass pipeline) (input source))
    (if (null? pass)
        input
        (f (cdr pass) ((car pass) input)))))

(print-node (compile pipeline test-code-0))