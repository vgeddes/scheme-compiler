
(declare (uses pass nodes liveness))

(use matchable)

(include "struct-syntax")

;; (define (print-block block)
;;   (reverse
;;    (fold-node
;;     (lambda (x node)
;;       (let ((name (node-name node))
;;             (op   (node-op node))
;;             (args (map (lambda (arg)
;;                          (if (node? arg)
;;                              (node-name arg)
;;                              arg))
;;                        (node-args node))))
;;         (cons
;;          (if (null? name)
;;              `(,op ,@args)
;;              `(,name (,op ,@args)))
;;          x)))
;;     '()
;;     block)))

(define write-sexp
  (lambda (node)
    (struct-case node
     ((if test conseq altern)
      `(if ,(write-sexp test)
           ,(write-sexp conseq)
           ,(write-sexp altern)))
     ((lambda name args body free-vars)
      `(lambda ,name ,args ,free-vars ,(write-sexp body)))
     ((fix defs body)
      `(fix ,(map write-sexp defs) ,(write-sexp body)))
     ((comb args)
      (map write-sexp args))
     ((app name args)
      `(app ,(write-sexp name) ,(map write-sexp args)))
     ((prim name args result cexp)
      `(prim ,(write-sexp name) ,(map write-sexp args) ,(write-sexp result) ,(write-sexp cexp)))
     ((nil)
      `())
     ((constant value) value)
     ((variable name) name)
     ((record values name cexp)
      `(record ,(map write-sexp values) ,(write-sexp name) ,(write-sexp cexp)))
     ((select index record name cexp)
      `(select
        ,index
        ,(write-sexp record)
        ,(write-sexp name)
        ,(write-sexp cexp)))
     ((label name)
      (string->symbol (format "$~s" name)))
     ((module contexts)
      `(module ,(map write-sexp contexts)))
     ((context formals start-block blocks)
      `(context ,formals ,start-block
                ,blocks))
     ((block label code)
      `(block ,label ,code))
     (else (error 'write-sexp "not an AST node" node)))))


 
(define test-code-0
  '(let ((fib (lambda (n f)
                (if (fx<= n 2)
                    1
                    (fx+ (f (fx- n 2) f) (f (fx- n 1) f))))))
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
    rtl-convert
    select-instructions
    allocate-registers))

(define (compile pipeline source)  
  (let f ((pass pipeline) (input source))
    (if (null? pass)
        input
        (f (cdr pass) ((car pass) input)))))

(compile pipeline test-code-0)