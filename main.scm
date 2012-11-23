
(declare (uses pass nodes liveness))

(use matchable)
(use srfi-95)
(use srfi-13)

(include "struct-syntax")

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
            


(define (print-node node)
  (pretty-print (write-sexp node)))

(define pipeline
  (list
    expand
    convert-source
    alpha-convert
    cps-convert
    beta-reduce
    identify-primitives
    raise-lambda
    closure-convert
    flatten
    tree-convert
    select-instructions
    alloc-regs))

(define (compile pipeline source)  
  (let f ((pass pipeline) (input source))
    (if (null? pass)
        input
        (f (cdr pass) ((car pass) input)))))


(define (main argv)
  (cond
    ((= (length argv) 1)
     (fprintf (current-output-port) "Usage: scc FILE\n"))
    (else
      (let* ((test-code (read-file (second argv)))
             (output    (compile pipeline (car test-code))))
        
        (cond
          ((mc-module? output)
           (mc-module-print output (current-output-port)))
          ((tree-module? output)
           (tree-module-print output (current-output-port)))
          (else 
           (pretty-print (write-sexp output))))
       ))))

(main (argv))

