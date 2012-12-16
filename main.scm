
(declare (uses pass nodes liveness))

(use matchable)
(use srfi-95)
(use srfi-13)

(include "struct-syntax")

(define (print-node node)
  (pretty-print (write-sexp node)))

(define pipeline
  (list
    normalize
    hil-convert
    alpha-convert
    cps-convert
    closure-convert
    flatten
    tree-convert
    select-instructions
    alloc-regs))

(define pipeline
  (list
    normalize
    alpha-convert
    hil-convert
    cps-convert
    closure-convert
    tree-convert
   ))

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
          ((hil-mod? output)
           (pretty-print (build-sexp output)))
          ((list? output)
           (pretty-print output))
          ((mc-module? output)
           (mc-module-print output (current-output-port)))
          ((tree-module? output)
           (tree-module-print output (current-output-port)))
          (else
           (pretty-print (build-sexp output))))
       ))))

(main (argv))
