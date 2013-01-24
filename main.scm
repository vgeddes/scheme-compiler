(declare (uses pass machine tree globals arch arch-x86-64 liveness))

(import pass)
(import machine (prefix machine mc-))
(import tree    (prefix tree tree-))
(import liveness)
(import globals)
(import arch-x86-64)
(import arch)

(use extras)
(use matchable)
(use srfi-95)
(use srfi-13)

(set! *arch* <arch-x86-64>)

(define (print-node node)
  (pretty-print (write-sexp node)))

(define pipeline
  (list
    normalize
    alpha-convert
    hil-convert
    cps-convert
    closure-convert
    tree-convert
    select-instructions
    alloc-regs
   ))

(define (compile pipeline source)
  (let f ((pass pipeline) (input source))
    (if (null? pass)
        input
        (f (cdr pass) ((car pass) input)))))


(define (main argv)
  (arch-init)
  (cond
    ((= (length argv) 1)
     (fprintf (current-output-port) "Usage: scc FILE\n"))
    (else
      (let* ((test-code (read-file (second argv)))
             (output    (compile pipeline (car test-code))))

        (cond
          ((hil-mod? output)
           (pretty-print (hil-format-sexp output)))
          ((list? output)
           (pretty-print output))
          ((mc-mod? output)
           (mc-mod-print output (current-output-port)))
          ((tree-module? output)
           (tree-module-print output (current-output-port)))
          (else
           (pretty-print (build-sexp output))))
       ))))

(main (argv))
