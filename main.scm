
(declare (uses pass nodes liveness))

(use matchable)
(use srfi-95)
(use srfi-13)

(include "struct-syntax")
;;(include "option-syntax")

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


(define (print-asm-block block port)
  (match block
    (('block name succ code)
     ;; print label
     (fprintf port "~a:\n" name)
     ;; print asm
     (let f ((code code))
       (match code
         (() '())
         ((c . c*)
          (fprintf port "  ~a\t\t\t# live=~a\n" (format-instr c) (instr-data c))
          (f c*))))
     (fprintf port "\n")
     ;; walk all successor blocks
     (for-each (lambda (succ)
                 (print-asm-block succ port))
       succ))))

(define (print-asm-context func port)
  (match func
    (('context name args entry)
     (fprintf port "# context name=~a args=~a\n" name args)
     (print-asm-block entry port))))

(define (print-asm mod port)
  (match mod
    (('module contexts)
     (let k ((c* contexts))
        (match c*
          (() '())
          ((c . c*)
           (print-asm-context c port)
           (k c*)))))))
            
(define test-code-0
  '(let ((fib (lambda (n f)
                (if (fx<= n 2)
                    1
                    (fx+ (f (fx- n 2) f) (f (fx- n 1) f))))))
     (fib 23 fib)))

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
    selection-convert
    select-instructions
    analyze-liveness-pass))

(define (compile pipeline source)  
  (let f ((pass pipeline) (input source))
    (if (null? pass)
        input
        (f (cdr pass) ((car pass) input)))))

(print-asm (compile pipeline test-code-0) (current-output-port))



;;(pretty-print (compile pipeline test-code-0))

;;(define-option-context *options*
  ;;
  ;;((-o -output-file FILENAME)
  ;; "Output file for compiled binary"
  ;; (lambda (v)
  ;;   (param-input-file v)))
  
  ;;((-V -version)
  ;; "Show version information"
  ;; (lambda ()
  ;;   (print "scc 0.1.1")))
  
  ;;((-h -help)
  ;; "Show this message"
  ;; (lambda ()
  ;;   (print-help
  ;;    "scc [OPTION ...] FILE"
  ;;    "scc is yet another scheme compiler."
  ;;    *options*)))
  
  ;;((-v -verbose)
  ;; "Show verbose messages"
  ;; (lambda ()
  ;;   '())))

;;(define (main args)
;;  (parse-args args *options*))
       
;;(main (argv))

