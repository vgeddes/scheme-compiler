
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
    ssa-convert))
   ;; select-instructions
   ;; allocate-registers))

(define (compile pipeline source)  
  (let f ((pass pipeline) (input source))
    (if (null? pass)
        input
        (f (cdr pass) ((car pass) input)))))

(compile pipeline test-code-0)


(define-struct option-spec (short long arg msg action))


(define (parse-option-specs specs)
  (map (lambda (spec)
         (apply make-option-spec spec))
       specs))
 
(define (print-help usage description specs)

  (define *padding* 2)
  
  (define (format-option-arg spec)
    (let* ((tmp
            (sprintf "-~a  -~a"
                    (option-spec-short spec)
                    (option-spec-long spec))))
      (if (option-spec-arg spec)
          (string-append tmp " " (symbol->string (option-spec-arg spec)))
          tmp)))

  (define (determine-col-size specs)
    (if (null? specs)
        0
        (let ((seq
               (sort
                (list->vector
                 (map (lambda (spec)
                        (string-length (format-option-arg spec)))
                      specs))
                <)))
          (vector-ref seq (- (vector-length seq) 1)))))

  (define (ws width)
    (make-string width #\space))
  
  (define (format-option spec left-margin col-width)
    (string-append
     (ws left-margin)
     (string-pad-right (format-option-arg spec) col-width #\space)
     "  "
     (option-spec-msg spec)))

  (define (format-options specs)
    (let* ((col-width (determine-col-size specs)))
      (string-append
       (ws 2) "Options:\n\n"
       (string-join
        (map (lambda (spec)
               (format-option spec 4 col-width))
             specs)
        "\n"))))

  (define (format-usage usage-str)
    (string-append "Usage: " usage-str))

  (define (format-descr descr-str)
    (string-append (ws 2) descr-str))
  
  (define (format-help usage description specs)
    (string-append
     (format-usage usage)
     "\n\n"
     (format-descr description)
     "\n\n"
     (format-options specs) "\n"))
  
  (print (format-help usage description specs)))
  
  
(define (matches-spec? arg spec)
  (cond
   ((and (option-spec-short spec) (option-spec-long spec))
    (or (string=? arg (make-string 1 (option-spec-short spec)))
        (string=? arg (symbol->string (option-spec-long spec)))))
   ((option-spec-short spec)
    (string=? arg (make-string 1 (option-spec-short spec))))
   ((option-spec-long spec)
    (string=? arg (symbol->string (option-spec-long spec))))
   (else (assert-not-reached))))
   
(define (fold-args args specs)
  (let f ((args args) (positionals '()))
    (match args
      (() (reverse positionals))
      ((a . a*)
       (let g ((specs specs))
         (match specs
           (() '())
           ((s . s*)
            (cond
             ((string-prefix? "-" a)
              (cond
               ((matches-spec? (substring a 1) s)
                (if (option-spec-arg s)
                    (cond
                     ((and (not (null? a*))
                           (not (string-prefix? "-" (car a*))))
                      ((option-spec-action s) (car a*)))
                     ((or (null? a*) (string-prefix? "-" (car a*)))
                      (option-error "option requires an argument"))
                     (else (assert-not-reached)))
                    ((option-spec-action s)))
                (f a* positionals))
               (else (g (cdr specs)))))
             (else 
              (f a* (cons a positionals)))))))))))

(define *options*
  (list 
   (make-option-spec
    #\o 'output-file 'FILENAME "Output file for compiled binary"
    (lambda (v)
      (param-input-file v)))
   (make-option-spec
    #\V 'version     #f       "Show version information"
    (lambda ()
      (print "scc 0.1.1")))
   (make-option-spec
    #\h 'help        #f       "Show this message"
    (lambda ()
      (print-help
       "scc [OPTION ...] FILE"
        "scc is yet another scheme compiler."
        *options*)))
   (make-option-spec
    #\v 'verbose     #f       "Show verbose messages"
    (lambda ()
      '()))))

(define (main args)
  (fold-args args *options*))
       
(main (argv))

