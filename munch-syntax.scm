
(import-for-syntax matchable)
(import-for-syntax srfi-1)

(define-syntax buf-append
  (syntax-rules ()
   ((append-to-buf buf (production ...))
    (box-set! buf (append (box-ref buf) (list production ...))))))

(define-syntax define-munch-rules
  (lambda (e r c)
    
    (define renamed '())
    (define (rename name)
      (cond
       ((assq name renamed)
        => cdr)
       (else
        (let ((x (gensym)))
          (set! renamed (cons (cons name x) renamed))
          x))))
  
    ;; select-names
    ;;
    ;; Find names (which are bound to nodes by 'match) which need to be expanded next by the maximal-munch algorithm 
    ;;
    ;; (add (i32 x) op2)
    ;; => (op2)

    (define (select-names pat)
      (match pat
        ((? symbol? x)
         (list x))
        ((or ('i8 x) ('i32 x) ('i64 x) ('label x) ('temp x))
         '())
        ((opcode operand* ...)
         (apply append (map select-names operand*)))))

    ;; compile-pattern
    ;;
    ;; Transform high-level patterns into low-level 'match patterns
    ;; 
    ;;  (add (i32 x) op2)
    ;;  => ($ selection-node 'add (? i32? x) (? symbol? g67))

    (define (compile-pattern pat)
      (match pat
        (('i8 x)
         `(? i8? ,x))
        (('i32 x)
         `(? integer? ,x))
        (('label x)
         `('label ,x))
        (('temp x)
         `(? symbol? ,x))
        ((? symbol? x) (rename x))
        ((opcode operand* ...)
         `($ selection-node ',opcode ,(map compile-pattern operand*)))))

    (define (generate-bindings bindings)
      (match bindings
        (() '())
        ((expr . rest)
         (cons `(,expr (munch-node ,(rename expr) buf)) (generate-bindings rest)))))

    (define (parse-temp-cls out)
      (match out
        (('temps t* ...) t*)
        (else (error 'parse-temp-cls))))

    (define (parse-out-cls out)
      (match out
        (('out x) x)
        (('out)  #f)
        (else (error 'parse-out-cls))))

    (define (parse-tmpl out)
      (match out
        ((instr* ...) instr*)
        (else (error 'parse-tmpl))))

    (define (generate-rule pat temps out tmpl)
      (let* ((nodes-to-expand (select-names    pat))
             (pat-compiled    (compile-pattern pat))
             (bindings
              (append
               ;; Bind names to expanded nodes
               (generate-bindings nodes-to-expand)
               (cond
                ;; bind the name 'out' to a gensym if this production requires a return value (in which case out != #f)
                ;; AND the user-specified return value is not already listed in nodes-to-expand. 
                ((and out (not (memq out nodes-to-expand)))
                 `((,out (gensym 't))))
                (else '()))
               ;; bind temps to unique symbols (remembering not to bind 'out again if it is declared as a temp)
               (map (lambda (temp) `(,temp (gensym 't))) (lset-difference eq? temps (list out)))))
             (%let* (r 'let*)))
        `(,pat-compiled
          (,%let* ,bindings
            (buf-append buf ,tmpl)
            ,out))))

    (define (compile-rule rule)
      (match rule
        ((('temp x) x)
         `((? symbol? ,x) ,x))
        ((pat temp-cls out-cls tmpl) 
         (generate-rule
            pat
            (parse-temp-cls temp-cls)  
            (parse-out-cls  out-cls)
            (parse-tmpl     tmpl)))))
    
    (match e
      (('define-munch-rules rule* ...)
       (let ((compiled-rule* (reverse
                               (fold (lambda (rule x)
                                       (cons (compile-rule rule) x))
                                     '()
                                   rule*)))
             (%match      (r 'match))
             (%else       (r 'else))
             (%define     (r 'define)))
         `(,%define (munch-node node buf)
            (,%match node
              ,@compiled-rule*
              (,%else (error 'munch-node "could not match pattern" (selection-node-opcode node))))))))))
