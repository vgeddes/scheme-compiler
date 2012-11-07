
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
        ((or ('const _ _) ('mode _) ('op _) ('label _) ('temp _))
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

        ;; assign

        (('assign ('temp x) op)
          `($ sl-instr 'assign _ (? symbol? ,x) ,(compile-pattern op) _ _ _ _ _))

        ;; binops
        
        (('add mode op1 op2)
          `($ sl-instr 'add ',(compile-pattern mode) ,(compile-pattern op1) ,(compile-pattern op2) _ _ _ _ _))

        (('sub mode op1 op2)
          `($ sl-instr 'sub ',(compile-pattern mode) ,(compile-pattern op1) ,(compile-pattern op2) _ _ _ _ _))

        (('and mode op1 op2)
          `($ sl-instr 'and ',(compile-pattern mode) ,(compile-pattern op1) ,(compile-pattern op2) _ _ _ _ _))

        (('ior mode op1 op2)
          `($ sl-instr 'ior ',(compile-pattern mode) ,(compile-pattern op1) ,(compile-pattern op2) _ _ _ _ _))

        (('xor mode op1 op2)
          `($ sl-instr 'xor ',(compile-pattern mode) ,(compile-pattern op1) ,(compile-pattern op2) _ _ _ _ _))

        (('shl mode op1 op2)
          `($ sl-instr 'shl ',(compile-pattern mode) ,(compile-pattern op1) ,(compile-pattern op2) _ _ _ _ _))

        (('shr mode op1 op2)
          `($ sl-instr 'shr ',(compile-pattern mode) ,(compile-pattern op1) ,(compile-pattern op2) _ _ _ _ _))

        ;; load

        (('load mode addr)
          `($ sl-instr 'load  ',(compile-pattern mode) ,(compile-pattern addr) _ _ _ _ _ _))


        ;; store
 
        (('store mode value addr)
          `($ sl-instr 'store ',(compile-pattern mode) ,(compile-pattern value) ,(compile-pattern addr) _ _ _ _ _))


        ;; brc

        (('brc cond label1 label2)
          `($ sl-instr 'brc _ ,(compile-pattern cond) ,(compile-pattern label1) ,(compile-pattern label2) _ _ _ _))

        ;; br

        (('br label)
          `($ sl-instr 'br _ ,(compile-pattern label) _ _ _ _ _ _))

        ;; cmp

        (('cmp mode ('op test) op1 op2)
          `($ sl-instr 'cmp ',(compile-pattern mode) ',test ,(compile-pattern op1) ,(compile-pattern op2) _ _ _ _))

        ;; atoms

        (('const size x)
         `($ sl-constant ',size ,x))
        (('label x)
         `($ sl-label ,x))
        (('temp x)
         `($ sl-temp ,x))

        ;; hmm, forgot
        
        ((? symbol? x) (rename x))

        ;; mode

        (('mode x)
         x)

        (_ (print pat))))

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
         `(($ sl-temp ,x) ,x))
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
             (%match      (r 'fast-match))
             (%else       (r 'else))
             (%define     (r 'define)))
 ;;         (pretty-print 
 ;;`(,%define (munch-node node buf)
 ;;           (match node 
 ;;             ,@compiled-rule*
 ;;             (_ (print (sl-instr-format node))))))


         `(,%define (munch-node node buf)
            (match node ,@compiled-rule* (_ (print "error: ") (print (sl-instr-format node))))))))))

