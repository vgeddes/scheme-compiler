
(import-for-syntax matchable)
(import-for-syntax srfi-1)

;; convenience constructors (used in pattern match rules)

(define-syntax addr
  (lambda (e r c)
    (match e
      (('addr ('base x) ('disp y))
       `(make-machine-addr-x86-64 ,x ,y))
      (('addr ('base x))
       `(make-machine-addr-x86-64 ,x #f))
      (('addr ('disp y))
       `(make-machine-addr-x86-64 #f ,y)))))

(define-syntax imm
  (lambda (e r c)
    (match e
      (('imm size value)
       `(make-machine-imm ',size ,value)))))


(define-syntax vreg
 (lambda (e r c)
    (match e
      (('vreg name)
       `(make-machine-vreg ,name)))))

(define-syntax buf-append
  (syntax-rules ()
   ((append-to-buf buf (production ...))
    (box-set! buf (append (box-ref buf) (list production ...))))))

(define-syntax define-munch-rules
  (lambda (e r c)
    (let ((%let*       (r 'let*))
          (%let        (r 'let))
          (%if         (r 'if))
          (%cond       (r 'cond))
          (%else       (r 'else))
          (%define     (r 'define))
          (%match      (r 'match))
          (%gensym     (r 'gensym))
          (%block      (r 'block))
          (%tree       (r 'tree))
          (%t1         (gensym))
          (%machine-block-append-instr! (r 'machine-block-append-instr!))
          (%make-machine-vreg           (r 'make-machine-vreg)))
 

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
    ;;  
    (define (compile-pattern pat)
      (define (walk pat)
      (match pat

        ;; assign
        (('assign ('temp x) op)
          `($ tree-instr 'assign _ (? symbol? ,x) ,(walk op) _ _ _ _ _))

        ;; binops
        (('add mode op1 op2)
          `($ tree-instr 'add ',(walk mode) ,(walk op1) ,(walk op2) _ _ _ _ _))
        (('sub mode op1 op2)
          `($ tree-instr 'sub ',(walk mode) ,(walk op1) ,(walk op2) _ _ _ _ _))
        (('and mode op1 op2)
          `($ tree-instr 'and ',(walk mode) ,(walk op1) ,(walk op2) _ _ _ _ _))
        (('ior mode op1 op2)
          `($ tree-instr 'ior ',(walk mode) ,(walk op1) ,(walk op2) _ _ _ _ _))
        (('xor mode op1 op2)
          `($ tree-instr 'xor ',(walk mode) ,(walk op1) ,(walk op2) _ _ _ _ _))
        (('shl mode op1 op2)
          `($ tree-instr 'shl ',(walk mode) ,(walk op1) ,(walk op2) _ _ _ _ _))
        (('shr mode op1 op2)
          `($ tree-instr 'shr ',(walk mode) ,(walk op1) ,(walk op2) _ _ _ _ _))

        ;; load
        (('load mode addr)
          `($ tree-instr 'load  ',(walk mode) ,(walk addr) _ _ _ _ _ _))

        ;; store
        (('store mode value addr)
          `($ tree-instr 'store ',(walk mode) ,(walk value) ,(walk addr) _ _ _ _ _))

        ;; brc
        (('brc cond label1 label2)
          `($ tree-instr 'brc _ ,(walk cond) ,(walk label1) ,(walk label2) _ _ _ _))

        ;; br
        (('br label)
          `($ tree-instr 'br _ ,(walk label) _ _ _ _ _ _))

        ;; cmp
        (('cmp mode ('op test) op1 op2)
          `($ tree-instr 'cmp ',(walk mode) ',test ,(walk op1) ,(walk op2) _ _ _ _))

        ;; atoms
        (('const size x)
           `($ tree-constant ',size ,x))
        (('label x)
         `($ tree-label ,x))
        (('temp x)
         `($ tree-temp ,x))

        ((? symbol? x) (rename x))

        ;; mode
        (('mode x) x)

        ;; (_ (print pat))

        ))
    (walk pat))
 
    (define (generate-bindings bindings)
      (match bindings
        (() '())
        ((expr . rest)
         (cons `(,expr (munch-tree ,%block ,(rename expr))) (generate-bindings rest)))))

    (define (parse-temp-cls out)
      (match out
        (('temps t* ...) t*)
        (else (assert-not-reached))))

    (define (parse-out-cls out)
      (match out
        (('out x) x)
        (('out)  #f)
        (else (assert-not-reached))))

    (define (generate pat temps out tmpl*)
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
                   `((,out (,%make-machine-vreg (,%gensym 't)))))
                  (else '()))
               ;; bind temps to unique symbols (remembering not to bind 'out again if it is declared as a temp)
               (map (lambda (temp)
                      `(,temp (,%make-machine-vreg (,%gensym 't))))
                    (lset-difference eq? temps (list out)))))
             (productions (map (lambda (tmpl) 
                                 `(,%machine-block-append-instr! ,%block ,tmpl))
                               tmpl*)))
        `(,pat-compiled
          (,%let* ,bindings
            ,@productions
            ,out))))

    (define (compile rule)
      (match rule
        ((pat temp-cls out-cls (tmpl* ...)) 
         (generate
            pat
            (parse-temp-cls temp-cls)  
            (parse-out-cls  out-cls)
            tmpl*))))
    
    (define (compile-rules rule*)
      (reverse
        (fold (lambda (rule x)
                (cons (compile rule) x))
              '()
              rule*)))

    (match e
      (('define-munch-rules rule* ...)
       (let* ((rule-compiled* (compile-rules rule*)))

 ;;         (pretty-print
  ;;`(,%define (munch-tree ,%block ,%tree)
  ;;           (,%match ,%tree
  ;;              (($ tree-temp ,%t1)
  ;;               (,%make-machine-vreg ,%t1))
  ;;             ,@rule-compiled*)
  ;;            %block))

         `(,%define (munch-tree ,%block ,%tree)
             (,%match ,%tree
               (($ tree-temp ,%t1)
                (,%make-machine-vreg ,%t1))
               ,@rule-compiled*
              (_ (tree-instr-print ,%tree (current-output-port)) (error "no matching pattern"))))))))))

