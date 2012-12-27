
(import-for-syntax matchable)
(import-for-syntax srfi-1)

;; convenience constructors (used in pattern match rules)

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
          (%t1         (gensym 't))
          (%mblk-append           (r 'mblk-append))
          (%mcxt-alloc-vreg  (r 'mcxt-alloc-vreg))
          (%mblk-cxt              (r 'mblk-cxt)))


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
    ;;  => ($ tree-instr 'add ('mode 'i32) (? i32? x) (? symbol? g67))
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

         ;;(_ (print pat))

        ))
    (walk pat))

    (define (gen-bindings arch bindings)
      (let ((function-name  (string->symbol (format "munch-~s" arch))))
        (match bindings
          (() '())
          ((expr . rest)
           (cons `(,expr (,function-name ,%block ,(rename expr))) (gen-bindings arch rest))))))

    (define (parse-temp-cls out)
      (match out
        (('temps t* ...) t*)
        (else (assert-not-reached))))

    (define (parse-out-cls out)
      (match out
        (('out x) x)
        (('out)  #f)
        (else (assert-not-reached))))

    (define (gen-code arch pat temps out tmpl*)
      (let* ((nodes-to-expand (select-names    pat))
             (pat-compiled    (compile-pattern pat))
             (bindings
              (append
               ;; Bind names to expanded nodes
                (gen-bindings arch nodes-to-expand)
                (cond
                  ;; bind the name 'out' to a gensym if this production requires a return value (in which case out != #f)
                  ;; AND the user-specified return value is not already listed in nodes-to-expand.
                  ((and out (not (memq out nodes-to-expand)))
                   `((,out (,%mcxt-alloc-vreg (,%mblk-cxt ,%block) (,%gensym 't)))))
                  (else '()))
               ;; bind temps to unique symbols (remembering not to bind 'out again if it is declared as a temp)
               (map (lambda (temp)
                      `(,temp (,%mcxt-alloc-vreg (,%mblk-cxt ,%block) (,%gensym 't))))
                    (lset-difference eq? temps (list out))))))

        `(,pat-compiled
          (,%let* ,bindings
            (arch-emit-code ,arch ,%block ,@tmpl*)
            ,out))))

    (define (compile arch rule)
      (match rule
        ((pat temp-cls out-cls (tmpl* ...))
         (gen-code
            arch
            pat
            (parse-temp-cls temp-cls)
            (parse-out-cls  out-cls)
            tmpl*))))

    (define (compile-rules arch rule*)
      (reverse
        (fold (lambda (rule x)
                (cons (compile arch rule) x))
              '()
              rule*)))

    (match e
      (('define-munch-rules arch rule* ...)
       (let* ((rule-compiled* (compile-rules arch rule*))
              (function-name  (string->symbol (format "munch-~s" arch))))

;;         (pretty-print
;;  `(,%define (,function-name ,%block ,%tree)
;;             (,%match ,%tree
;;               (($ tree-temp ,%t1)
;;                (,%mcxt-allocate-vreg (,%mblk-cxt ,%block) ,%t1))
;;               ,@rule-compiled*
;;              (_ (tree-instr-print ,%tree (current-output-port)) (error "no matching pattern")))))

         `(,%define (,function-name ,%block ,%tree)
             (,%match ,%tree
               (($ tree-temp ,%t1)
                (,%mcxt-alloc-vreg (,%mblk-cxt ,%block) ,%t1))
               ,@rule-compiled*
              (_ (tree-instr-print ,%tree (current-output-port)) (error "no matching pattern"))))))))))
