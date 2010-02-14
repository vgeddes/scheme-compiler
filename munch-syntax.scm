
(import-for-syntax matchable)
(import-for-syntax srfi-1)

(define-syntax munch
  (lambda (e r c)
    (define (rewrite-pattern pat)
      (match pat
        (('i8 x)
         `(? i8? ,x))
        (('i32 x)
         `(? integer? ,x))
        (('L x)
         `('L ,x))
        (('T x)
         `(? symbol? ,x))
        ((? symbol? x) x)
        ((op e* ...)
         `#(',op ,@(map rewrite-pattern e*)))))
    (define (generate-bindings bindings buf)
      (match bindings
        (() '())
        ((expr . rest)
         (cons `(,expr (munch-expr ,expr ,buf)) (generate-bindings rest buf)))))    
    (define (generate-rule rule buf)
      (match rule
        ((pat value)
         `(,(rewrite-pattern pat) ,value))
        ((pat ('in in* ...) ('out out* ...) tmpl)
         (let* ((bindings (generate-bindings (reverse in*) buf))
                (return-value (if (null? out*) '() (car out*)))
                (return-value-binding (cond
                                       ((null? return-value) '())
                                       ((find (lambda (in) (eq? in return-value)) in*)
                                        '())
                                       (else
                                        `((,return-value (gensym 't)))))))
           (if (null? return-value)
               `(,(rewrite-pattern pat)
                 (let* ,bindings
                   (box-set! ,buf (append (box-ref ,buf) (list ,@tmpl)))))
               `(,(rewrite-pattern pat)
                 (let ,return-value-binding
                   (let* ,bindings
                     (box-set! ,buf (append (box-ref ,buf) (list ,@tmpl)))
                     ,return-value))))))))
    (match e
      (('munch node buf rule* ...)
       `(match ,node
          ,@(fold (lambda (rule lst)
                    (cons (generate-rule rule buf) lst))
                  '()
                  rule*)
          (else (munch-rest ,node ,buf)))))))


(define-syntax instruction-info
  (lambda (e r c)
    (define (generate-instruction-descriptor name fmt op-spec)
      (let ((%define (r 'define))
            (%lambda (r 'lambda))
            (%let    (r 'let))
            (descriptor-name (string->symbol (format "~s-descriptor" name))))
        `((,%define ,descriptor-name
            (make-instr-descriptor
             ',name
             ',fmt
             (make-def-fold-proc ',op-spec)
             (make-use-fold-proc ',op-spec)))
          (,%define ,name
            (,%lambda operands
              (make-instr ,descriptor-name operands))))))
    (match e
      (('instruction-info arch definition* ...)
       (let ((compiled-definitions
              (apply append
                (map (lambda (definition)
                       (match definition
                         ((name (op-spec* ...) fmt)
                          (generate-instruction-descriptor name fmt op-spec*))))
                     definition*)))
             (%begin (r 'begin)))
         `(,%begin
           ,@compiled-definitions))))))

