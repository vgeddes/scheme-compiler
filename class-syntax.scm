
(import-for-syntax matchable)
(import-for-syntax srfi-1)

(define-for-syntax (make-gensyms inits)
  (map (lambda (init)
         (cons init (gensym 't)))
       inits))

(define-for-syntax (rename bindings expr)
  (cond 
   ((list? expr)
    (map (lambda (e) (rename bindings e))
         expr))
   ((symbol? expr) 
    (cond ((assq expr bindings) => cdr)
          (else expr)))
   (else expr)))
 
(define-for-syntax (emit-initializer inits super-inits slots slot-inits)
  (let ((bindings (fold (lambda (slot slot-init bindings)
                          (cons `(slot-set! object ',slot ,slot-init) bindings))
                         '() slots slot-inits)))
    `(lambda (class)
      (lambda (object ,@inits)
        ((class-initializer (class-super class)) object ,@super-inits)
        ,@bindings))))

(define-for-syntax (emit-class name inits super super-inits slots slot-inits)
  (let ((initializer (emit-initializer inits super-inits slots slot-inits)))
    `(begin
       (define ,name (make <class> ',name ,super ',slots ,initializer)))))

(define-syntax define-class
  (lambda (e r c)
    (match e
      (('define-class name (inits ...) '=> super (super-inits ...) (slots slot-inits) ...)
       (emit-class name inits super super-inits slots slot-inits))
      (('define-class name (inits ...) (slots slot-inits) ...)
       (emit-class name inits '<top> '() slots slot-inits)))))

(define-syntax match-object
  (lambda (e r c)
    (match e
      ((match-object object ((type bindings ...) expr ...) ... ('else else-expr ...))
       `(cond
         ,@(reverse (fold (lambda (type bindings expr x)
                            (cons `((eq? (class-of ,object) ,type)
                                    (let
                                        ,(fold (lambda (name bindings)
                                                 (cons `(,name (slot-ref ,object ',name)) bindings))
                                               '()
                                               bindings)
                             ,@expr))
                         x))
                 '() type bindings expr))
         (else ,@else-expr))))))