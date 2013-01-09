(declare (unit helpers))
(module helpers ()

  (import scheme)
  (import chicken)

  (import-for-syntax matchable)
  (import-for-syntax srfi-1)

  (export assert-not-reached)
  (export define-struct struct-case struct-let* push)

  (define-syntax push
    (syntax-rules ()
      ((push lst val)
       (set! lst (cons val lst)))))

  (define-syntax assert-not-reached
    (lambda (e r c)
      (let ((%assert (r 'assert)))
        `(,%assert #f "should not reach here"))))

  (define-syntax define-struct
    (syntax-rules ()
      ((define-struct name (fields ...))
       (define-record name fields ...))))

  (define-syntax struct-case
    (lambda (e r c)
      (let* ((%begin      (r 'begin))
             (%if         (r 'if))
             (%let        (r 'let))
             (%block-ref  (r '##sys#block-ref))
             (%structure? (r '##sys#structure?)))
        (define (generate-bindings v fields i)
          (match fields
                 (() '())
                 ((x . x*)
                  (cons `(,x (,%block-ref ,v ,i)) (generate-bindings v x* (+ i 1))))))
        (define (generate-body v clauses)
          (match clauses
                 (() `(error 'struct-case "unmatched " ,v))
                 ((('else expr expr* ...))
                  `(,%begin ,expr ,@expr*))
                 ((((name fields* ...) expr expr* ...) clause* ...)
                  (let ((bindings (generate-bindings v fields* 1))
                        (altern   (generate-body v clause*))
                        (pred     (string->symbol (format "~s?" name))))
                    `(,%if (,pred ,v)
                           (,%let ,bindings
                                  ,expr ,@expr*)
                           ,altern)))))
        (match e
               ((_ expr clause* ...)
                (let* ((v (gensym))
                       (body (generate-body v clause*)))
                  `(,%let ((,v ,expr)) ,body)))))))

  (define-syntax struct-let*
    (syntax-rules ()
      ((struct-let* () body* ...)
       (begin body* ...))
      ((struct-let* ((pat v) cls* ...) body* ...)
       (struct-case v
                    (pat
                     (struct-let* (cls* ...) body* ...))))))

  )
