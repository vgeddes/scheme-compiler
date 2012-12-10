
(import-for-syntax matchable)
(import-for-syntax srfi-1)

(define-syntax define-node
  (lambda (ex r c)
    (match ex
     (('define-node name (field* ...))
      (let ((make (string->symbol (format "make-~a" name))))
       `(begin
          (define-record ,name data ,@field*)
          (define (,name ,@field*)
            (,make '() ,@field*))))))))
       

(define-syntax node-case
  (lambda (ex r c)
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
      (define (generate v rule*)
        (match rule*
          (() `(error 'struct-case "unmatched " ,v))
          ((('else expr expr* ...))
           `(,%begin ,expr ,@expr*))
          ((((type (field* ...)) expr expr* ...) rest* ...)
           (let ((bindings (generate-bindings v field* 2))
                 (altern   (generate v rest*)))
             `(,%if (,%structure? ,v ',type)
                    (,%let ,bindings
                           ,expr ,@expr*)
                    ,altern)))))
      (match ex
        (('node-case obj rule* ...)
         (let* ((v    (gensym))
                (body (generate v rule*)))
           `(,%let ((,v ,obj)) ,body)))))))
  
