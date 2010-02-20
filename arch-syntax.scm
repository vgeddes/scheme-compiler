
(import-for-syntax matchable)
(import-for-syntax srfi-1)

(define-syntax define-arch-instructions
  (lambda (e r c)

    (define (generate-instr-descriptor name fmt operand-spec*)
      (let ((descriptor (string->symbol (format "~s-descriptor" name)))
            (%define                     (r 'define))
            (%lambda                     (r 'lambda))
            (%let                        (r 'let))
            (%make-instr-with-descriptor (r 'make-instr-with-descriptor))
            (%make-instr-descriptor      (r 'make-instr-descriptor)))
        `((,%define ,descriptor
            (make-instr-descriptor
             ',name
             ',fmt
             ',operand-spec*))
          (,%define ,name
            (,%lambda operands
              (,%make-instr-with-descriptor ,descriptor operands))))))
           
    (match e
      (('define-arch-instructions arch instr-def* ...)
       (let ((compiled-definitions
              (apply append
                (map (lambda (instr-def)
                       (match instr-def
                         ((name (operand-spec* ...) fmt)
                          (generate-instr-descriptor name operand-spec* fmt))))
                     instr-def*)))
             (%begin (r 'begin)))
         `(,%begin
           ,@compiled-definitions))))))

