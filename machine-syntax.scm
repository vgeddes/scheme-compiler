
(import-for-syntax matchable)
(import-for-syntax srfi-1)

(define-syntax define-arch-instructions
  (lambda (e r c)

;; operand types
;; i8   8-bit immediate
;; i32  32-bit immediate
;; i64  64-bit immediate
;; m64  64-bit memory reference (using [base + disp] addressing)
;; r8   8-bit register
;; r64  64-bit register

    ;; scan operand spec string and determine which operands are used, defined,
    ;; and create a bunch of functions for accessing or modifying operands
    (define (generate-instr-funcs spec*)
       (let f ((i 0) (s* spec*) (uses '()) (defs '()) (verifiers '()))
         (match spec*
           (() '())
           ((s . s*)
            (match s
               ((or (i8 flag* ...) (i32 flag* ...) (i64 flag* ...))
                (cons 'machine-constant? verifiers))
               ((m64 flag* ...)
                (cons 'machine-address? verifiers))
               ((r64 flag* ....)
                (cons 'machine-temp? verifiers))

             
 

            (f (+ i 1) s*)))))
            
          

        







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

