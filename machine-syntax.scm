
(import-for-syntax matchable)
(import-for-syntax srfi-1)


(define-syntax assert-not-reached
  (lambda (e r c)
     `(,(r 'assert) #f "should not reach here")))




(define-syntax define-machine-instructions
  (lambda (e r c)

;; operand types
;; i8   8-bit immediate
;; i32  32-bit immediate
;; i64  64-bit immediate
;; m64  64-bit memory reference (using [base + disp] addressing)
;; r8   8-bit register
;; r64  64-bit register

    (define (parse-operand-type type)
      (case type
        ((i8 i32 i64)
          'machine-imm?)
        ((m32 m64)
          'machine-addr-x86-64?)
        ((r8 r32 r64)
          'machine-vreg?)
        (else (assert-not-reached))))

    (define (operand-is-use? flags)
      (and (memq 'in flags)  #t))

    (define (operand-is-def? flags)
      (and (memq 'out flags) #t))

    ;; scan operand spec string and return the following three lists
    ;;
    ;;  list of verifier functions for each operand
    ;;  list of operand indices of temps which are USED
    ;;  list of operand indices of temps which are DEFINED

    (define (parse-operand-specs operand-spec*)
       (let f ((os* operand-spec*) (i 0) (uses '()) (defs '()) (verifiers '()))
         (match os*
           (()
            (list 
              (reverse verifiers)
              (reverse uses)
              (reverse defs)))
           ((os . os*)
            (match os
               ((type flag* ...)
                 (let ((verifier (parse-operand-type type))
                       (uses      (if (operand-is-use? flag*)
                                     (cons i uses) uses))
                       (def       (if (operand-is-def? flag*)
                                     (cons i defs) defs)))
                    (f os* (+ i 1) uses defs (cons verifier verifiers)))))))))


    (define (generate-instr-descriptor name fmt operand-spec*)
      (let* ((descriptor                (string->symbol (format "~s-descriptor" name)))
             (operand-info              (parse-operand-specs operand-spec*))
             (verifiers                 (first operand-info))
             (uses                      (second operand-info))
             (defs                      (third operand-info))
             (%define                   (r 'define))
             (%lambda                   (r 'lambda))
             (%let                      (r 'let))
             (%make-machine-instr       (r 'make-machine-instr))
             (%make-machine-descriptor  (r 'make-machine-descriptor)))
        
        `((,%define ,descriptor
            (make-machine-descriptor
             ',name
             ',fmt
             ',verifiers
             ',uses
             ',defs))
          (,%define ,name
            (,%lambda operands
              (make-machine-instr ,descriptor operands '() '() '()))))))
           
    

    (match e
      (('define-machine-instructions arch instr-def* ...)
       (let ((compiled-definitions
              (apply append
                (map (lambda (instr-def)
                       (match instr-def
                         ((name (operand-spec* ...) fmt)
                          (generate-instr-descriptor name fmt operand-spec*))))
                     instr-def*)))
             (%begin (r 'begin)))
         `(,%begin
           ,@compiled-definitions))))))


;; utils


