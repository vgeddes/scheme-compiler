
(import-for-syntax matchable)
(import-for-syntax srfi-1)


(define-syntax assert-not-reached
  (lambda (e r c)
     `(,(r 'assert) #f "should not reach here")))

(define-syntax define-arch-instructions
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
          'mc-imm?)
        ((disp32)
          'mc-disp?)
        ((reg)
          'mc-vreg?)
        (else (assert-not-reached))))

    (define (is-in? flags)
      (and (memq 'in flags)  #t))

    (define (is-out? flags)
      (and (memq 'out flags) #t))

    ;; Generate a function for accessing specific temps, based on an input list of booleans
    ;; Used to to created def/use accessors.
    ;;
    (define (gen-accessor bool-flag*)
       (define (accessor index)
         (case index
           ((0) 'car)
           ((1) 'cadr)
           ((2) 'caddr)
           ((3) 'cadddr)
           (else (assert-not-reached))))
       (let f ((flag* bool-flag*) (i 0) (accessors '()))
          (match flag*
            (()
             `(lambda (ops) (append ,@(reverse accessors))))
            ((flag . flag*)
             (let ((accessors (if flag
                                  (cons `(mc-operand-vregs (,(accessor i) ops)) accessors)
                                 accessors)))
               (f flag* (+ i 1) accessors))))))
       

    ;; Parse the operand spec string and return the following three lists
    ;;
    ;;
    ;;  list of verifier functions for each operand
    ;;  list of booleans indicating temps which are USED
    ;;  list of booleans indicating temps which are DEFINED
    ;;
    ;;  For example 
    ;;              ((i32) (r64 in))  =>  ((mc-imm? mc-vreg?)
    ;;                                     (#f #t)
    ;;                                     (#f #f)
    ;;

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
                      (uses      (if (is-in? flag*)
                                    (cons #t uses)
                                    (cons #f uses)))
                      (defs       (if (is-out? flag*)
                                    (cons #t defs)
                                    (cons #f defs))))
                   (f os* (+ i 1) uses defs (cons verifier verifiers)))))))))


    (define (gen-instr-spec arch name fmt operand-spec*)
      (let* ((spec                      (string->symbol (format "~s.~s-spec" arch name)))
             (predicate                 (string->symbol (format "~s.~s?" arch name)))
             (operand-info              (parse-operand-specs operand-spec*))
             (verifiers                 (first operand-info))
             (vregs-read                (gen-accessor (second operand-info)))
             (vregs-written             (gen-accessor (third operand-info)))
             (%define                   (r 'define))
             (%lambda                   (r 'lambda))
             (%let                      (r 'let))
             (%mc-make-instr            (r 'mc-make-instr))
             (%make-mc-spec             (r 'make-mc-spec)))

        `((,%define ,spec
            (,%make-mc-spec
             ',name
             ',fmt
             ',verifiers
              ,vregs-read
              ,vregs-written))
          (,%define ,predicate
            (,%lambda (x)
                (and (mc-instr? x) (eq? (mc-instr-spec x) ,spec))))
          (,%define ,name
            (,%lambda operands
              (mc-make-instr ,spec operands))))))

    (match e
      (('define-arch-instructions arch spec* ...)
       (let ((code
              (apply append
                (map (lambda (instr-def)
                       (match instr-def
                         ((name (operand-spec* ...) fmt)
                          (gen-instr-spec arch name fmt operand-spec*))))
                     spec*)))
             (%begin (r 'begin)))
         `(,%begin
           ,@code))))))

;; Convenience macro for manual code emission

(define-syntax arch-emit-code
  (lambda (e r c)
     
    (define (expand blk e)
      (let ((%mc-context-allocate-vreg (r 'mc-context-allocate-vreg))
            (%mc-block-cxt             (r 'mc-block-cxt))
            (%mc-make-imm              (r 'mc-make-imm))
            (%mc-make-disp             (r 'mc-make-disp)))

      (match e
        ((e1* ...)
         (map (lambda (e) (expand blk e)) e1*))
        (('vreg x)
         `(,%mc-context-allocate-vreg (,%mc-block-cxt ,blk) ,x))
        (('imm size x)
         `(,%mc-make-imm ,size ,x))
        (('disp x)
         `(,%mc-make-disp ,x))
        (_ e))))

    (define (generate arch blk instr)
      (let ((qualified-name (string->symbol (format "~s.~s" arch (car instr)))))
        (match instr
          ((name operands* ...)
           `(,qualified-name ,blk ,@(expand blk operands*))))))

     (match e
       (('arch-emit-code arch blk x* ...)
        `(begin
           ,@(map (lambda (instr)
                    (generate arch blk instr))
                  x*))))))


