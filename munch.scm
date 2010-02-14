  
(declare (unit munch)
         (uses x86-64))

(use matchable)
(use srfi-1)

(include "class-syntax")
(include "munch-syntax")

;; closures

(define (L label)
  (list 'L label))

(define (M b o)
  (list 'M b o))

(define (buf-append! buf data)
  (box-set! buf (append (box-ref buf) data)))

(define (munch-rest node buf)
  (match node
    (#('call label args)
     (buf-append! buf (list (call32rel32 label))))
    (_ (error 'munch "unknown rtl node" node))))

(define (munch-expr node buf)
  (munch node buf

    ((alloc (T x) (i32 size))
     (in) (out)
     ((mov64rr 'rsi x)
      (add64i32r size 'rsi)))

    ;; mov

    ((mov op1 (T y))
     (in op1) (out)
     ((mov64rr op1 y)))
         
    ;; branch
         
    ((br (L label))
     (in) (out)
     ((jmp64rel32 (L label))))
    
    ;; branch if true
    
    ((brc op1 (L tl) (L fl))
     (in op1) (out)
     ((cmp64i8r 0 op1)
      (jne32rel32 (L tl))))

    ;; branch if >
    
    ((brc (cmpgt (i8 x) op2) (L tl) (L fl))
     (in op2) (out)
     ((cmp64i8r op1 op2)
      (jg32rel32 (L tl))))

    ((brc (cmpgt (i32 x) op2) (L tl) (L fl))
     (in op2) (out)
     ((cmp64i8r op1 op2)
      (jg32rel32 (L tl))))

    ((brc (cmpgt op1 op2) (L tl) (L fl))
     (in op1 op2) (out)
     ((cmp64rr op1 op2)
      (jg32rel32 (L tl))))

    ;; branch if >=
    
    ((brc (cmpge (i8 x) op2) (L tl) (L fl))
     (in op2) (out)
     ((cmp64i8r x op2)
      (jge32rel32 (L tl))))

    ((brc (cmpge (i32 x) op2) (L tl) (L fl))
     (in op2) (out)
     ((cmp64i32r x op2)
      (jge32rel32 (L tl))))

    ((brc (cmpge op1 op2) (L tl) (L fl))
     (in op1 op2) (out)
     ((cmp64rr op1 op2)
      (jge32rel32 (L tl))))

    ;; branch if == 

    ((brc (cmpeq (i8 x) op2) (L tl) (L fl))
     (in op2) (out)
     ((cmp64i8r x op2)
      (je32rel32 (L tl))))
    
    ((brc (cmpeq (i32 x) op2) (L tl) (L fl))
     (in op2) (out)
     ((cmp64i32r x op2)
      (je32rel32 (L tl))))
    
    ((brc (cmpeq op1 op2) (L tl) (L fl))
     (in op1 op2) (out)
     ((cmp64rr op1 op2)
      (je32rel32 (L tl))))

    ;; branch if < 
    
    ((brc (cmplt (i8 x) op2) (L tl) (L fl))
     (in op2) (out)
     ((cmp64i8r x op2)
      (jl32rel32 (L tl))))

    ((brc (cmplt (i32 x) op2) (L tl) (L fl))
     (in op2) (out)
     ((cmp64i32r x op2)
      (jl32rel32 (L tl))))

    ((brc (cmplt op1 op2) (L tl) (L fl))
     (in op1 op2) (out)
     ((cmp64rr op1 op2)
      (jl32rel32 (L tl))))

    ;; branch if <= 
    
    ((brc (cmple (i8 x) op2) (L tl) (L fl))
     (in op2) (out)
     ((cmp64i8r x op2)
      (jle32rel32 (L tl))))

    ((brc (cmple (i32 x) op2) (L tl) (L fl))
     (in op2) (out)
     ((cmp64i32r x op2)
      (jle32rel32 (L tl))))

    ((brc (cmple op1 op2) (L tl) (L fl))
     (in op1 op2) (out)
     ((cmp64rr op1 op2)
      (jle32rel32 (L tl))))
         
    ;; compare <= 

    ((cmple (i8 x) op2)
     (in op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64i8r x op2)
      (setle8r t1)))

    ((cmple (i32 x) op2)
     (in op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64i32r x op2)
      (setle8r t1)))

    ((cmple op1 op2)
     (in op1 op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64rr op1 op2)
      (setle8r t1)))

    ;; compare <

    ((cmplt (i8 x) op2)
     (in op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64i8r x op2)
      (setl8r t1)))

    ((cmplt (i32 x) op2)
     (in op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64i32r x op2)
      (setl8r t1)))

    ((cmplt op1 op2)
     (in op1 op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64rr op1 op2)
      (setl8r t1)))
    
    ;; compare == 
    
    ((cmpeq (i8 x) op2)
     (in op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64i8r x op2)
      (sete8r t1)))

    ((cmpeq (i32 x) op2)
     (in op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64i32r x op2)
      (sete8r t1)))

    ((cmpeq op1 op2)
     (in op1 op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64rr op1 op2)
      (sete8r t1)))
    
    ;; compare >

    ((cmpgt (i8 x) op2)
     (in op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64i8r x op2)
      (setg8r t1)))

    ((cmpgt (i32 x) op2)
     (in op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64i32r x op2)
      (setg8r t1)))

    ((cmpgt op1 op2)
     (in op1 op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64rr op1 op2)
      (setg8r t1)))

    ;; compare >=

    ((cmpge (i8 x) op2)
     (in op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64i8r x op2)
      (setge8r t1)))

    ((cmpge (i32 x) op2)
     (in op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64i32r x op2)
      (setge8r t1)))

    ((cmpge op1 op2)
     (in op1 op2) (out t1)
     ((mov64i32r 0 t1)
      (cmp64rr op1 op2)
      (setge8r t1)))
    
    ;; temp reference
                   
    ((T t) t)
    
    ;; load immediate
    
    ((i32 x)
     (in) (out t1)
     ((mov64i32r x t1)))
    
    ;; memory load
    
    ((mov (ldq base offset) (T x))
     (in) (out)
     ((mov64mr (M base offset) x)))

    ;; memory store

    ((stq (T x) (T base) (i32 offset))
     (in) (out)
     ((mov64rm x (M base offset))))

    ;; add
    
    ((add op1 (i32 x))
     (in op1) (out t5) 
     ((mov64rr op1 t5)
      (add64i32r x t5)))
    
    ((add (i32 x) op2)
     (in op2) (out t5) 
     ((mov64rr op2 t5)
      (add64i32r x t5)))

    ((add op1 op2)
     (in op1 op2) (out t5) 
     ((mov64rr op2 t5)
      (add64rr op1 t5)))

    ;; sub
   
    ((sub op1 (i32 x))
     (in op1) (out t5) 
     ((mov64rr op1 t5)
      (sub64i32r x t5)))
    
    ((sub (i32 x) op2)
     (in op2) (out t5) 
     ((mov64rr op2 t5)
      (sub64i32r x t5)))

    ((sub op1 op2)
     (in op1 op2) (out t5) 
     ((mov64rr op2 t5)
      (sub64rr op1 t5)))
    
    ;; and

    ((and op1 (i32 x))
     (in op1) (out t5) 
     ((mov64rr op1 t5)
      (and64i32r x t5)))
    
    ((and (i32 x) op2)
     (in op2) (out t5) 
     ((mov64rr op2 t5)
      (and64i32r x t5)))

    ((and op1 op2)
     (in op1 op2) (out t5) 
     ((mov64rr op2 t5)
      (and64rr op1 t5)))

    ;; or

    ((or op1 (i32 x))
     (in op1) (out t5) 
     ((mov64rr op1 t5)
      (or64i32r x t5)))
    
    ((or (i32 x) op2)
     (in op2) (out t5) 
     ((mov64rr op2 t5)
      (or64i32r x t5)))

    ((or op1 op2)
     (in op1 op2) (out t5) 
     ((mov64rr op2 t5)
      (or64rr op1 t5)))

    ;; xor

    ((xor op1 (i32 x))
     (in op1) (out t5) 
     ((mov64rr op1 t5)
      (xor64i32r x t5)))

    ((xor (i32 x) op2)
     (in op2) (out t5) 
     ((mov64rr op2 t5)
      (xor64i32r x t5)))

    ((xor op1 op2)
     (in op1 op2) (out t5) 
     ((mov64rr op2 t5)
      (xor64rr op1 t5)))

    ;; shl
   
    ((shl (i8 x) op2)
     (in op2) (out t5)
     ((mov64rr op2 t5)
      (shl64i8r x t5)))

    ;; shr
   
    ((shr (i8 x) op2)
     (in op2) (out t5)
     ((mov64rr op2 t5)
      (shr64i8r x t5)))))
