
(define-munch-rules

  ((alloc (temp x) (i32 size))
   (temps) (out)
   ((mov64rr 'rsi x)
    (add64i32r size 'rsi)))

  ;; mov

  ((mov op1 (temp y))
   (temps) (out)
   ((mov64rr op1 y)))
  
  ;; branch
  
  ((br (label x))
   (temps) (out)
   ((jmp64rel32 x)))
  
  ;; branch if true
  
  ((brc op1 (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r 0 op1)
    (jne32rel32 tl)))

  ;; branch if >
  
  ((brc (cmpgt (i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r x op2)
    (jg32rel32 t1)))

  ((brc (cmpgt (i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r x op2)
    (jg32rel32 tl)))

  ((brc (cmpgt op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr op1 op2)
    (jg32rel32 tl)))

  ;; branch if >=
  
  ((brc (cmpge (i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r x op2)
    (jge32rel32 tl)))

  ((brc (cmpge (i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r x op2)
    (jge32rel32 tl)))

  ((brc (cmpge op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr op1 op2)
    (jge32rel32 tl)))

  ;; branch if == 

  ((brc (cmpeq (i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r x op2)
    (je32rel32 tl)))
  
  ((brc (cmpeq (i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r x op2)
    (je32rel32 tl)))
  
  ((brc (cmpeq op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr op1 op2)
    (je32rel32 tl)))

  ;; branch if < 
  
  ((brc (cmplt (i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r x op2)
    (jl32rel32 tl)))

  ((brc (cmplt (i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r x op2)
    (jl32rel32 tl)))

  ((brc (cmplt op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr op1 op2)
    (jl32rel32 tl)))

  ;; branch if <= 
  
  ((brc (cmple (i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r x op2)
    (jle32rel32 tl)))

  ((brc (cmple (i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r x op2)
    (jle32rel32 tl)))

  ((brc (cmple op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr op1 op2)
    (jle32rel32 tl)))
  
  ;; compare <= 

  ((cmple (i8 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i8r x op2)
    (setle8r t1)))

  ((cmple (i32 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i32r x op2)
    (setle8r t1)))

  ((cmple op1 op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64rr op1 op2)
    (setle8r t1)))

  ;; compare <

  ((cmplt (i8 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i8r x op2)
    (setl8r t1)))

  ((cmplt (i32 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i32r x op2)
    (setl8r t1)))

  ((cmplt op1 op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64rr op1 op2)
    (setl8r t1)))
  
  ;; compare == 
  
  ((cmpeq (i8 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i8r x op2)
    (sete8r t1)))

  ((cmpeq (i32 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i32r x op2)
    (sete8r t1)))

  ((cmpeq op1 op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64rr op1 op2)
    (sete8r t1)))
  
  ;; compare >

  ((cmpgt (i8 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i8r x op2)
    (setg8r t1)))

  ((cmpgt (i32 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i32r x op2)
    (setg8r t1)))

  ((cmpgt op1 op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64rr op1 op2)
    (setg8r t1)))

  ;; compare >=

  ((cmpge (i8 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i8r x op2)
    (setge8r t1)))

  ((cmpge (i32 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i32r x op2)
    (setge8r t1)))

  ((cmpge op1 op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64rr op1 op2)
    (setge8r t1)))
  
  ;; temp reference
  
  ((temp t) t)
  
  ;; load immediate
  
  ((i32 x)
   (temps t1) (out t1)
   ((mov64i32r x t1)))
  
  ;; memory load
  
  ((mov (ldq base offset) (temp x))
   (temps) (out)
   ((mov64mr (mem base offset) x)))

  ;; memory store

  ((stq (temp x) (temp base) (i32 offset))
   (temps) (out)
   ((mov64rm x (mem base offset))))

  ((stq (label x) (temp base) (i32 offset))
   (temps t1) (out)
   ((lea64mr (mem 'rip x) t1)
    (mov64rm t1 (mem base offset))))

  ;; add
  
  ((add op1 (i32 x))
   (temps t5) (out) 
   ((mov64rr op1 t5)
    (add64i32r x t5)))
  
  ((add (i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (add64i32r x t5)))

  ((add op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (add64rr op1 t5)))

  ;; sub
  
  ((sub op1 (i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (sub64i32r x t5)))
  
  ((sub (i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (sub64i32r x t5)))

  ((sub op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (sub64rr op1 t5)))
  
  ;; and

  ((and op1 (i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (and64i32r x t5)))
  
  ((and (i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (and64i32r x t5)))

  ((and op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (and64rr op1 t5)))

  ;; or

  ((or op1 (i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (or64i32r x t5)))
  
  ((or (i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (or64i32r x t5)))

  ((or op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (or64rr op1 t5)))

  ;; xor

  ((xor op1 (i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (xor64i32r x t5)))

  ((xor (i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (xor64i32r x t5)))

  ((xor op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (xor64rr op1 t5)))

  ;; shl
  
  ((shl (i8 x) op2)
   (temps t5) (out t5)
   ((mov64rr op2 t5)
    (shl64i8r x t5)))

  ;; shr
  
  ((shr (i8 x) op2)
   (temps t5) (out t5)
   ((mov64rr op2 t5)
    (shr64i8r x t5))))
