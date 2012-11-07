

(define-munch-rules
  
  ;; branch to label
  
  ((br (label x))
   (temps) (out)
   ((jmp64rel32 `(label ,x))))

  ;; branch indirect

  ((br (temp x))
   (temps) (out)
   ((jmp64r x)))
  
  ;; branch if true
  
  ((brc op1 (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r 0 op1)
   (jne32rel32 `(label ,tl))))

  ;; branch if >
  
  ((brc (cmp (mode i8) (op gt) (const i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r x op2)
    (jg32rel32 `(label ,tl))))

  ((brc (cmp (mode i32) (op gt) (const i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r x op2)
    (jg32rel32 `(label ,tl))))

  ((brc (cmp (mode i32) (op gt) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr op1 op2)
    (jg32rel32 `(label ,tl))))

  ;; branch if >=
  
  ((brc (cmp (mode i8) (op ge) (const i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r x op2)
    (jge32rel32 `(label ,tl))))

  ((brc (cmp (mode i32) (op ge) (const i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r x op2)
    (jge32rel32 `(label ,tl))))

  ((brc (cmp (mode i32) (op ge) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr op1 op2)
    (jge32rel32 `(label ,tl))))

  ;; branch if == 

  ((brc (cmp (mode i8) (op eq) (const i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r x op2)
    (je32rel32 `(label ,tl))))
  
  ((brc (cmp (mode i32) (op eq) (const i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r x op2)
    (je32rel32 `(label ,tl))))
  
  ((brc (cmp (mode i32) (op eq) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr op1 op2)
    (je32rel32 `(label ,tl))))

  ;; branch if < 
  
  ((brc (cmp (mode i8) (op lt) (const i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r x op2)
    (jl32rel32 `(label ,tl))))

  ((brc (cmp (mode i32) (op lt) (const i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r x op2)
    (jl32rel32 `(label ,tl))))

  ((brc (cmp (mode i32) (op lt) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr op1 op2)
    (jl32rel32 `(label ,tl))))

  ;; branch if <= 
  
  ((brc (cmp (mode i8) (op le) (const i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r x op2)
    (jle32rel32 `(label ,tl))))

  ((brc (cmp (mode i32) (op le) (const i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r x op2)
    (jle32rel32 `(label ,tl))))

  ((brc (cmp (mode i32) (op le) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr op1 op2)
    (jle32rel32 `(label ,tl))))
  
  ;; compare <= 

  ((cmp (mode i8) (op le) (const i8 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i8r x op2)
    (setle8r t1)))

  ((cmp (mode i32) (op le) (const i32 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i32r x op2)
    (setle8r t1)))

  ((cmp (mode i32) (op le) op1 op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64rr op1 op2)
    (setle8r t1)))

  ;; compare <

  ((cmp (mode i8) (op lt) (const i8 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i8r x op2)
    (setl8r t1)))

  ((cmp (mode i32) (op lt) (const i32 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i32r x op2)
    (setl8r t1)))

  ((cmp (mode i32) (op lt) op1 op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64rr op1 op2)
    (setl8r t1)))
  
  ;; compare == 
  
  ((cmp (mode i8) (op eq) (const i8 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i8r x op2)
    (sete8r t1)))

  ((cmp (mode i32) (op eq) (const i32 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i32r x op2)
    (sete8r t1)))

  ((cmp (mode i32) (op eq) op1 op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64rr op1 op2)
    (sete8r t1)))
  
  ;; compare >

  ((cmp (mode i8) (op gt) (const i8 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i8r x op2)
    (setg8r t1)))

  ((cmp (mode i32) (op gt) (const i32 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i32r x op2)
    (setg8r t1)))

  ((cmp (mode i32) (op gt) op1 op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64rr op1 op2)
    (setg8r t1)))

  ;; compare >=

  ((cmp (mode i8) (op ge) (const i8 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i8r x op2)
    (setge8r t1)))

  ((cmp (mode i32) (op ge) (const i32 x) op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64i32r x op2)
    (setge8r t1)))

  ((cmp (mode i32) (op ge) op1 op2)
   (temps t1) (out t1)
   ((mov64i32r 0 t1)
    (cmp64rr op1 op2)
    (setge8r t1)))
  
  ;; temp reference
  
  ((temp t) t)
  
  ;; load immediate
  
  ((const i8 x)
   (temps t1) (out t1)
   ((mov64i8r x t1)))
  
  ((const i32 x)
   (temps t1) (out t1)
   ((mov64i32r x t1)))
  
  ;; memory load
  
  ((assign (temp x)
     (load (mode i64) (add (mode i32)
                        (temp base)
                        (const i32 disp))))
   (temps) (out)
   ((mov64mr (mem base disp) x)))

  ((assign (temp x) (load (mode i64) (label disp)))
   (temps) (out)
   ((mov64mr (mem 'rip disp) x)))

  ;; memory store

  ((store (mode i64) (temp x) (add (mode i32)
                                (temp base)
                                (const i32 disp)))
   (temps) (out)
   ((mov64rm x (mem base disp))))

  ((store (mode i64) (label x) (add (mode i32)
                                 (temp base)
                                 (const i32 disp)))
   (temps t1) (out t1)
   ((lea64mr (mem 'rip x) t1)
    (mov64rm t1 (mem base disp))))

  ((store (mode i64) op1 (add (mode i32)
                                 (temp base)
                                 (const i32 disp)))
   (temps) (out)
   ((mov64rm op1 (mem base disp))))

  ((store (mode i64) op1 (label disp))
   (temps) (out)
   ((mov64rm op1 (mem 'rip disp))))

  ;; assign
  
  ((assign (temp y) op1)
   (temps) (out)
   ((mov64rr op1 y)))
  
  ;; add
  
  ((add (mode i64) op1 (const i8 x))
   (temps t5) (out t5)
   ((mov64rr op1 t5)
    (add64i8r x t5)))

  ((add (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (add64i8r x t5)))

  ((add (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (add64i32r x t5)))
  
  ((add (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (add64i32r x t5)))

  ((add (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (add64rr op1 t5)))

  ;; sub
  
  ((sub (mode i64) op1 (const i8 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (sub64i8r x t5)))
  
  ((sub (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (sub64i8r x t5)))

  ((sub (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (sub64i32r x t5)))
  
  ((sub (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (sub64i32r x t5)))

  ((sub (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (sub64rr op1 t5)))
  
  ;; and

  ((and (mode i64) op1 (const i8 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (and64i8r x t5)))
  
  ((and (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (and64i8r x t5)))

  ((and (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (and64i32r x t5)))
  
  ((and (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (and64i32r x t5)))

  ((and (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (and64rr op1 t5)))

  ;; or

  ((ior (mode i64) op1 (const i8 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (or64i8r x t5)))
  
  ((ior (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (or64i8r x t5)))

  ((ior (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (or64i32r x t5)))
  
  ((ior (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (or64i32r x t5)))

  ((ior (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (or64rr op1 t5)))

  ;; xor

  ((xor (mode i64) op1 (const i8 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (xor64i8r x t5)))

  ((xor (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (xor64i8r x t5)))

  ((xor (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (xor64i32r x t5)))

  ((xor (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (xor64i32r x t5)))

  ((xor (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (xor64rr op1 t5)))

  ;; shl
  
  ((shl (mode i64) (const i8 x) op2)
   (temps t5) (out t5)
   ((mov64rr op2 t5)
    (shl64i8r x t5)))

  ;; shr
  
  ((shr (mode i64) (const i8 x) op2)
   (temps t5) (out t5)
   ((mov64rr op2 t5)
    (shr64i8r x t5))))

