

(define-munch-rules
  
  ;; branch to label
  
  ((br (label x))
   (temps) (out)
   ((jmp64rel32 (addr (disp x)))))

  ;; branch indirect

  ((br (temp x))
   (temps) (out)
   ((jmp64r (addr (base (vreg x))))))
  
  ;; branch if true
  
  ((brc op1 (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r  (imm i8 0) op1)
   (jne32rel32 (addr (disp tl)))))

  ;; branch if >
  
  ((brc (cmp (mode i8) (op gt) (const i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r  (imm i8 x) op2)
    (jg32rel32 (addr (disp tl)))))

  ((brc (cmp (mode i32) (op gt) (const i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r (imm i32 x) op2)
    (jg32rel32 (addr (disp tl)))))

  ((brc (cmp (mode i32) (op gt) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr   op1 op2)
    (jg32rel32 (addr (disp tl)))))

  ;; branch if >=
  
  ((brc (cmp (mode i8) (op ge) (const i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r (imm i8 x) op2)
    (jge32rel32 (addr (disp tl)))))

  ((brc (cmp (mode i32) (op ge) (const i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r (imm i8 x) op2)
    (jge32rel32 (addr (disp tl)))))

  ((brc (cmp (mode i32) (op ge) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr op1 op2)
    (jge32rel32 (addr (disp tl)))))

  ;; branch if == 

  ((brc (cmp (mode i8) (op eq) (const i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r (imm i8 x) op2)
    (je32rel32 (addr (disp tl)))))
  
  ((brc (cmp (mode i32) (op eq) (const i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r (imm i32 x) op2)
    (je32rel32 (addr (disp tl)))))
  
  ((brc (cmp (mode i32) (op eq) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr   op1 op2)
    (je32rel32 (addr (disp tl)))))

  ;; branch if < 
  
  ((brc (cmp (mode i8) (op lt) (const i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r  (imm i8 x) op2)
    (jl32rel32 (addr (disp tl)))))

  ((brc (cmp (mode i32) (op lt) (const i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r (imm i32 x) op2)
    (jl32rel32 (addr (disp tl)))))

  ((brc (cmp (mode i32) (op lt) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr   op1 op2)
    (jl32rel32 (addr (disp tl)))))

  ;; branch if <= 
  
  ((brc (cmp (mode i8) (op le) (const i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i8r   (imm i8 x) op2)
    (jle32rel32 (addr (disp tl)))))

  ((brc (cmp (mode i32) (op le) (const i32 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64i32r  (imm i8 x) op2)
    (jle32rel32 (addr (disp tl)))))

  ((brc (cmp (mode i32) (op le) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp64rr    op1 op2)
    (jle32rel32 (addr (disp tl)))))
  
  ;; compare <= 

  ((cmp (mode i8) (op le) (const i8 x) op2)
   (temps t1) (out t1)
   ((xor64rr  t1 t1)
    (cmp64i8r (imm i8 x) op2)
    (setle8r  t1)))

  ((cmp (mode i32) (op le) (const i32 x) op2)
   (temps t1) (out t1)
   ((xor64rr   t1 t1)
    (cmp64i32r (imm i32 x)  op2)
    (setle8r   t1)))

  ((cmp (mode i32) (op le) op1 op2)
   (temps t1) (out t1)
   ((xor64rr t1  t1)
    (cmp64rr op1 op2)
    (setle8r t1)))

  ;; compare <

  ((cmp (mode i8) (op lt) (const i8 x) op2)
   (temps t1) (out t1)
   ((xor64rr  t1 t1)
    (cmp64i8r (imm i8 x) op2)
    (setl8r   t1)))

  ((cmp (mode i32) (op lt) (const i32 x) op2)
   (temps t1) (out t1)
   ((xor64rr   t1 t1)
    (cmp64i32r (imm i32 x)  op2)
    (setl8r    t1)))

  ((cmp (mode i32) (op lt) op1 op2)
   (temps t1) (out t1)
   ((xor64rr  t1 t1)
    (cmp64rr  op1 op2)
    (setl8r   t1)))
  
  ;; compare == 
  
  ((cmp (mode i8) (op eq) (const i8 x) op2)
   (temps t1) (out t1)
   ((xor64rr  t1 t1)
    (cmp64i8r (imm i8 x) op2)
    (sete8r   t1)))

  ((cmp (mode i32) (op eq) (const i32 x) op2)
   (temps t1) (out t1)
   ((xor64rr   t1 t1)
    (cmp64i32r (imm i32 x) op2)
    (sete8r    t1)))

  ((cmp (mode i32) (op eq) op1 op2)
   (temps t1) (out t1)
   ((xor64rr  t1 t1)
    (cmp64rr  op1 op2)
    (sete8r   t1)))
  
  ;; compare >

  ((cmp (mode i8) (op gt) (const i8 x) op2)
   (temps t1) (out t1)
   ((xor64rr  t1 t1)
    (cmp64i8r (imm i8 x) op2)
    (setg8r   t1)))

  ((cmp (mode i32) (op gt) (const i32 x) op2)
   (temps t1) (out t1)
   ((xor64rr   t1 t1)
    (cmp64i32r (imm i32 x) op2)
    (setg8r    t1)))

  ((cmp (mode i32) (op gt) op1 op2)
   (temps t1) (out t1)
   ((xor64rr  t1 t1)
    (cmp64rr  op1 op2)
    (setg8r   t1)))

  ;; compare >=

  ((cmp (mode i8) (op ge) (const i8 x) op2)
   (temps t1) (out t1)
   ((xor64rr  t1 t1)
    (cmp64i8r (imm i8 x) op2)
    (setge8r  t1)))

  ((cmp (mode i32) (op ge) (const i32 x) op2)
   (temps t1) (out t1)
   ((xor64rr   t1 t1)
    (cmp64i32r (imm i32 x) op2)
    (setge8r   t1)))

  ((cmp (mode i32) (op ge) op1 op2)
   (temps t1) (out t1)
   ((xor64rr t1 t1)
    (cmp64rr op1 op2)
    (setge8r t1)))
  
  ;; load effective address

  ((load (mode ptr64) (label l1))
   (temps t1) (out t1)
   ((lea64mr (addr (disp l1)) t1)))

  ((assign (temp x) (load (mode ptr64) (label l1)))
   (temps) (out)
   ((lea64mr (addr (disp l1)) (vreg x))))
  
  ;; load immediate
  
  ((const i8 x)
   (temps t1) (out t1)
   ((xor64rr  t1 t1)
    (mov64i8r (imm i8 x) t1)))
  
  ((const i32 x)
   (temps t1) (out t1)
   ((xor64rr   t1 t1)
    (mov64i32r (imm i32 x) t1)))

  ((const i64 x)
   (temps t1) (out t1)
   ((xor64rr   t1 t1)
    (mov64i64r (imm i64 x) t1)))

  
  ;; memory load
  
  ((load (mode i64) (add (mode i32)
                        (temp t1)
                        (const i32 c1)))
   (temps t2) (out t2)
   ((mov64mr (addr (base (vreg t1)) (disp c1))
             t2)))

  ((assign (temp x)
     (load (mode i64) (add (mode i32)
                        (temp t1)
                        (const i32 c1))))
   (temps) (out)
   ((mov64mr (addr (base (vreg t1)) (disp c1))
             (vreg x))))

  ((assign (temp x)
     (load (mode i64) (temp t1)))
   (temps) (out)
   ((mov64mr (addr (base (vreg t1))))))

  ((assign (temp x) (load (mode i64) (label l1)))
   (temps) (out)
   ((mov64mr (addr (disp l1)) (vreg x))))

  ;; memory store

  ((store (mode i64) (temp x) (add (mode i32)
                                (temp t1)
                                (const i32 c1)))
   (temps) (out)
   ((mov64rm (vreg x) 
             (addr (base (vreg t1)) (disp c1)))))

  ((store (mode i64) op1 (add (mode i32)
                                 (temp t1)
                                 (const i32 c1)))
   (temps) (out)
   ((mov64rm op1 (addr (base (vreg t1)) (disp c1)))))

  ((store (mode i64) op1 (label l1))
   (temps) (out)
   ((mov64rm op1 (addr (disp l1)))))

  ;; assign
  
  ((assign (temp x) op1)
   (temps) (out)
   ((mov64rr op1 (vreg x))))
  
  ;; add
  
  ((add (mode i64) op1 (const i8 x))
   (temps t5) (out t5)
   ((mov64rr op1 t5)
    (add64i8r (imm i8 x) t5)))

  ((add (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (add64i8r (imm i8 x) t5)))

  ((add (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (add64i32r (imm i32 x) t5)))
  
  ((add (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (add64i32r (imm i32 x) t5)))

  ((add (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (add64rr op1 t5)))

  ;; sub
  
  ((sub (mode i64) op1 (const i8 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (sub64i8r (imm i8 x) t5)))
  
  ((sub (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (sub64i8r (imm i8 x) t5)))

  ((sub (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (sub64i32r (imm i32 x) t5)))
  
  ((sub (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (sub64i32r (imm i32 x) t5)))

  ((sub (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (sub64rr op1 t5)))
  
  ;; and

  ((and (mode i64) op1 (const i8 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (and64i8r (imm i8 x) t5)))
  
  ((and (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (and64i8r (imm i8 x) t5)))

  ((and (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (and64i32r (imm i32 x) t5)))
  
  ((and (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (and64i32r (imm i32 x) t5)))

  ((and (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (and64rr op1 t5)))

  ;; or

  ((ior (mode i64) op1 (const i8 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (or64i8r (imm i8 x) t5)))
  
  ((ior (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (or64i8r (imm i8 x) t5)))

  ((ior (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (or64i32r (imm i32 x) t5)))
  
  ((ior (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (or64i32r (imm i32 x) t5)))

  ((ior (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (or64rr op1 t5)))

  ;; xor

  ((xor (mode i64) op1 (const i8 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (xor64i8r (imm i8 x) t5)))

  ((xor (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (xor64i8r (imm i8 x) t5)))

  ((xor (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov64rr op1 t5)
    (xor64i32r (imm i32 x) t5)))

  ((xor (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (xor64i32r (imm i32 x) t5)))

  ((xor (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov64rr op2 t5)
    (xor64rr op1 t5)))

  ;; shl
  
  ((shl (mode i64) (const i8 x) op2)
   (temps t5) (out t5)
   ((mov64rr op2 t5)
    (shl64i8r (imm i8 x) t5)))

  ;; shr
  
  ((shr (mode i64) (const i8 x) op2)
   (temps t5) (out t5)
   ((mov64rr op2 t5)
    (shr64i8r (imm i8 x) t5))))

