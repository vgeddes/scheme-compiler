(declare (unit rules-x86-64)
         (uses tree))

(include "munch-syntax")
(include "arch-syntax")

(define-munch-rules x86-64
  
  ;; branch to label
  
  ((br (label x))
   (temps) (out)
   ((jmp64.d (disp x))))

  ;; branch indirect

  ((br (temp x))
   (temps) (out)
   ((jmp64.r (vreg x))))

  ;; branch if true
  
  ((brc op1 (label tl) (label fl))
   (temps) (out)
   ((cmp.i8r (imm i8 0) op1)
    (jne.d   disp tl)))

  ;; branch if >
  
  ((brc (cmp (mode i8) (op gt) (const i8 x) op2)
        (label tl) (label fl))
   (temps) (out)
   ((cmp.i8r  (imm i8 x) op2)
    (jg.d     (disp tl))))

  ((brc (cmp (mode i64) (op gt) op1 op2)
        (label tl) (label fl))
   (temps) (out)
   ((cmp.rr   op1 op2)
    (jg.d     (disp tl))))

  ;; branch if >=
  
  ((brc (cmp (mode i8) (op ge) (const i8 x) op2) (label tl) (label fl))
   (temps) (out)
   ((cmp.i8r  (imm i8 x) op2)
    (jge.d    (disp tl))))

  ((brc (cmp (mode i32) (op ge) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp.rr    op1 op2)
    (jge.d     (disp tl))))

  ;; branch if == 
  
  ((brc (cmp (mode i64) (op eq) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp.rr   op1 op2)
    (je.d     (disp tl))))

  ;; branch if < 

  ((brc (cmp (mode i64) (op lt) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp.rr   op1 op2)
    (jl.d     (disp tl))))

  ;; branch if <= 

  ((brc (cmp (mode i64) (op le) op1 op2) (label tl) (label fl))
   (temps) (out)
   ((cmp.rr   op1 op2)
    (jle.d    (disp tl))))
  
  ;; compare <= 

  ((cmp (mode i64) (op le) op1 op2)
   (temps t1) (out t1)
   ((xor.rr  t1  t1)
    (cmp.rr  op1 op2)
    (setle.r t1)))

  ;; compare <

  ((cmp (mode i64) (op lt) op1 op2)
   (temps t1) (out t1)
   ((xor.rr  t1 t1)
    (cmp.rr  op1 op2)
    (setl.r  t1)))
  
  ;; compare == 

  ((cmp (mode i64) (op eq) op1 op2)
   (temps t1) (out t1)
   ((xor.rr  t1 t1)
    (cmp.rr  op1 op2)
    (sete.r  t1)))
  
  ;; compare >

  ((cmp (mode i64) (op gt) op1 op2)
   (temps t1) (out t1)
   ((xor.rr  t1 t1)
    (cmp.rr  op1 op2)
    (setg.r  t1)))

  ;; compare >=

  ((cmp (mode i64) (op ge) op1 op2)
   (temps t1) (out t1)
   ((xor.rr  t1 t1)
    (cmp.rr  op1 op2)
    (setge.r t1)))
  
  ;; load effective address

  ((load (mode ptr64) (label l1))
   (temps t1) (out t1)
   ((lea.mr (disp l1) t1)))

  ((assign (temp x) (load (mode ptr64) (label l1)))
   (temps) (out)
   ((lea.mr (disp l1) (vreg x))))
  
  ;; load immediate
  
  ((const i8 x)
   (temps t1) (out t1)
   ((xor.rr  t1 t1)
    (mov64i8r (imm i8 x) t1)))
  
  ((const i32 x)
   (temps t1) (out t1)
   ((xor.rr   t1 t1)
    (mov64i32r (imm i32 x) t1)))

  ((const i64 x)
   (temps t1) (out t1)
   ((mov64i64r (imm i64 x) t1)))

  
  ;; memory load

  ((assign (temp x)
     (load (mode i64) (temp t1)))
   (temps) (out)
   ((mov.mr (vreg t1) (vreg x))))

  ((assign (temp x)
     (load (mode i64) (add (mode i32)
                        (temp t1)
                        (const i32 c1))))
   (temps) (out)
   ((mov.mdr (vreg t1) (disp c1) (vreg x))))

  ((assign (temp x) (load (mode i64) (label l1)))
   (temps) (out)
   ((mov.dr (disp l1) (vreg x))))

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
   ((mov.rr op1 (vreg x))))
  
  ;; add
  
  ((add (mode i64) op1 (const i8 x))
   (temps t5) (out t5)
   ((mov.rr op1 t5)
    (add.i8r (imm i8 x) t5)))

  ((add (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (add.i8r (imm i8 x) t5)))

  ((add (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov.rr op1 t5)
    (add.i32r (imm i32 x) t5)))
  
  ((add (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (add.i32r (imm i32 x) t5)))

  ((add (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (add.rr op1 t5)))

  ;; sub
  
  ((sub (mode i64) op1 (const i8 x))
   (temps t5) (out t5) 
   ((mov.rr op1 t5)
    (sub.i8r (imm i8 x) t5)))
  
  ((sub (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (sub.i8r (imm i8 x) t5)))

  ((sub (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov.rr op1 t5)
    (sub.i32r (imm i32 x) t5)))
  
  ((sub (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (sub.i32r (imm i32 x) t5)))

  ((sub (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (sub.rr op1 t5)))
  
  ;; and

  ((and (mode i64) op1 (const i8 x))
   (temps t5) (out t5) 
   ((mov.rr op1 t5)
    (and.i8r (imm i8 x) t5)))
  
  ((and (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (and.i8r (imm i8 x) t5)))

  ((and (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov.rr op1 t5)
    (and.i32r (imm i32 x) t5)))
  
  ((and (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (and.i32r (imm i32 x) t5)))

  ((and (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (and.rr op1 t5)))

  ;; or

  ((ior (mode i64) op1 (const i8 x))
   (temps t5) (out t5) 
   ((mov.rr op1 t5)
    (or.i8r (imm i8 x) t5)))
  
  ((ior (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (or.i8r (imm i8 x) t5)))

  ((ior (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov.rr op1 t5)
    (or.i32r (imm i32 x) t5)))
  
  ((ior (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (or.i32r (imm i32 x) t5)))

  ((ior (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (or.rr op1 t5)))

  ;; xor

  ((xor (mode i64) op1 (const i8 x))
   (temps t5) (out t5) 
   ((mov.rr op1 t5)
    (xor.i8r (imm i8 x) t5)))

  ((xor (mode i64) (const i8 x) op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (xor.i8r (imm i8 x) t5)))

  ((xor (mode i64) op1 (const i32 x))
   (temps t5) (out t5) 
   ((mov.rr op1 t5)
    (xor.i32r (imm i32 x) t5)))

  ((xor (mode i64) (const i32 x) op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (xor.i32r (imm i32 x) t5)))

  ((xor (mode i64) op1 op2)
   (temps t5) (out t5) 
   ((mov.rr op2 t5)
    (xor.rr op1 t5)))

  ;; shl
  
  ((shl (mode i64) (const i8 x) op2)
   (temps t5) (out t5)
   ((mov.rr op2 t5)
    (shl.i8r (imm i8 x) t5)))

  ;; shr
  
  ((shr (mode i64) (const i8 x) op2)
   (temps t5) (out t5)
   ((mov.rr op2 t5)
    (shr.i8r (imm i8 x) t5))))

