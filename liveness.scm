
(declare (unit liveness)
         (uses nodes machine utils))

(use matchable)
(use srfi-1)

(include "struct-syntax")
(include "munch-syntax")

(define assert-not-reached
  (lambda ()
    (error 'assert-not-reached)))

(define-struct scan-context (mcxt ranges hreg-pool))
(define-struct node         (index value pred succ in out def use live))
(define-struct range        (vreg hreg pref start end))

(define (format-range range)
  `(range ,(mc-vreg-name (range-vreg range))
          ,(range-hreg  range)
          ,(range-start range)
          ,(range-end   range)))

(define (format-step hregs-free current active fixed rest)
  `(step ,(range-start  step)
          (hregs-free  ,hregs-unused)
          (current     ,(format-range current))
          (active      ,@(map (lambda (range) (format-range range)) active))
          (fixed       ,@(map (lambda (range) (format-range range)) fixed))
          (rest        ,@(map (lambda (range) (format-range range)) rest))))

;;
;; Build a control-flow DAG for the given context. The graph is used for live variable analysis.
;;
;; The graph abstracts away from basic blocks. Each node represents an individual instruction.
;; Multiple outgoing edges on a node indicate a branching decision.
;;  
(define (build-graph cxt)

  (define (walk block counter)
    (match block
      (($ mc-block name head tail (succ* ...) cxt)
       (let* ((nodes (let f ((instr head) (nodes '()))
                       (cond
                         ((null? instr) (reverse nodes))
                         (else
                          (let ((number (counter)))
                            (mc-instr-number-set! instr number)
                            (f (mc-instr-next instr)
                               (cons (make-node number instr '() '() '() '() '() '() '()) nodes)))))))
              (head (car nodes))
              (tail (car (reverse nodes)))
              (succ (map (lambda (succ)
                           (walk succ counter))
                     succ*)))
         
          ;; set next/prev pointers
         (let f ((cur (car nodes)) (node* (cdr nodes)))
           (match node*
             (() cur)
             ((node . node*)
              (node-succ-set! cur  (list node))
              (node-pred-set! node (list cur))
              (f node node*))))

         (node-succ-set! tail succ)

         ;; make successors point back to tail
         (for-each (lambda (node)
                     (node-pred-set! node (list tail)))
                   succ)
         head))))

     (walk (mc-context-start cxt) (make-count-generator)))

;;
;; Sort the graph nodes using a reverse pre-ordering
;;
;; Given the graph, with each node numbered from 1 to 6:
;;   1 --> 2 --> 3 --> 4
;;         \
;;           --> 5 --> 6
;;
;; The result is: 6 5 4 3 2 1
;;
(define (sort-reverse-pre-order graph)
  (define (walk node)
    (cons node (apply append (map walk (node-succ node)))))
  (reverse (walk graph)))

;; Get all vregs defined at the given node
(define (def-at node)
  (mc-instr-vregs-written (node-value node)))

;; Get all vregs used at the given node
(define (use-at node)
  (append (mc-instr-vregs-read (node-value node)) (mc-instr-implicit-uses (node-value node))))

;; Perform iterative liveness analysis on the graph
;;
;; We define the following sets for each node:
;;   def: Set of vregs defined at this node
;;   use: Set of vregs used at this node
;;   in:  Set of vregs that are live just before this node
;;   out: Set of vregs that are live just after this node
;;
;; The analysis takes place on a reverse pre-ordering of the graph nodes. 
;; (i.e from the last node to the first node)
;;
(define (analyze-liveness graph)
  (let ((node* (sort-reverse-pre-order graph)))
    ;; initialize def/use for each node
    (for-each
      (lambda (node)
        (node-def-set! node (def-at node))
        (node-use-set! node (use-at node)))
      node*)
    ;; iterate over nodes (backwards) to propagate uses.
    (for-each
      (lambda (node)
        ;; node[i].out = node[i+1].in
        (node-out-set! node
          (fold (lambda (succ acc)
                  (append (node-in succ) acc))
               '()
                (node-succ node)))
        ;; node[i].in = node[i].use UNION (node[i].out MINUS node[i].def)
        (node-in-set! node
          (lset-union mc-vreg-equal?
            (node-use node)
            (lset-difference mc-vreg-equal?
              (node-out node)
              (node-def node)))))
       node*)
   ;; final set of live variables at each point is (node[i].in UNION node[i].def)
   (for-each
     (lambda (node)
       (node-live-set! node
         (lset-union mc-vreg-equal? (node-in node) (node-def node))))
     node*)
   graph))

;; Determines whether live range r1 starts before r2
;;
(define (range-starts-before? r1 r2)
  (< (range-start r1) (range-start r2)))

;; Determines whether live range r1 ends before r2
;;
(define (range-ends-before? r1 r2)
  (< (range-end r1) (range-end r2)))

;; Determines whether r1 and r2 overlap
;; TODO: we can surely remove redundant tests here
(define (ranges-overlap? r1 r2)
  (cond
    ((or (<= (range-end r1) (range-start r2)) (<= (range-end r2) (range-start r1)))
     #f)
    ;;   r1  0----5
    ;;   r2     3--6
    ((and (>= (range-start r2) (range-start r1)) (<= (range-start r2) (range-end r1)))
     #t)
    ;;   r1  0----5
    ;;   r2   1-3
    ((and (<= (range-start r1) (range-start r2)) (>= (range-end r1) (range-end r2)))
     #t)
    ;;   r1    1-3
    ;;   r2  0----5 
    ((and (>= (range-start r1) (range-start r2)) (<= (range-start r1) (range-end r2)))
     #t)
    ;;   r1    1--5
    ;;   r2  0--3
    ((and (>= (range-start r1) (range-start r2)) (<= (range-start r1) (range-end r2)))
     #t)
    (else #f)))

;;
;; Compute live ranges for each vreg in the context
;;
(define (compute-live-ranges cxt graph)

   (analyze-liveness graph)

  ;; update live ranges at node
  (define (update node)
    (for-each
      (lambda (vreg)
        (let ((range (mc-vreg-data vreg))
              (index (node-index node)))
          (cond
            ((null? range)
             (mc-vreg-data-set! vreg (make-range vreg #f (mc-vreg-constraint vreg) index index)))
            (else
             (range-end-set! range index)))))
      (node-live node)))
 
  (let walk ((node graph))
     (match node
       (() '())
       (_
        (update node)
        (for-each (lambda (succ)
                    (walk succ))
                  (node-succ node)))))

  ;; make dummy ranges for unused variables
  (for-each (lambda (arg)
              (cond
                ((null? (mc-vreg-data vreg))
                 (mc-vreg-data-set! vreg (make-range vreg #f (mc-vreg-constraint vreg) -1 -1)))))
    (mc-cxt-args cxt))

  ;; return all ranges
  (map (lambda (vreg) (mc-vreg-data vreg)) (mc-cxt-vregs cxt)))


;; TODO: abstract over arch specifics
(define *regs-default* '(rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

(define (regs-reset)
  (set! *regs* *all-regs*))

(define (regs-free?)
  (not (null? *regs*)))

(define (reg-free reg)
  (set! *regs* (cons reg *regs*)))

(define (reg-alloc reserved)
  (cond
   ((null? *regs*)
    (assert-not-reached))
   (else
  
                        

    (let ((reg (find (lambda (reg)
                       (not (find (lambda (ignore)
                                    (eq? reg ignore))
                                  ignores)))
                     *regs*)))
      (set! *regs* (lset-difference eq? *regs* (list reg)))
      reg))))

(define (regs-remove name)
  (cond
   ((null? *regs*) (assert-not-reached))
   ((memq name *regs*)
    (set! *regs* (lset-difference eq? *regs* (list name)))
    name)
   (else (assert-not-reached))))

;; Expire constrained ranges which end before the given range starts
;;
;; returns constrained ranges which have not yet expired
;;
(define (expire-constrained range constrained)
  (let f ((c* constrained) (acc '()))
    (match c*
      (() (reverse acc))
      ((c . c*)
       (cond
         ((< (range-end c) (range-start range))
          (f c* acc))
         (else
          (f c* (cons c acc))))))))

;; Expire active ranges which end before the given range starts
;;
;; returns active ranges that have not yet expired
;;
(define (expire range active) 
  (let f ((a* active) (acc '()))
    (match a*
      (() acc)
      ((a . a*)
       (cond
         ((< (range-end a) (range-start range))
          (regs-push (range-hreg a))
          (f a* acc))
         (else
          (f a* (cons a acc))))))))

(define (assign-register bt range active constrained)
  (let ((hreg (range-constraint range)))
    ;; check to see if this range is constrained to a particular register
    (cond
     ((or (eq? hreg 'rbp) (eq? hreg 'rsp))
      ;; rbp and rsp do not take part in the linear scan alogorithm, so we freely assign them to constrained ranges.
      (range-hreg-set! range hreg))
     ((not hreg)
      (let* ((not-use (let loop ((cr* constrained) (x '()))
                         (match cr*
                           (() x)
                           ((cr . cr*)
                            (cond
                              ((> (range-start cr) (range-end range))
                               (loop '() x))
                              ((ranges-overlap? range cr)
                               (loop cr* (lset-union eq? (list (range-constraint cr)) x)))
                              (else (loop cr* x)))))))
             (assigned (regs-pop not-use)))
      ;; an unconstrained range; pop a free hreg off the stack of available hregs
      (range-hreg-set! range assigned)))
     (else
      (range-hreg-set! range (regs-remove hreg))))))

(define (spill cxt ranges index vreg)
  (let loop ((user* (mc-vreg-users vreg)) (ranges ranges))
    (match user*
      (()
       (let f ((range* ranges) (x '()))
         (match range*
           (() (sort x range-compare-start))
           ((range . range*)
            (cond
             ((mc-vreg-equal? (range-vreg range) vreg)
              (f range* x))
             (else
              (range-hreg-set! range #f)
              (f range* (cons range x))))))))
      ((user . user*)  
       (let ((tmp (mc-context-allocate-vreg cxt (gensym 't))))
         ;; replace vreg use with another tmp. The vreg contents are now passed between/from the stack and the tmp 
         (mc-instr-replace-vreg user vreg tmp)
         (cond
           ((mc-instr-is-read? user vreg)
            (mc-instr-sp-load-set! user (list index tmp))))
         (cond
           ((mc-instr-is-written? vreg)
            (mc-instr-sp-store-set! user (list index tmp))))
         (let ((tmp-range (make-range tmp #f #f (mc-instr-number user) (mc-instr-number user))))
               (loop user* (cons tmp-range ranges))))))))

(define (iterate ranges)
  (call/cc
    (lambda (backtrack)
      (let loop ((ranges ranges)
                 (active '())
                 (constrained (sort 
                   (fold (lambda (r x)
                           (if (range-constraint r)
                               (cons r x)
                               x))
                        '()
                         ranges) range-compare-start)))
        (if (null? ranges)
            ;; return (#t) to indicate allocation success
            (list #t)
            (let* ((next         (car ranges))
                   (active       (expire next active))
                   (constrained  (expire-constrained next constrained)))
              (pretty-print (format-iteration (range-start next) *regs* next active constrained (cdr ranges)))
              ;; Backtrack if a spill is required
              ;; return (#f vreg) to indicate allocation failure
              (cond
                ((not (regs-available?))
                 (backtrack (list 'spill (range-vreg next)))))        
              ;; Assign a register
              (assign-register backtrack next active constrained)
              ;; loop
              (let ((active-sorted      (sort (cons next active) range-compare-end))
                    (constrained-sorted (sort constrained range-compare-start))) 
                (loop (cdr ranges) active-sorted constrained-sorted))))))))



(define (scan-context-make mcxt ranges)
  (let ((scxt (make-scan-context mcxt ranges '() *hregs-default)))
    scxt))

(define (scan cxt ranges)

  (define (update-vregs ranges)
    (for-each
      (lambda (range)
        (mc-vreg-hreg-set! (range-vreg range) (range-hreg range)))
      ranges))

  ;; enter scanning loop
  (let loop ((ranges (sort ranges range-compare-start)))
    (regs-reset)
    (print (length ranges))
    (match (iterate ranges)
      ((#f vreg)
       ;; Restart the scan after handling the spill
       (loop (spill cxt ranges (spill-index-gen) vreg)))
      ((#t)
       ;; update vregs to reflect the final register assignments
       (update-vregs ranges)

      (pretty-print  
         `(assignments ,(map (lambda (vreg)
              `(,(mc-vreg-name vreg) ,(mc-vreg-hreg vreg)))
            (mc-context-vregs cxt))))))))

(define (allocate-registers-pass cxt)
  (let* ((ranges (compute-live-ranges cxt (build-graph cxt))))
    (scan cxt ranges)))

(define (allocate-registers mod)
  (mc-context-for-each
    (lambda (cxt)
      (allocate-registers-pass cxt))
    mod)
  mod)



