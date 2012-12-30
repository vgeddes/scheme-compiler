(declare (unit liveness)
         (uses machine helpers spec-x86-64))

(module liveness *

  (import scheme)
  (import chicken)
  (import extras)
  (import data-structures)

  (import machine (prefix machine mc-))
  (import helpers)

  (use matchable)
  (use srfi-1)
  (use srfi-69)

  (import spec-x86-64)

  (define *regs* '(rax rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

  (define-struct scan-context (mcxt ranges hreg-pool))
  (define-struct node         (index value pred succ in out def use live))
  (define-struct range        (vreg hreg pref start end))
  (define-struct pool         (hregs reg-names ranges))

  (define (make-count-generator)
    (let ((k 0))
      (lambda ()
        (let ((ret k))
          (set! k (+ k 1))
          ret))))

  (define (format-range range)
    `(range ,(mc-vreg-name (range-vreg range))
            ,(range-hreg  range)
            ,(range-pref  range)
            ,(range-start range)
            ,(range-end   range)))

  (define (format-step pool cur active rest)
    `(step ,(range-start  cur)
           (hregs-free  ,(pool-hregs pool))
           (current     ,(format-range cur))
           (active      ,@(map (lambda (range) (format-range range)) active))
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
             (($ mc-blk name head tail (succ* ...) cxt)
              (let* ((nodes (let f ((instr head) (nodes '()))
                              (cond
                               ((null? instr) (reverse nodes))
                               (else
                                (let ((number (counter)))
                                  (mc-inst-idx-set! instr number)
                                  (f (mc-inst-nxt instr)
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

    (walk (mc-cxt-strt cxt) (make-count-generator)))

  ;;
  ;; Sort the graph nodes into reverse post-order
  ;;
  ;; Given the graph, with each node numbered from 1 to 6:
  ;;
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
    (append (mc-inst-vregs-written (node-value node)) (mc-inst-id (node-value node))))

  ;; Get all vregs used at the given node
  (define (use-at node)
    (append (mc-inst-vregs-read (node-value node)) (mc-inst-iu (node-value node))))

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

  (define (range-make vreg hreg pref start end)
    (make-range vreg hreg pref start end))

  (define (range-fixed? r)
    (and (mc-vreg-hreg (range-vreg r)) #t))

  (define (range-pref r)
    (mc-vreg-pref (range-vreg r)))

  (define (range-fixed-hreg r)
    (mc-vreg-hreg (range-vreg r)))

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
  (define (range-overlap? r1 r2)
    (cond
     ((or (<= (range-end r1) (range-start r2)) (<= (range-end r2) (range-start r1)))
      #f)
     ;;   r1  0--5
     ;;   r2     3-6
     ((and (>= (range-start r2) (range-start r1)) (<= (range-start r2) (range-end r1)))
      #t)
     ;;   r1  0--5
     ;;   r2   1-3
     ((and (<= (range-start r1) (range-start r2)) (>= (range-end r1) (range-end r2)))
      #t)
     ;;   r1    1-3
     ;;   r2  0--5
     ((and (>= (range-start r1) (range-start r2)) (<= (range-start r1) (range-end r2)))
      #t)
     ;;   r1    1-5
     ;;   r2  0-3
     ((and (>= (range-start r1) (range-start r2)) (<= (range-start r1) (range-end r2)))
      #t)
     (else #f)))

  ;;
  ;; Compute live ranges for each vreg in the context
  ;;
  (define (compute-ranges cxt graph)

    (analyze-liveness graph)

    ;; update live ranges at node
    (define (update node)
      (for-each
       (lambda (vreg)
         (let ((range (mc-vreg-data vreg))
               (index (node-index node)))
           (cond
            ((null? range)
             (mc-vreg-data-set! vreg (range-make vreg (mc-vreg-hreg vreg) (mc-vreg-pref vreg) index index)))
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
    (for-each (lambda (vreg)
                (cond
                 ((null? (mc-vreg-data vreg))
                  (mc-vreg-data-set! vreg (range-make vreg (mc-vreg-hreg vreg) (mc-vreg-pref vreg) -1 -1)))))
              (mc-cxt-args cxt))

    ;; return all ranges

    (map (lambda (vreg)
       ;;    (pretty-print (list (mc-vreg-name vreg) (if (mc-vreg? (mc-vreg-hreg vreg))
         ;;                                              (mc-vreg-name (mc-vreg-hreg vreg))
           ;;                                            (mc-vreg-hreg vreg))
             ;;                  (if (null? (mc-vreg-data vreg)) #f (format-range (mc-vreg-data vreg)))))
           (mc-vreg-data vreg))
         (mc-cxt-vrgs cxt)))

  (define (pool-make hregs ranges)
    (let ((table  (make-hash-table eq? symbol-hash 24)))
      (let loop ((hreg* hregs))
        (match hreg*
               (() '())
               ((hreg . hreg*)
                (hash-table-set! table hreg '())
                (let loop-fx ((fx* ranges) (acc '()))
                  (match fx*
                         (() (hash-table-set! table hreg (reverse acc)))
                         ((fx . fx*)
                          (if (eq? hreg (range-hreg fx))
                              (loop-fx fx* (cons fx acc))
                              (loop-fx fx* acc)))))
                (loop hreg*))))
      (make-pool hregs hregs table)))

  (define (pool-empty? pool)
    (null? (pool-hregs pool)))

  (define (pool-reset pool)
    (pool-hregs-set! pool (pool-reg-names pool)))

  (define (pool-push pool hreg)
    (pool-hregs-set! pool (cons hreg (pool-hregs pool))))

  (define (pool-member? pool hreg)
    (and (memq hreg (pool-hregs pool)) #t))

  (define (pool-count pool)
    (length (pool-hregs pool)))

  (define (pool-remove pool hreg)
    (pool-hregs-set! pool (lset-difference eq? (pool-hregs pool) (list hreg)))
    hreg)

  ;;
  ;; Allocate an hreg to a range, taking preferences into account
  ;;
  (define (hreg-alloc pool range)
    (if (null? (pool-hregs pool))
        #f
        (let ((pref (range-pref range)))
          (cond
           ((and pref (pool-member? pool pref) (can-alloc? pool range pref))
            (pool-remove pool pref))
           (else
            (let loop ((hreg* (pool-hregs pool)))
              (match hreg*
                (() #f)
                ((hreg . hreg*)
                 (if (can-alloc? pool range hreg)
                     (pool-remove pool hreg)
                     (loop hreg*))))))))))

  ;;
  ;; Check whether the given range overlaps with any of a hreg's fixed ranges
  ;;
  (define (can-alloc? pool range hreg)
    (let loop ((fxr* (hash-table-ref (pool-ranges pool) hreg)))
      (match fxr*
             (() #t)
             ((fxr . fxr*)
              (if (range-overlap? fxr range)
                  #f
                  (loop fxr*))))))


  ;; get all fixed ranges
  (define (fixed-ranges ranges)
    (fold (lambda (range acc)
            (if (range-hreg range)
                (cons range acc)
                acc))
          '()
          ranges))

  ;; get all unconstrained ranges
  (define (free-ranges ranges)
    (fold (lambda (range acc)
            (if (range-hreg range)
                acc
                (cons range acc)))
          '()
          ranges))

  ;; Expire active ranges which end before the given range starts
  ;;
  ;; returns active ranges that have not yet expired
  ;;
  (define (expire-active pool cur active)
    (let loop ((ac* active))
      (match ac*
             (() '())
             ((ac . rest*)
              (cond
               ((< (range-end ac) (range-start cur))
                (pool-push pool (range-hreg ac))
                (loop rest*))
               (else ac*))))))

  ;; TODO optimize
  (define (update-active active range)
    (sort (cons range active) range-ends-before?))

  ;; remove range
  (define (remove-range vreg ranges)
    (let f ((range* ranges) (x '()))
      (match range*
             (() (sort x range-starts-before?))
             ((range . range*)
              (cond
               ((mc-vreg-equal? (range-vreg range) vreg)
                (f range* x))
               (else
                (range-hreg-set! range #f)
                (f range* (cons range x))))))))

  ;; For our spilling heuristic, we select the longest range in active
  (define (select-range-to-spill active)
    (if (null? active)
        #f
        (car (sort active
                   (lambda (r1 r2)
                     (>= (- (range-end r1) (range-start r1)) (- (range-end r2) (range-start r2))))))))

  (define (add-spill spills index cxt instr vreg)
    (let ((tmp (mc-cxt-alloc-vreg cxt (gensym 'g))))
      ;; replace vreg use with another tmp, which will represent a scratch register. The vreg contents are now
      ;; passed between/from the stack and the scratch register
      ;; add spill info for the user
      (cond
       ((and (mc-inst-is-read? instr vreg) (mc-inst-is-written? instr vreg))
        (hash-table-set! spills (mc-inst-idx instr) (list 'read-write instr index tmp)))
       ((mc-inst-is-read? instr vreg)
        (hash-table-set! spills (mc-inst-idx instr) (list 'read instr index tmp)))
       ((mc-inst-is-written? instr vreg)
        (hash-table-set! spills (mc-inst-idx instr) (list  'write instr index tmp)))
       (else (assert-not-reached)))
      ;; replace vreg with tmp in instruction
      (mc-inst-replace-vreg instr vreg tmp)
      ;; create a range for the scratch register
      (make-range tmp #f #f (mc-inst-idx instr) (mc-inst-idx instr))))

  (define (spill spills index cxt ranges vreg)
    (let loop ((user* (mc-vreg-usrs vreg)) (ranges ranges))
      (match user*
             (() (remove-range vreg ranges))
             ((user . user*)
              (let ((tmp (add-spill spills index cxt user vreg)))
                (loop user* (cons tmp ranges)))))))

  ;; Spilling heuristics:
  ;; if pool is empty AND active non-empty, select longest range in Active
  ;; if pool is empty select cur
  ;; if pool is non-empty and hreg-alloc fails, then spill cur (indicates that cur is a callee-save var)

  (define (iterate pool ranges)
    (let loop ((re*  ranges)
               (ac  '()))
      (match re*
             ;; return (#t) to indicate allocation success
             (() (list #t))
             ;; handle current range
             ((cur . re*)
              (pretty-print (format-step pool cur ac re*))
              (let ((ac (expire-active pool cur ac)))
                ;; Backtrack if a spill is required
                ;; return (#f vreg) to indicate allocation failure
                (cond
                 ((and (null? (pool-hregs pool)) (null? ac))
                  (print 'a)
                  (list #f (range-vreg cur)))
                 ((and (null? (pool-hregs pool)) (not (null? ac)))
                  (print 'b)
                  (list #f (range-vreg (select-range-to-spill ac))))
                 (else
                  (let ((hreg (hreg-alloc pool cur)))
                    (if (not hreg)
                        (list #f (range-vreg cur))
                        (begin
                          (range-hreg-set! cur hreg)
                          (loop re* (update-active ac cur))))))))))))

  (define (scan cxt pool spills sp-index-gen ranges)
    ;; enter scanning loop
    (let loop ((ranges (sort ranges range-starts-before?)))
      (pool-reset pool)
      (match (iterate pool ranges)
             ((#f vreg)
              ;; Restart the scan after handling the spill
              (pretty-print (list 'spill (mc-vreg-name vreg)))
              (loop (spill spills (sp-index-gen) cxt ranges vreg)))
             ((#t)
              ;; update vregs to reflect the final register assignments
              (for-each (lambda (range)
                          (mc-vreg-hreg-set! (range-vreg range) (range-hreg range)))
                        ranges)
              ))))

  (define (rewrite-spills cxt spills)
    (let ((rbp (mc-cxt-alloc-vreg cxt 'rbp 'rbp #f)))
      (hash-table-for-each spills
                           (lambda (k v)
                             (match v
                                    (('read-write instr index vreg)
                                     (mc-blk-insert (mc-inst-blk instr) instr 'before
                                                           (x86-64.mov.mdr #f '() '() rbp (mc-disp-make (* 8 index)) vreg))
                                     (mc-blk-insert  (mc-inst-blk instr) instr 'after
                                                     (x86-64.mov.rmd #f '() '() vreg rbp (mc-disp-make (* 8 index)))))
                                    (('read instr index vreg)
                                     (mc-blk-insert (mc-inst-blk instr) instr 'before
                                                           (x86-64.mov.mdr #f '() '() rbp (mc-disp-make (* 8 index)) vreg)))
                                    (('write instr index vreg)
                                     (mc-blk-insert (mc-inst-blk instr) instr 'after
                                                    (x86-64.mov.rmd #f '() '() vreg rbp (mc-disp-make (* 8 index))))))))))

  (define (alloc-registers-pass cxt regs)
    (print "*** alloc-regs-pass ***\n")

    (let* ((ranges    (compute-ranges cxt (build-graph cxt)))
           (pool      (pool-make regs (fixed-ranges ranges)))
           (spills    (make-hash-table = number-hash 20))
           (index-gen (make-count-generator)))

      (mc-cxt-print cxt (current-output-port))
      ;; enter scanning loop
      (scan cxt pool spills index-gen (free-ranges ranges))


      (rewrite-spills cxt spills)

      (pretty-print
       `(assignments ,(map (lambda (vreg)
                             `(,(mc-vreg-name vreg) ,(mc-vreg-hreg vreg)))
                           (mc-cxt-vrgs cxt))))))


  (define (alloc-regs mod)
    (mc-cxt-for-each
     (lambda (cxt)
       (alloc-registers-pass cxt *regs*))
     mod)
    mod)

  (define (alloc-regs-test mod regs)
    (mc-cxt-for-each
     (lambda (cxt)
       (alloc-registers-pass cxt regs))
     mod)
    mod)

)
