(declare (unit liveness)
         (uses machine helpers globals spec-x86-64 arch))

(module liveness *

  (import scheme)
  (import chicken)
  (import extras)
  (import data-structures)

  (import arch)
  (import machine (prefix machine mc-))
  (import helpers)

  (use matchable)
  (use srfi-1)
  (use srfi-69)
  (use iset)

  (import spec-x86-64)
  (import arch-syntax)
  (import globals)

  (define-struct scan-context (mcxt ranges hreg-pool))
  (define-struct node         (index value pred succ in out *in *out def use))
  (define-struct range        (vreg start end))
  (define-struct pool         (hregs fixed))

  (define (format-range range)
    `(range ,(string->symbol (arch-operand-format (range-vreg range)))
            ,(range-start range)
            ,(range-end   range)))

  (define (format-step pool cur active rest)
    `(step ,(range-start  cur)
           (hregs-free  ,(pool-hregs pool))
           (current     ,(format-range cur))
           (active      ,@(map (lambda (range) (format-range range)) active))
           (rest        ,@(map (lambda (range) (format-range range)) rest))))

  (eval-when (load compile)
             (define (make-count-generator)
               (let ((k 0))
                 (lambda ()
                   (let ((ret k))
                     (set! k (+ k 1))
                     ret)))))

  ;;
  ;; Build a control-flow DAG for the given context.
  ;;
  ;; The graph abstracts away from basic blocks. Each node represents an individual instruction.
  ;; Multiple outgoing edges on a node indicate a branching decision.
  ;;
  (define (build-graph cxt)
    (define (add-dummy graph cxt)
      (let* ((head   (make-node 0 #f '() (list graph) '() '() '() '() '() '())))
        (node-pred-set! graph (list head))
        head))
    (define (walk block counter)
      (match block
        (($ mc-blk name head tail (succ* ...) cxt)
         (let* ((nodes (let f ((instr head) (nodes '()))
                         (cond
                          ((null? instr) (reverse nodes))
                          (else
                           (let ((number (+ 1 (counter))))
                             (mc-inst-idx-set! instr number)
                             (f (mc-inst-nxt instr)
                                (cons (make-node number instr '() '() '() '() '() '() '() '()) nodes)))))))
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
    (let ((graph (walk (mc-cxt-strt cxt)
                       (make-count-generator))))
      (add-dummy graph cxt)))

  (define (range-make vreg hreg start end)
    (make-range vreg hreg start end))

  (define (range-fixed? r)
    (and (mc-vreg-hreg (range-vreg r)) #t))

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
     ((or (< (range-end r1) (range-start r2)) (< (range-end r2) (range-start r1)))
      #f)
     ;;   r1  0--5
     ;;   r2     3-6
     ((and (> (range-start r2) (range-start r1)) (< (range-start r2) (range-end r1)))
      #t)
     ;;   r1  0--5
     ;;   r2   1-3
     ((and (< (range-start r1) (range-start r2)) (> (range-end r1) (range-end r2)))
      #t)
     ;;   r1    1-3
     ;;   r2  0--5
     ((and (> (range-start r1) (range-start r2)) (< (range-start r1) (range-end r2)))
      #t)
     ;;   r1    1-5
     ;;   r2  0-3
     ((and (> (range-start r1) (range-start r2)) (< (range-start r1) (range-end r2)))
      #t)
     (else #f)))

  (define (range-home rg)
    (mc-vreg-slot (range-vreg rg)))

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
  (define (sort-reverse-post-order graph)
    (define (walk node)
      (cons node (apply append (map walk (node-succ node)))))
    (reverse (walk graph)))

  (define (init-bitset bs vregs)
    (let loop ((vregs vregs))
      (if (null? vregs)
          bs
          (begin
            (bit-vector-set! bs (bs-index (car vregs)) #t)
            (loop (cdr vregs))))))

  (define (defs-at inst)
    (append (mc-vreg-defs inst)
            (or (mc-inst-attr inst 'defs)
                '())))

  (define (uses-at inst)
    (append (mc-vreg-uses inst)
            (or (mc-inst-attr inst 'uses)
                '())))

  (define (bs-index vreg)
    (if (mc-vreg-hreg vreg)
        (- (mc-vreg-id vreg) *hreg-start-id*)
        (+ (vector-length *hreg-table*) (mc-vreg-id vreg))))

  (define (make-bitset cxt)
    (let ((size (+ (vector-length *hreg-table*)
                   (mc-cxt-vrgs-count cxt))))
      (make-bit-vector size)))

  (define (init-bitsets cxt node)
    (let* ((ubs  (make-bitset cxt))
           (dbs  (make-bitset cxt))
           (ibs  (make-bitset cxt))
           (obs  (make-bitset cxt)))
      (cond
       ((= (node-index node) 0)
        (init-bitset dbs (mc-cxt-hreg-params cxt)))
       (else
        (init-bitset dbs (defs-at (node-value node)))
        (init-bitset ubs (uses-at (node-value node)))))
      (node-def-set! node dbs)
      (node-use-set! node ubs)
      (node-in-set!  node ibs)
      (node-out-set! node obs)))

  ;; Perform live-variable analysis on the graph using a
  ;; worklist based approach
  ;;
  ;; We define the following sets for each node:
  ;;   def: Set of vregs defined at this node
  ;;   use: Set of vregs used at this node
  ;;   in:  Set of vregs that are live-in at this node
  ;;   out: Set of vregs that are live-out at this node
  ;;
  ;; The analysis takes place on a reverse post-ordering of the graph nodes.
  ;; (i.e from the last node to the first node)
  ;;
  (define (analyze-liveness cxt cp nodes)
    (let loop ()
      (let loop ((nodes nodes))
        (cond
         ((not (null? nodes))
          (let* ((node  (car nodes)))

            (node-*in-set!  node (node-in  node))
            (node-*out-set! node (node-out node))

            ;; (pretty-print (list (node-index node)
            ;;                     (list 'def  (map (lambda (v) (arch-operand-format v))
            ;;                                      (cp (node-def node))))
            ;;                     (list 'use  (map (lambda (v) (arch-operand-format v))
            ;;                                      (cp (node-use node))))))

            ;; node.out = UNION successors(node).in
            (for-each (lambda (succ)
                        (bit-vector-ior! (node-out node) (node-in succ)))
                      (node-succ node))


            ;; node[i].in = node[i].use UNION (node[i].out - node[i].def)
            (node-in-set! node
                          (bit-vector-ior
                           (node-use node)
                           (bit-vector-and
                            (node-out node)
                            (bit-vector-xor
                             (node-out node)
                             (node-def node)))))

            (loop (cdr nodes))))))

      (for-each (lambda (node)
                  (if (not (and (equal? (node-in node)
                                        (node-*in node))
                                (equal? (node-out node)
                                        (node-*out node))))
                      (loop)))
                nodes)))

  (define (create-lookup cxt)
    (let ((tbl (make-vector (+ (vector-length *hreg-table*)
                               (mc-cxt-vrgs-count cxt)))))
      (let loop ((i 0))
        (cond
         ((< i (vector-length *hreg-table*))
          (vector-set! tbl
                       i
                       (vector-ref *hreg-table* i))
          (loop (+ 1 i)))))
      (let loop ((vregs (reverse (mc-cxt-vrgs cxt))) (i 0))
        (cond
         ((not (null? vregs))
          (vector-set! tbl
                       (+ i
                          (vector-length *hreg-table*))
                       (car vregs))
          (loop (cdr vregs) (+ 1 i)))))
      (lambda (bs)
        (let loop ((i 0) (len (vector-length tbl)) (x '()))
          (cond
           ((< i len)
            (if (bit-vector-ref bs i)
                (loop (+ 1 i) len (cons (vector-ref tbl i) x))
                (loop (+ 1 i) len x)))
           (else x))))))

  ;; Create a dummy node to represent definitions of
  ;; function parameters

  (define (add-range rgs vreg start end)
    (cond
     ((mc-vreg-hreg vreg)
      (let ((ls (hash-table-ref/default rgs vreg '())))
        (cond
         ((null? ls)
          (hash-table-set! rgs vreg (list (cons start end))))
         (else
          (let ((rg (car ls)))
            (if (= end (cdr rg))
                (set-car! rg start)
                (hash-table-set! rgs vreg
                                 (cons (cons start end) ls))))))))
     (else
      (let ((range (hash-table-ref/default rgs vreg '())))
        (if (null? range)
            (hash-table-set! rgs vreg
                             (cons start end))
            (set-car! range start))))))

    (define (build-ranges ranges)
      (let ((fixed '())
            (free  '()))
        (hash-table-for-each
          ranges
          (lambda (k v)
            (if (mc-vreg-hreg k)
                (set! fixed (cons (cons k
                                        (map (lambda (sr)
                                               (make-range
                                                 k
                                                 (car sr)
                                                 (cdr sr)))
                                               v))
                                  fixed))
                (set! free
                      (cons
                       (make-range
                        k
                        (car v)
                        (cdr v))
                       free)))))
        (cons free fixed)))

  ;;
  ;; Compute live ranges for each vreg in the context
  ;;
  (define (compute-ranges cxt graph)
    (let* ((node*      (sort-reverse-post-order graph))
           (cp         (create-lookup cxt))
           (actv       (make-hash-table
                         mc-vreg-equal?
                         (lambda (k bound)
                           (number-hash (mc-vreg-id k) bound))
                         32))
           (rgs        (make-hash-table
                          mc-vreg-equal?
                          (lambda (k bound)
                            (number-hash (mc-vreg-id k) bound))
                          32)))

      ;; initialize bitsets at each node
      (map (lambda (node)
             (init-bitsets cxt node))
           node*)

      (analyze-liveness cxt cp node*)

      ;; (define (walk node)
      ;;   (let ((k    (node-index node))
      ;;         (def  (cp (node-def node)))
      ;;         (use  (cp (node-use node))))

      ;;    (pretty-print (list k
      ;;                        (map (lambda (vr) (arch-operand-format vr)) def)
      ;;                        (map (lambda (vr) (arch-operand-format vr)) use)))

      ;;     ))

      (define (walk node)
        (let ((k    (node-index node))
              (def  (cp (node-def node)))
              (in   (cp (node-in  node))))

          ;; for each vreg defined at this node, see if there is
          ;; an active range in actv, and if so set range.start to this node's index
          ;; and move to the list of the vreg's processed ranges in rgs.
          ;;
          ;; If there is no active range, then we create a point range.
          (for-each (lambda (vreg)
                      (let ((end (hash-table-ref/default actv vreg #f)))
                        (cond
                         (end
                          (hash-table-delete! actv vreg)
                          (add-range rgs vreg k end))
                         (else
                          (add-range rgs vreg k k)))))
                    def)

          (for-each (lambda (vreg)
                      (let ((end (hash-table-ref/default actv vreg #f)))
                        (cond
                         ((not end)
                          (hash-table-set! actv vreg k)))))
                    in)))

      (for-each walk node*)

      (build-ranges rgs)))

  (define (pool-make hregs fixed)
    (let ((table  (make-vector (vector-length *hreg-table*) '()))
          (regs   (reverse
                   (fold (lambda (hreg x)
                           (if (mc-vreg-reserved hreg)
                               x
                               (cons hreg x)))
                         '()
                         hregs))))
      (for-each (lambda (hreg/fixed)
                  (vector-set! table
                               (- (mc-vreg-id (car hreg/fixed)) *hreg-start-id*)
                               (cdr hreg/fixed)))
                fixed)
      (make-pool regs table)))

  (define (pool-empty? pool)
    (null? (pool-hregs pool)))

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
        (let loop ((hreg* (pool-hregs pool)))
          (match hreg*
            (() #f)
            ((hreg . hreg*)
             (if (can-alloc? pool range hreg)
                 (pool-remove pool hreg)
                 (loop hreg*)))))))

  ;;
  ;; Check whether the given range overlaps with any of a hreg's fixed ranges
  ;;
  (define (can-alloc? pool range hreg)
    (let loop ((fxr* (vector-ref (pool-fixed pool) (- (mc-vreg-id hreg) *hreg-start-id*))))
      (match fxr*
        (() #t)
        ((fxr . fxr*)
         (if (range-overlap? fxr range)
             #f
             (loop fxr*))))))

  ;; Expire active ranges which end before the given range starts
  ;;
  ;; returns active ranges that have not yet expired
  ;;
  (define (expire-active pool assns cur active)
    (let loop ((ac* active))
      (match ac*
        (() '())
        ((ac . rest*)
         (cond
          ((<= (range-end ac) (range-start cur))
           (let ((hreg (hash-table-ref assns (range-vreg ac))))
             (pool-push pool hreg))
           (loop rest*))
          (else ac*))))))

  ;; TODO optimize
  (define (update-active active range)
    (sort (cons range active) range-ends-before?))

  ;; For our spilling heuristic, we select the longest range in active
  ;; TODO: rather use the number of vreg uses as a heuristic
  ;;
  (define (select-for-spill active)
    (if (null? active)
        #f
        (car (sort active
                   (lambda (r1 r2)
                     (>= (- (range-end r1) (range-start r1))
                         (- (range-end r2) (range-start r2))))))))


  (define (spill vreg tbl cxt ranges)
    (print "spilling t" (mc-vreg-id vreg))
    ;; loop through all users, and create a point range for each use
    (let loop ((usr* (mc-vreg-usrs vreg)) (ranges ranges) (wl '()))
      (match usr*
        (()
         (hash-table-set! tbl vreg wl)
         (sort ranges range-starts-before?))
        ((usr . usr*)
         (let* ((tmp     (mc-vreg-alloc cxt))
                (wli     (cons tmp usr))
                (rng     (make-range tmp (mc-inst-idx usr) (mc-inst-idx usr))))
           (loop usr*
                 (cons rng  ranges)
                 (cons wli  wl)))))))

  ;; Spilling heuristics:
  ;; if pool is empty AND active non-empty, select longest range in Active
  ;; if pool is empty select cur
  ;; if pool is non-empty and hreg-alloc fails, then spill cur (indicates that cur is a callee-save var)

  (define (scan cxt pool ranges)
    (let loop ((ranges  (sort ranges range-starts-before?))
               (acv     '())
               (assns   (make-hash-table eq? (lambda (v b)
                                               (number-hash (mc-vreg-id v) b))
                                         20))
               (spd     (make-hash-table eq? (lambda (v b)
                                               (number-hash (mc-vreg-id v) b))
                                         20)))
      (cond
        ;; completion. return spills worklist and an array mapping
        ((null? ranges)
         (cons assns spd))
        (else
         (let ((cur     (car ranges))
               (range*  (cdr ranges)))
           ;; handle current range
           ;;(pretty-print (format-step pool cur acv range*))

           ;; expire intervals that end before cur
           (set! acv (expire-active pool assns cur acv))

           ;; all hregs allocated to active set?
           (if (and (null? (pool-hregs pool)) (not (null? acv)))
               (let ((x (select-for-spill acv)))

                 (set! range* (spill (range-vreg x) spd cxt range*))
                 (set! acv    (delete! x acv eq?))

                  ;; return hreg to the pool
                 (pool-push pool (hash-table-ref assns (range-vreg x)))
                 (hash-table-delete! assns (range-vreg x))))

           ;; now at least 1 free hreg is available, but need to check for overlaps with fixed ranges
           ;; we should always be able to allocate point ranges here (TODO: add assertions)
           (let ((hreg (hreg-alloc pool cur)))
             (if (not hreg)
                 (let ((range* (spill (range-vreg cur) spd cxt range*)))
                   (loop range* acv assns spd))
                 (begin
                   (hash-table-set! assns (range-vreg cur) hreg)
                   (loop range* (update-active acv cur) assns spd)))))))))

  (define (print-ranges free/fixed)
    (pretty-print
     `(ranges
       (fixed
        ,@(map (lambda (name/sr)
                `(,(string->symbol (arch-operand-format (car name/sr)))
                  ,@(map (lambda (sr)
                           (format-range sr))
                         (cdr name/sr))))
              (cdr free/fixed)))
       (free
        ,@(map (lambda (r)
                 (format-range r))
               (car free/fixed))))))

  (define (alloc-registers-pass cxt regs)
    (print "*** alloc-regs-pass ***\n")
    (let* ((graph       (build-graph cxt))
           (free/fixed  (compute-ranges cxt graph))
           (pool        (pool-make regs (cdr free/fixed)))
           (spill-tbl   (make-hash-table eq? number-hash 32)))

      ;; Debugging
      (print-ranges free/fixed)
      (mc-cxt-print cxt (current-output-port))

      (let ((assns/spd (scan cxt pool (car free/fixed))))

        (hash-table-for-each (car assns/spd)
          (lambda (k v)
            (pretty-print (list (string->symbol (arch-operand-format k))
                                (string->symbol (arch-operand-format v))))))

        (hash-table-for-each (cdr assns/spd)
                             (lambda (k v)
                               (pretty-print (list (string->symbol (arch-operand-format k))
                                                   v))))

        (arch-rewrite cxt (car assns/spd) (cdr assns/spd))
        #f)))

  (define (alloc-regs mod)
    (mc-cxt-for-each
     (lambda (cxt)
       (alloc-registers-pass cxt (vector->list *hreg-table*)))
     mod)
    mod)

  (define (alloc-regs-test mod regs)
    (mc-cxt-for-each
     (lambda (cxt)
       (alloc-registers-pass cxt regs))
     mod)
    mod)

)
