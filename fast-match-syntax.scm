
(import-for-syntax matchable)
(import-for-syntax srfi-1)
(import-for-syntax srfi-95)
(import-for-syntax chicken)

(define (fast-match-fail)
  (error 'fast-match "no matching pattern"))

(define-syntax fast-match
  (lambda (e r c)

    (define-syntax assert-not-reached
      (lambda (e r c)
        `(,(r 'assert) #f "should not reach here")))

    (define (make-node path binding branches live-indices)
      (vector 'node path binding branches live-indices #f #f))

    (define (make-leaf-node index bindings action)
      (vector 'leaf-node '() '() '() (list index) bindings action))

    (define (node-type     x) (vector-ref x 0))
    (define (node-path     x) (vector-ref x 1))
    (define (node-binding  x) (vector-ref x 2))
    (define (node-branches x) (vector-ref x 3))
    (define (node-indices  x) (vector-ref x 4))
    (define (node-bindings x) (vector-ref x 5))
    (define (node-action   x) (vector-ref x 6))

    (define (node? x)
      (eq? (node-type x) 'node))

    (define (leaf-node? x)
      (eq? (node-type x) 'leaf-node))

    (define (make-branch constr succ)
      (cons constr succ))
    
    (define (branch-constr br) (car br))
    (define (branch-succ   br) (cdr br))

    (define (parse-rule index pat action) 

      (define (parse pat names path cont)
        (match pat
         ('_
          (make-node path names (list (make-branch `(any) cont)) (list index)))
         ((? symbol?)
          (make-node path (lset-union eq? (list pat) names) (list (make-branch `(any) cont)) (list index)))
         ((? null?)
          (make-node path names (list (make-branch `(null) cont)) (list index)))            
         ((or (? boolean?) (? string?) (? number?) (? char?))
          (make-node path names (list (make-branch `(constant ,pat) cont)) (list index)))
         (('quote _)
          (make-node path names (list (make-branch `(constant ,pat) cont)) (list index)))
         (('? pred)
          (make-node path names (list (make-branch `(predicate ,pred) cont)) (list index)))
         (('as x p)
          (parse p (lset-union eq? (list x) names) path cont))
         (('$ struct p* ...)
          (make-node path names
                     (list
                      (make-branch `(structure ,struct)
                                   (parse-struct p* path 1 cont)))
                     (list index)))
         ((p . q)
          (make-node path names (list (make-branch `(pair)
                                        (parse p '() (cons 'car path)
                                          (parse q '() (cons 'cdr path) cont)))) (list index)))
         ((? vector?)
          (make-node path names
                     (list
                       (make-branch `(vector ,(vector-length pat))
                         (parse-vector pat path 0 cont)))
                     (list index)))
         (else (assert-not-reached))))

      ;; Parses all the sub-patterns in `pat`, where `pat` represents a vector.
      (define (parse-vector pat path index cont)
        (cond
         ((< index (vector-length pat))
          (parse (vector-ref pat index)
            '()
            (cons `(vector-ref ,index) path)
            (parse-vector pat path (+ index 1) cont)))
         (else cont)))

      ;; Parses all the sub-patterns in `pat`, where `pat` represents a structure.
      (define (parse-struct pats path index cont)
        (match pats
          (() cont)
          ((x . xs)
           (parse x
             '()
             (cons `(block-ref ,index) path)
             (parse-struct xs path (+ index 1) cont)))))
    
      ;; Extract all names in the source pattern `pat`.
      (define (extract-names pat)
        (match pat
          ('_ '())
          (() '())
          ((? symbol?)
           (list pat))
          ((or #t #f (? string?) (? number?) (? char?) ('quote _))
           '())
          (('? pred)
           '())
          (('as x p)
           (cons x (extract-names p)))
          (('$ type p* ...)
           (append-map extract-names p*))
          ((p . q)
           (append (extract-names p) (extract-names q)))
          (#(p* ...)
           (append-map extract-names p*))))
      
      ;; the access path for the root value
      (define *root-path* '(root))

      (parse pat '() *root-path* (make-leaf-node index (extract-names pat) action)))

    ;; Adds `branch` to the set of outgoing branches of `node`
    (define (add-branch node branch)
      (make-node
       (node-path node)
       (node-binding node)
       (merge-branches
        (node-branches node)
        (list branch))
       (lset-union eq? (node-indices node) (node-indices (branch-succ branch)))))
  
    ;; Merges two sets of branches `x` and `y`
    (define (merge-branches x y)

      (define (branch=? x y)
        (equal? (branch-constr x) (branch-constr y)))
      
      (define (merge br branches)
        (let f ((xs branches) (acc '()) (found #f))
          (match xs
            (()
             (if found
                 acc
                 (cons br acc)))
            ((x . xs)
             (if (branch=? br x)
                 (cond 
                  ((and (node? (branch-succ x)) (node? (branch-succ br)) (equal? (node-path (branch-succ x)) (node-path (branch-succ br))))
                   (f xs
                      (cons (make-branch
                             (branch-constr x)
                             (merge-two-trees (branch-succ x) (branch-succ br)))
                            acc) #t))
                  ((and (leaf-node? (branch-succ x)) (leaf-node? (branch-succ br)))
                   (f xs
                      (cons (make-branch
                             (branch-constr x)
                             (merge-two-trees (branch-succ x) (branch-succ br)))
                            acc) #t))
                  ((and (node? (branch-succ x)) (node? (branch-succ br)) (equal? (branch-constr x) '(any)))
                   (f xs
                      (cons
                       (make-branch
                         (branch-constr x)
                         (add-branch (branch-succ x) br))
                       acc) #t))
                  ((and (node? (branch-succ x)) (leaf-node? (branch-succ br)) (equal? (branch-constr x) '(any)))
                   (f xs
                      (cons
                       (make-branch
                         (branch-constr x)
                         (add-branch (branch-succ x) br))
                       acc) #t))
                  ((and (leaf-node? (branch-succ x)) (node? (branch-succ br)) (equal? (branch-constr x) '(any)))
                   (f xs
                      (cons
                       (make-branch
                         (branch-constr x)
                         (add-branch (branch-succ br) x))
                       acc) #t))
                  (else (assert-not-reached)))
                 (f xs (cons x acc) found))))))
      
      (fold (lambda (br acc)
              (merge br acc))
            y x))

    (define (merge-two-trees x y)
      (cond
       ((and (leaf-node? x) (leaf-node? y))
        (if (< (car (node-indices x)) (car (node-indices y)))
            x 
            y))
       ((and (node? x) (node? y) (equal? (node-path x) (node-path y)))
        (let ((merged-branches (merge-branches (node-branches x) (node-branches y)))
              (merged-indices  (lset-union eq? (node-indices x) (node-indices y)))
              (merged-bindings (lset-union eq? (node-binding x) (node-binding y))))
          (make-node (node-path x) merged-bindings merged-branches merged-indices)))
       (else (begin (pretty-print x) "bar" (pretty-print y) (assert-not-reached)))))

    (define (merge-trees trees)
      (cond
       ((null? trees) trees)
       (else
        (reduce
         (lambda (tree acc)
           (merge-two-trees acc tree))
         '()
         trees))))

    (define (nfa->dfa x)

      (define (walk-node x fc)
        (case (node-type x)
          ((node)
           (let-values (((any branches)
                         (extract-any (node-branches x))))
             (if (and (null? any) (null? fc))
                 (make-node (node-path x) (node-binding x)
                            (map (lambda (br)
                                   (make-branch (branch-constr br)
                                                (nfa->dfa (branch-succ br))))
                                 (node-branches x))
                            (node-indices x))
                 (let* ((any* (if (null? any)
                                 fc
                                 (make-branch `(any) (walk-node (branch-succ any) fc))))
                       (branches*
                        (let walk-branches ((branches branches) (acc '()))
                          (match branches
                            (() acc)
                            ((b . bs)
                             (if (and (node? (branch-succ b)) (node? (branch-succ any*))
                                      (equal? (node-path (branch-succ b)) (node-path (branch-succ any*))))
                                 (walk-branches bs (cons (make-branch (branch-constr b) (nfa->dfa (merge-two-trees (branch-succ b) (branch-succ any*)))) acc))
                                 (walk-branches bs (cons (make-branch (branch-constr b) (walk-node (branch-succ b) any*)) acc))))))))
                   (make-node (node-path x) (node-binding x) (reverse (cons any* branches*)) (lset-union eq? (node-indices (branch-succ any*)) (node-indices x)))))))
          ((leaf-node) x)))

      (define (extract-any branches)
        (let f ((xs branches) (disjoint '()) (any '()))
          (match xs
            (()
             (values any disjoint))
            ((x . xs)
             (case (car (branch-constr x))
               ((any)
                (assert (null? any)) 
                (f xs disjoint x))
               (else
                (f xs (cons x disjoint) any)))))))

      (define (order-branches branches)
        (let-values (((any branches*)
                      (extract-any branches)))
          (if (null? any)
              branches*
              (reverse (cons any branches*)))))

      (walk-node x '()))

    ;;
    ;; Prints a dot graph
    ;;
    
    (define (gen-dot-graph tree)
      
      (define (join str items)
        (match items
          (()  "")
          ((x) (format "~a" x))
          ((x . x*)
           (format "~a~a~a" x str (join str x*)))))
      
      (define (format-constr constr)
        (define (format-arg arg)
          (match arg
            (('quote x)
             (format "~s" x))
            (_ (format "~s" arg))))
        (match constr
          ((name args ...)
           (format "~s(~a)" name (join "," (map format-arg args))))))
      
      (define (write-branch parent-name child-name branch)
        (let ((succ   (branch-succ branch))
              (constr (branch-constr branch)))
          (case (node-type succ)
            ((node)
             (cons (format "  ~s -> ~s [label=~s];"
                           parent-name
                           child-name
                           (format-constr constr))
                   (write-node succ child-name)))
            ((leaf-node)
             (append
              (write-node succ child-name)
              (list (format "  ~s -> ~s [label=~s];" parent-name child-name (format-constr constr))))))))
      
      (define (write-node node name)
        (case (node-type node)
          ((node)
           (cons (format "  ~s [label=\"~a {~a}\"];" name
                         (format "~s" (node-path node))
                         (join   ", " (node-indices node)))
                 (append-map
                  (lambda (branch)
                    (write-branch name (gensym) branch))
                  (node-branches node))))
          ((leaf-node)
           (list (format "  ~s [label=\"{~s}\"];" name (car (node-indices node)))))))
      
      `("digraph G {"
        "  nodesep=1.75;"
        "  ranksep=1.75;"
        ,@(write-node tree (gensym))
        "}"))

    (define (gen-dfa node actions value)

     ;; (print actions)


      (define (make-action-entry x)
        (list (first (node-indices x))
              (gensym)
              `(lambda ,(node-bindings x) ,@(node-action x))))

      (define *action-table*
        (map make-action-entry actions))
      (define *action-code*
        (map (lambda (action-entry)
               (cdr action-entry))
             *action-table*))
      

      ;; Compiles the DFA `node` into native scheme code which will match against the scheme datum `value`. 
  
      (define (gen-ref path table)
        ;; Generates scheme code to reference a part of an aggregate scheme datum.
        (let ((%car        (r 'car))
              (%cdr        (r 'cdr))
              (%vector-ref (r 'vector-ref)))
          (match path
            ((root)
             (let ((parent
                    (cond
                     ((assoc '(root) (car table))
                      => cdr)
                     (else (error 'compile-node)))))
               parent))
            (('car rest ...)
             (let ((parent
                    (cond
                     ((assoc rest (car table))
                      => cdr)
                     (else (error 'compile-node)))))
               `(,%car ,parent)))
            (('cdr rest ...)
             (let ((parent
                    (cond
                     ((assoc rest (car table))
                      => cdr)
                     (else (error 'compile-node)))))
               `(,%cdr ,parent)))
            ((('block-ref index) rest* ...)
             (let ((parent
                    (cond
                     ((assoc rest* (car table))
                      => cdr)
                     (else (error 'compile-node)))))
               `(##sys#block-ref ,parent ,index)))
            ((('vector-ref index) rest* ...)
             (let ((parent
                    (cond
                     ((assoc rest* (car table))
                      => cdr)
                     (else (error 'compile-node)))))
               `(,%vector-ref ,parent ,index))))))

      ;; Generates code for outgoing edges and successors of the current decision node.
      (define (gen-branches branches name mappings)
        (let walk ((branches branches))
          (match branches
            (() `(fast-match-fail))
            ((x . xs)
             (let ((rest            (walk xs))
                   (succ            (gen-node (branch-succ x) mappings))
                   (%if             (r 'if))
                   (%and            (r 'and))
                   (%vector?        (r 'vector?))
                   (%vector-length  (r 'vector-length))
                   (%pair?          (r 'pair?))
                   (%equal?         (r 'equal?))
                   (%null?          (r 'null?))
                   (%=              (r '=)))
               (match (branch-constr x)
                 (('vector size)
                  `(,%if (,%and (,%vector? ,name) (,%= (,%vector-length ,name) ,size))
                         ,succ
                         ,rest))
                 (('null)
                  `(,%if (,%null? ,name)
                         ,succ
                         ,rest))
                 (('pair)
                  `(,%if (,%pair? ,name)
                         ,succ
                         ,rest))
                 (('constant data)
                  `(,%if (,%equal? ,name ,data)
                         ,succ
                         ,rest))
                 (('predicate pred)
                  `(,%if (,pred ,name)
                         ,succ
                         ,rest))
                  (('structure type)
                  `(,%if (##sys#structure? ,name ',type)
                         ,succ
                         ,rest))
                 (('any)
                  succ)))))))
      
      (define (extend-table path bindings name table)
        (cons
         (cons (cons path name) (car table))
         (if (null? bindings)
             (cdr table)  
             (append (map (lambda (binding)
                            (cons binding name))
                          bindings)
                     (cdr table)))))

      (define (gen-node x table)
        (case (node-type x)
          ((node)
           (let* ((path         (node-path x))
                  (binding      (node-binding x))
                  (branches     (node-branches x))
                  (name         (gensym))
                  (ref-code     (gen-ref path table))
                  (branch-code  (gen-branches branches name (extend-table path binding name table)))
                  (%let         (r 'let)))
             `(,%let ((,name ,ref-code))
                ,branch-code)))
          ((leaf-node)
           (let* ((action-proc (cond
                                ((assq (first (node-indices x)) *action-table*)
                                 => second)
                                (else (assert-not-reached))))
                  (bindings     (map (lambda (binding)
                                       (cond
                                        ((assq binding (cdr table))
                                         => cdr
                                         (else (assert-not-reached)))))
                                     (node-bindings x))))
             `(,action-proc ,@bindings)))))
      
      (define (init-mappings root name)
        (let ((binding (node-binding root)))
          (cons (list (cons '(root) name))
                (if (null? binding)
                    '()
                    (list (cons binding name))))))

      (let* ((root-name (gensym))
             (code (gen-node node (init-mappings node root-name)))
             (%let (r 'let)))
        `(,%let ((,root-name ,value) ,@*action-code*)
                ,code)))
 
   (define (index-generator)
     (let ((k 0))
       (lambda ()
         (let ((ret k))
           (set! k (+ k 1))
           ret))))
    
   (define (get-actions x)
     (case (node-type x)
       ((node)
        (append-map (lambda (br)
                      (get-actions (branch-succ br)))
                    (node-branches x)))
       ((leaf-node) (list x))))

    (match e
      (('fast-match data cls* ...)
       (let* ((forest
               (let ((index (index-generator)))
                 (map (lambda (cls)
                      (match cls
                        ((pat action* ...)
                         (parse-rule (index) pat action*))))
                      cls*)))
              (actions (append-map get-actions forest))
              (dfa  (nfa->dfa (merge-trees forest)))
              (code (gen-dfa dfa actions data)))
         code)))))