

(use matchable)
(use srfi-1)
(use srfi-95)

(include "struct-syntax")

(define-struct dec-node        (name access-path branches live-patterns))
(define-struct dec-action-node (name pat-index action))

(define-struct dec-branch (constr succ))
(define-struct dec-constr (name args))

(define (parse-pattern pat pat-index)

  (define (walk-vector pat access-path index cont)
    (cond
     ((< index (vector-length pat))
      (walk (vector-ref pat index)
            (reverse (cons (+ 1 index) (reverse access-path)))
            (walk-vector pat access-path (+ index 1) cont)))
     (else cont)))
       
  (define (walk pat access-path cont)
    (match pat
      ((or () #t #f (? string?) (? number?) (? char?) ('quote _))
       (let* ((branch (make-dec-branch
                        (make-dec-constr 'constant (list pat))
                        cont))
              (node   (make-dec-node (gensym) access-path (list branch) (list pat-index))))
         node))
      (#(_ ...)
       (let* ((succ   (walk-vector pat access-path 0 cont))
              (branch (make-dec-branch
                        (make-dec-constr 'vector (list (vector-length pat)))
                        succ))
              (node   (make-dec-node (gensym) access-path (list branch) (list pat-index))))
         node))))

  (walk pat `(1) (make-dec-action-node (gensym) pat-index '(print "hello"))))

(define (merge-branches x y)

  ;; O(n) merging algorithm
  ;; Works as follows
  ;;
  ;; 1. Merges lists x and y into an ordered list
  ;; 2. Iterates over the ordered list and merges branches which are equivalent
  ;;

  ;; define an equivalence relation on constructors
  (define (constr=? x y)
   (equal? x y))

  ;; define an ordering relation on constructors
  (define (constr<? x y)
      (string<? (symbol->string (dec-constr-name x))
                (symbol->string (dec-constr-name y))))


  ;; main merging function
  (define (merge-branches* branches result)
    (match branches
      (()
       (reverse result))
      ((br)
       (merge-branches* '() (cons br result)))
      ((br1 br2 br* ...)
       (cond
        ((constr=? (dec-branch-constr br1) (dec-branch-constr br2))
         (let ((merged
                (make-dec-branch
                 (dec-branch-constr br1)
                 (merge-two-trees
                  (dec-branch-succ br1)
                  (dec-branch-succ br2)))))
           (merge-branches* (cons merged br*) result)))
        (else
         (merge-branches* (cons br2 br*) (cons br1 result)))))))

  (let* ((sorted-branches
          (merge x y
                 (lambda (x y)
                   (constr<? (dec-branch-constr x)
                             (dec-branch-constr y))))))

    (reverse (merge-branches* sorted-branches '()))))


(define (merge-two-trees x y)
  (struct-let* (((dec-node name1 ap1 br1 lp1) x)
                ((dec-node name2 ap2 br2 lp2) y))
    (cond
     ((equal? ap1 ap2)
      (make-dec-node name2 ap2 (merge-branches br1 br2) (append lp1 lp2)))
     (else
      (error 'merge-two-trees "could not merge trees")))))


(define (merge-trees trees)
  (cond
   ((null? trees) trees)
   (else (reduce (lambda (tree acc)
                   (merge-two-trees acc tree))
               '()
               trees))))

(define (print-node node)
  (struct-case node
    ((dec-action-node name pat-index action)
     `(action-node
       (name ,name)
       (pat-index ,pat-index)
       (action ,action)))
    ((dec-node name access-path branches live-patterns)
     `(dec-node
       (name ,name)
       (access-path ,access-path)
       (live-patterns ,live-patterns)
       (branches ,(map print-node branches))))
    ((dec-branch constr succ)
     `(dec-branch
       (constr ,(print-node constr))
       (succ ,(print-node succ))))
   ((dec-constr name args)
     `(dec-constr
       (name ,name)
       (args ,args)))))
    

(define (compile-tree node)

  (define syms '())
  (define *root* (gensym))

  (define (make-syms node)
    (struct-case node
      ((dec-node name ap br lp)
       (cond
        (ap
         (set! syms (cons
                     (cons ap name)
                     syms))
         (for-each (lambda (branch)
                     (struct-case branch
                       ((dec-branch constr succ)
                        (make-syms succ))))
                   br))))
      ((dec-action-node name pat-index action)
       #f)))
                 
  (define (generate-destructuring node access-path)
    (match access-path
      ((1) *root*)
      ((x ...)
       (let ((parent
              (cond
               ((assoc (reverse (cdr (reverse x))) syms)
                => cdr)
               (else (error 'compile-node)))))
         `(vector-ref ,parent ,(- (car (reverse x)) 1))))))
  
  (define (compile-node node)
    (struct-case node
      ((dec-node name access-path branches)
       `(let ((,name ,(generate-destructuring node access-path)))
          ,(let f ((branches branches))
             (match branches
               (() `(tree-match-error))
               ((b . b*)
                (struct-let* (((dec-branch constr succ) b)
                              ((dec-constr constr-name args) constr))
                  (case constr-name
                    ((vector)
                     (match-let (((len) args)) 
                       `(if (and (vector? ,name) (= (vector-length ,name) ,len))
                            ,(compile-node succ)
                            ,(f b*))))
                    ((constant)
                     (match-let (((value) args)) 
                       `(if (equal? ,name ,value)
                            ,(compile-node succ)
                            ,(f b*)))))))))))
      ((dec-action-node name pat-index action)
       action)))
  
  (make-syms node)
  
  (compile-node node))


(define (generate-dot-graph tree)

  (define (join-with-str str items)
    (match items
      ((x) (format "~a" x))
      ((x . x*)
       (format "~a~a~a" x str (join-with-str str x*)))))

  (define (format-constr-arg arg)
    (match arg
      (('quote x)
       (format "~s" x))
      (_ (format "~s" arg))))

  (define (format-constr constr)
    (struct-let* (((dec-constr name args) constr))
      (format "~s(~a)" name (join-with-str "," (map format-constr-arg args)))))

  (define (write-branch parent branch)
    (struct-let* (((dec-branch constr succ) branch))
      (struct-case succ
        ((dec-node name access-path branches live-patterns)
         (cons (format "  ~s -> ~s [label=~s];\n"
                 (dec-node-name parent)
                 name
                 (format-constr constr))
               (write-node succ)))
        ((dec-action-node name pat-index action)
         (append
           (write-node succ)
           (list (format "  ~s -> ~s [label=~s];\n" (dec-node-name parent) name (format-constr constr))))))))
              

  (define (write-node node)
    (struct-case node
      ((dec-node name access-path branches live-patterns)
       (cons (format "  ~s [label=\"~a {~a}\"];\n" name
                     (join-with-str "." access-path)
                     (join-with-str ", " live-patterns))
             (apply append
                    (map (lambda (branch)
                           (write-branch node branch))
                         branches))))
      ((dec-action-node name pat-index action)
       (list (format "  ~s [label=\"{~s}\"];\n" name pat-index)))))

  (apply string-append
         `("digraph G {\n"
           "  nodesep=1.75;\n"
           "  ranksep=1.75;\n"
           ,@(write-node tree)
          "}\n")))
         
(let* ((tree (merge-trees (list (parse-pattern '#('add #('sub 'x #f) 'z) 0)
                                (parse-pattern '#('add #('sub 'y #f) 'z) 1)
                                (parse-pattern '#('or #('sub 'x #f) 'z) 2)
                                (parse-pattern '#('add 'x 'z) 3)
                                (parse-pattern 45 4)
                                (parse-pattern '#('and 't 'z) 5)
                                (parse-pattern '#('xor) 6)))))
  (print (generate-dot-graph tree)))

