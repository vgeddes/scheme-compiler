
(declare (unit pass)
         (uses nodes arch tree utils))

(use matchable)
(use srfi-1)
(use srfi-69)

(include "hil-syntax")
(include "struct-syntax")

(define build-sexp
  (lambda (node)
    (node-case node
     ((hil-mod (entry))
      `(module ,(build-sexp entry)))
     ((hil-if (x y z))
      `(if ,(build-sexp x)
           ,(build-sexp y)
           ,(build-sexp z)))
     ((hil-fn (name params body cxt))
      `(let (,name (fn ,params ,(build-sexp body)))
         ,(build-sexp cxt)))
     ((hil-cont (name params body cxt))
      `(let (,name (cont ,params ,(build-sexp body)))
         ,(build-sexp cxt)))
     ((hil-retcont (name value cxt))
      `(let (,name (retcont (,value)))
         ,(build-sexp cxt)))
   ;;  ((hil-fix defs body)
   ;;   `(fix ,(map write-sexp defs) ,(write-sexp body)))
     ((hil-app (fn args))
      `(app ,(build-sexp fn) ,@(map build-sexp args)))
     ((hil-appcont (cont value))
      `(appcont ,(build-sexp cont) ,(build-sexp value)))
 ;;    ((app name args)
 ;;     `(app ,(write-sexp name) ,(map write-sexp args)))
 ;;    ((prim name args result cexp)
 ;;     `(prim ,(write-sexp name) ,(map write-sexp args) ,(write-sexp result) ,(write-sexp cexp)))
 ;;    ((nil)
 ;;     `())
     ((hil-const (value)) value)
     ((hil-var   (name))  name)
     ((hil-nil   ())     'nil)
     ((hil-prim  (name params)) `(prim ,name ,(map build-sexp params)))
     ((hil-rec   (name values cxt))
      `(let ((,name (record ,@(map build-sexp values))))
         ,(build-sexp cxt)))
     ((hil-ref   (name rec index cxt))
      `(let ((,name (ref ,(build-sexp rec) ,index)))
         ,(build-sexp cxt)))
 ;;    ((record values name cexp)
  ;;    `(record ,(map write-sexp values) ,(write-sexp name) ,(write-sexp cexp)))
 ;;    ((select index record name cexp)
 ;;     `(select
 ;;       ,index
 ;;       ,(write-sexp record)
 ;;       ,(write-sexp name)
 ;;       ,(write-sexp cexp)))
 ;;   ((label name)
 ;;     (string->symbol (format "$~s" name)))
 ;;    ((module contexts)
 ;;     `(module ,(map write-sexp contexts)))
 ;;    ((context formals start-block blocks)
  ;;    `(context ,formals ,start-block
 ;;               ,blocks))
 ;;    ((block label code)
 ;;     `(block ,label ,code))
     (else node ))))

;;(define-node lil-mod      (cxts))

;;(define-node lil-const    (type value))
;;(define-node lil-call     (proc args))
;;(define-node lil-brc      (test tl fl))
;;(define-node lil-block    (head tail))
;;(define-node lil-context  (name params start))


(define-node hil-mod      (entry))
(define-node hil-if       (x y z))
(define-node hil-app      (fn args))
(define-node hil-appcont  (cont value))
(define-node hil-const    (value))
(define-node hil-var      (name))
(define-node hil-prim     (name params))
(define-node hil-fn       (name params body cxt))
(define-node hil-cont     (name params body cxt))
(define-node hil-ref      (name rec index cxt))
(define-node hil-rec      (name values cxt))
(define-node hil-retcont  (name params cxt))

(define (data-set node key val)
 (##sys#block-set! node 1 (cons (cons key val) (##sys#block-ref node 1))))

(define (data-ref node key)
  (cond
    ((assq key (##sys#block-ref node 1)) => cdr)
     (else #f)))

;;
;; Normalization: Simplify various constructs and perform basic macro expansion
;;
;; (begin e1 e2 ...) is transformed into nested lambdas, each of which contain only one expression
;;
;; The boolean operations and, or, not are simplified to nested if statements
;;
;;

(define (normalize e)
  (define (N e)
   (match e
     (('let (bindings ...) body ...)
      (N
       `((fn ,(map car bindings)
           (begin ,@body))
         ,@(map (lambda (binding) (car (cdr binding))) bindings))))
     (('begin) '())
     (('begin body)
      (N body))
     (('begin body1 body2 ...)
      (N
       (let ((a (gensym)))
         `((fn (,a)
             (begin ,@body2)) ,body1))))
     (('not ex1)
      (N
        `(if ,ex1 #f #t)))
     (('or) #f)
     (('or ex1)
      (N ex1))
     (('or ex1 ex2 ...)
      (N
       (let ((tmp (gensym)))
         `(let ((,tmp ,ex1))
            (if ,tmp
                ,tmp
                (or ,@ex2))))))
     (('and) #t)
     (('and ex1)
      (N ex1))
     (('and ex1 ex2 ...)
      (N
       `(if ,ex1
            (and ,@ex2)
            #f)))
     (('lambda (bindings ...) ex)
      `(fn ,bindings ,(N ex)))
     (('lambda (bindings ...) ex1 rest* ...)
      `(fn ,bindings ,(N `(begin ,ex1 ,@rest*))))
     ((ex* ...)
      (map N ex*))
     (_ e)))
  (N e))

;;
;; Alpha Conversion: Rename variables so that each variable name is unique
;;
;; Before:
;;
;; ((lambda (u)
;;   ((lambda (x)
;;      (fx+ u x))
;;    4))
;; 3)
;;
;; After:
;;
;; ((lambda (v23)
;;   ((lambda (v24)
;;      (fx+ v23 24))
;;    4))
;; 3)
;;

(define *prim-table*
  '((fx+     . __sc_fxadd)
    (fx-     . __sc_fxsub)
    (fx*     . __sc_fxmul)
    (fx/     . __sc_fxdiv)
    (fx<     . __sc_fxlt)
    (fx>     . __sc_fxgt)
    (fx<=    . __sc_fxle)
    (fx>=    . __sc_fxge)
    (fx=     . __sc_fxeq)
    (car     . __sc_car)
    (cdr     . __sc_cdr)
    (cons    . __sc_cons)
    (null?   . __sc_nullp)
    (pair?   . __sc_pairp)
    (list?   . __sc_listp)
    (fixnum? . __sc_fixnump)))

(define (prim? name)
  (and (assq name *prim-table*) #t))

(define (rename name scopes)
  (let loop ((scope* scopes))
    (match scope*
      ((scope . scope*)
       (cond
         ((assq name scope) => cdr)
         (else (loop scope*))))
      (()
       (if (prim? name)
           name
           (assert-not-reached))))))

(define (alpha-convert ex)
  (define (A ex sc)
     (match ex
       (('fn args body)
        (let ((scope (map (lambda (arg)
                            (cons arg (gensym 't)))
                          args)))
          `(fn ,(map cdr scope)
               ,(A body (cons scope sc)))))
       (('if x y z)
        `(if ,(A x sc)
             ,(A y sc)
             ,(A z sc)))
       ((ex* ...)
        (map (lambda (ex)
               (A ex sc))
             ex*))
       ((? symbol?)
        (rename ex sc))
       (_ ex)))
  (A ex '()))

;;
;; Convert normalized scheme text into an AST
;;
;;

(define (hil-convert ex)
  (define (H ex)
    (match ex
      (('if x y)
       (hil-if (H x) (H y) '()))
      (('if x y z)
       (hil-if (H x) (H y) (H z)))
      (('if _ ...)
       (error cs "ill-formed conditional expression"))
      (('fn (bindings ...) body)
       (let ((name (gensym 'fn)))
         (hil-fn name bindings (H body) (hil-var name))))
      ((? null?)
       (hil-const ex))
      ((? boolean?)
       (hil-const ex))
      ((? number?)
       (hil-const ex))
      ((ex* ...)
       (let* ((ex-data  (reverse
                          (fold (lambda (ex x)
                                  (match ex
                                    (('fn (param* ...) body)
                                     (cons (list 'proc (gensym 'fn) ex) x))
                                    (_
                                     (cons (list 'other ex) x))))
                                '()
                                ex*)))
              (args (map (lambda (dt)
                           (match dt
                             (('proc name fn)
                              (hil-var name))
                             (('other obj)
                              (H obj))
                             (else (assert-not-reached))))
                          ex-data))
              (defs (fold (lambda (dt x)
                               (match dt
                                 (('proc name fn)
                                  (match fn
                                    (('fn (arg* ...) body)
                                     (hil-fn name arg* (H body) x))))
                                 (_ x)))
                           (if (and (hil-var? (car args)) (prim? (hil-var-name (car args))))
                               (hil-prim (hil-var-name (car args)) (cdr args))
                               (hil-app  (car args) (cdr args)))
                           ex-data)))
       defs))
      ((? symbol?)
       (hil-var ex))))
  (hil-mod (H ex)))

(define (cps-convert node)

   (define (analyze exps)
     (let loop ((exp* exps) (atoms '()) (cpx '()))
       (match exp*
         (() (cons (reverse atoms) cpx))
         ((exp . exp*)
          (cond
           ((hil-var? exp)
            (loop exp* (cons exp atoms) cpx))
           ((hil-const? exp)
            (loop exp* (cons exp atoms) cpx))
           (else
            (let ((tmp (gensym 't)))
              (loop exp* (cons (hil-var tmp) atoms) (cons (cons exp tmp) cpx)))))))))

   (define (build exps cont leafp)
     (let* ((data     (analyze exps))
            (atoms    (car data))
            (cpx*     (cdr data))
            (leafexp  (leafp atoms cont)))
       (let loop ((cpx* cpx*) (cexp leafexp))
         (match cpx*
           (() cexp)
           ((cpx . cpx*)
            (match cpx
              ((exp . tmp)
               (let ((cname (gensym 'c)))
                 (loop cpx* (hil-cont cname (list tmp) cexp
                              (F exp (hil-var cname))))))))))))

   (define (F node cont)
    (node-case node
      ((hil-const (value))
       (hil-appcont cont node))
      ((hil-var (name))
       (hil-appcont cont node))
      ((hil-fn (name params body cxt))
       (let* ((cn       (gensym 'c))
              (node-cps (hil-fn
                          name
                          (cons cn params)
                          (F body (hil-var cn))
                          (F cxt cont))))
          node-cps))
      ((hil-app (fn args))
       (let* ((leafp (lambda (atoms cont)
                       (hil-app (car atoms) (cons cont (cdr atoms)))))
              (cexp  (build (cons fn args) cont leafp)))
         cexp))
      ((hil-prim (name params))
       (let* ((leafp (lambda (atoms cont)
                      (hil-prim name (cons cont atoms))))
              (cexp (build params cont leafp)))
         cexp))
      ((hil-if (test x y))
       (cond
         ((or (hil-var? test) (hil-const? test))
          (hil-if
           test
           (F x cont)
           (F y cont)))
       (else
        (let ((cn  (gensym 'c))
              (tn  (gensym 't)))
          (hil-cont cn (list tn)
            (hil-if
             (hil-var tn)
             (F x cont)
             (F y cont))
            (F test (hil-var cn)))))))
      (else (assert-not-reached))))
  (let ((cn (gensym 'c))
        (tn (gensym 't)))
   (hil-mod
     (hil-retcont cn
       tn
       (F (hil-mod-entry node) (hil-var cn))))))

;;
;; reduce the administrative reducible expressions produced by the CPS transform
;;
;; Example: ((lambda (x) (... x ...)) v)
;;                                        =>   (... v ...)
;; (define (beta-reduce node)
;;   (define dt (make-hash-table eq? symbol-hash 500))
;;   (define (add-info
;;   (define (B cexp dt)
;;     (node-case cexp
;;      ((hil-mod (body))
;;       (hil-mod (B body dt)))
;;      ((hil-var ())
;;       cexp)
;;      ((hil-const ())
;;       cexp)
;;      ((hil-if (x y z))
;;       (hil-if
;;         x
;;         (B y)
;;         (B z)))
;;      ((hil-fn (params body))
;;       (hil-fn
;;         params
;;         (B body)))
;;      ((hil-cont (params body))
;;       (hil-cont params (B body)))
;;      ((hil-app (fn args))
;;       (let* ((fn   (B fn))
;;              (args (map (lambda (arg)
;;                           (B arg))
;;                         args)))
;;         (cond
;;          ;; Rules for reducing applications
;;          ;; 1. Reduce if the applied object is a literal lambda AND if no other argument is a literal lambda
;;          ;; 2. Need to think of some more...
;;          ((and (hil-fn? fn)
;;                (fold (lambda (arg x)
;;                        (if (or (hil-fn? arg) (hil-cont? arg)) #f x))
;;                      #t
;;                      args))
;;            (substitute (hil-fn-body fn) (hil-fn-params fn) args))
;;          ((and (hil-cont? fn)
;;                (fold (lambda (arg x)
;;                        (if (or (hil-fn? arg) (hil-cont? arg)) #f x))
;;                      #t
;;                      args))
;;           (substitute (hil-cont-body fn) (list (hil-cont-name fn)) args))
;;          (else (hil-app fn args)))))
;;      ((hil-primc (name params cont))
;;       (hil-primc name
;;                  params
;;                  (B cont)))
;;      ((hil-nil ())
;;       cexp)
;;      (else (assert-not-reached))))

;;  (B node))

;;
;; Substitutes objects for names in a HIL expression
;;
;;

(define (substitute exp names objects)
  (let ((mappings (fold (lambda (name obj mappings)
                          (cons (cons name obj) mappings))
                        '() names objects)))

  (define (walk-cexp cexp)
    (node-case cexp
      ((hil-var (name))
       (cond
        ((assq name mappings) => cdr)
        (else cexp)))
      ((hil-const ())
       cexp)
      ((hil-if (test conseq altern))
       (hil-if (walk-cexp test) (walk-cexp conseq) (walk-cexp altern)))
      ((hil-fn (name params body))
       (hil-fn
         name
         params
         (substitute body (lset-difference names params) objects)))
      ((hil-cont (name body))
       (hil-cont name (walk-cexp body)))
      ((hil-app (fn args))
       (hil-app (walk-cexp fn)
                (map (lambda (arg)
                       (walk-cexp arg))
                     args)))
      ((hil-prim (name params cont))
       (hil-prim name
                  (map (lambda (arg)
                         (walk-cexp arg))
                       params)
                  (walk-cexp cont)))
      ((hil-nil ())
        cexp)
      (else (assert-not-reached))))

  (walk-cexp exp)))

(define select-matching
  (lambda (fn lst)
    (reverse
     (fold (lambda (x acc)
             (if (fn x)
                 (cons x acc)
                 acc))
           (list)
           lst))))

(define (analyze-free-vars node)
  (let ((union (lambda lists
                 (apply lset-union (cons eq? lists))))
        (diff  (lambda lists
                 (apply lset-difference (cons eq? lists)))))
    (define (A node)
      (node-case node
        ((hil-mod (entry))
         (A entry))
        ((hil-var (name))
         (list name))
        ((hil-const (value)) '())
        ((hil-if (test conseq altern))
         (union
          (A test)
          (A conseq)
          (A altern)))
        ((hil-app (fn args))
         (let loop ((x '()) (args (cons fn args)))
           (if (null? args)
               x
             (loop (union x (A (car args))) (cdr args)))))
        ((hil-prim (name params))
         (let loop ((x (list)) (args params))
           (if (null? args)
               x
             (loop (union x (A (car args))) (cdr args)))))
        ((hil-appcont (cont value))
         (if (hil-var? value)
             (list (hil-var-name value))
             (list)))
        ((hil-fn (name params body cxt))
         (data-set node 'free (diff (A body) params))
         (union (data-ref node 'free) (diff (A cxt) (list name))))
        ((hil-cont (name params body cxt))
         (union (diff (A body) params) (diff (A cxt) (list name))))
        ((hil-retcont (name value cxt))
         (diff (A cxt) (list value)))
        (else (assert-not-reached))))
    (A node)))

(define (closure-index name names)
  (let f ((i 1) (names names))
    (if (null? names)
        (error "should not reach here")
        (if (eq? name (car names))
            i
            (f (+ i 1) (cdr names))))))

(define (rewrite-app fn args kfn)
  (let ((name (hil-var-name fn)))
    (cond
      ((assq name kfn)
       => (lambda (x)
            (hil-app fn (cons (car args) (cons (hil-var (cdr x)) (cdr args))))))
      (else
        (let ((tmp (gensym 't)))
          (hil-ref tmp fn 0 (hil-app (hil-var tmp) (cons (car args) (cons fn (cdr args))))))))))

;;
;; assume all functions escape -> each function takes a closure arg
;; optimize application of known functions by jumping directly to the function's label instead of using the function ptr stored in the function's closure.
;;

#| * 1. create closure record for each lambda
   * 2. In fix body, replace each lambda reference (those not in the operator position) with closure reference
   * 3. rewrite lambda applications. Extract function label from closure and apply function to closure + args
 |#



(define (closure-convert node)

  (define (build-refs atoms env free leafp)
    (define (analyze atoms)
      (let loop ((at* atoms) (new '()) (refs '()))
        (match at*
          (() (cons (reverse new) (apply lset-union (cons eq? (map (lambda (ref) (list ref)) refs)))))
          ((at . at*)
           (cond
            ((and (hil-var? at) (memq (hil-var-name at) free))
             (let ((tmp (gensym 't)))
               (loop at* (cons (hil-var tmp) new) (cons (cons (hil-var-name at) tmp) refs))))
            (else
             (loop at* (cons at new) refs)))))))
    (let* ((data        (analyze atoms))
           (atoms-bound (car data))
           (atoms-free  (cdr data))
           (leafexp     (leafp atoms-bound)))
      (let loop ((ref* atoms-free) (exp leafexp))
        (match ref*
          (() exp)
          ((ref . ref*)
           (match ref
             ((name . tmp)
              (loop ref* (hil-ref tmp (hil-var env) (closure-index name free) exp)))))))))

  (define (R atom kfn)
    (cond
     ((and (hil-var? atom) (assq (hil-var-name atom) kfn))
      => (lambda (x)
           (hil-var (cdr x))))
     (else atom)))

  (define (R* atom* kfn)
    (map (lambda (x) (R x kfn)) atom*))

  (define (F node env free kfn)
    (node-case node
      ((hil-mod (entry))
       (hil-mod (F entry env free kfn)))
      ((hil-fn (name params body cxt))
       (let* ((fn-env     (gensym 'e))
              (fn-free    (data-ref node 'free))
              (cxt*       (F (hil-rec
                               fn-env
                               (cons (hil-var name)
                                     (map (lambda (x)
                                            (hil-var x))
                                          fn-free))
                               cxt)
                             env
                             free
                             (cons (cons name fn-env) kfn)))
              (fn         (hil-fn name (cons (car params) (cons fn-env (cdr params))) (F body fn-env fn-free kfn) cxt*)))
         (data-set fn 'free fn-free)
         fn))
      ((hil-cont (name params body cxt))
       (let* ((cxt*       (F cxt
                             env
                             free
                             kfn))
              (cont       (hil-cont name params (F body env free kfn) cxt*)))
         cont))
      ((hil-retcont (name value cxt))
       (hil-retcont name value (F cxt env free kfn)))
      ((hil-app (fn args))
       (let ((exp (build-refs (cons fn (R* args kfn))
                              env
                              free
                              (lambda (atoms)
                                (rewrite-app (car atoms) (cdr atoms) kfn)))))
         exp))
      ((hil-prim (name params))
       (let ((exp (build-refs (R* params kfn)
                              env
                              free
                              (lambda (atoms)
                                (hil-prim name atoms)))))
         exp))
      ((hil-rec (name values cxt))
       (let ((exp (build-refs (R* (cdr values) kfn)
                              env
                              free
                            (lambda (atoms)
                              (hil-rec name (cons (car values) atoms) (F cxt env free kfn))))))
         exp))
      ((hil-if (test x y))
       (let ((x*  (if (or (hil-var? x) (hil-const? x))
                      (R x kfn)
                      (F x env free kfn)))
             (y*  (if (or (hil-var? y) (hil-const? y))
                      (R y kfn)
                      (F y env free kfn)))
             (if* (if (and (hil-var? test) (memq (hil-var-name test) free))
                      (hil-ref tmp (hil-var env) (closure-index (hil-var-name test) free)
                               (hil-if (hil-var tmp) x* y*))
                      (hil-if (R test kfn) x* y*))))
         if*))
       ((hil-appcont (cont value))
        (cond
         ((and (hil-var? value) (memq (hil-var-name value) free))
          (let ((tmp (gensym 't)))
            (hil-ref tmp (hil-var env) (closure-index (hil-var-name value) free)
                     (hil-appcont cont (hil-var tmp)))))
         (else (hil-appcont cont (R value kfn)))))
 ;;      ((hil-const ())
;;        node)
;;       ((hil-var ())
;;        node)
       (else  (assert-not-reached))))
  (analyze-free-vars node)
  (F node '() '() '()))

(define (tree-convert-cexp cexp cont blk mod)

  (define (branch cexp cont pred)
    ;; process the code path referenced by `cexp' and return a label to the start block of the newly created subtree
    (let* ((blk (tree-make-block (gensym 'b) '() '() '() '() (tree-block-function pred))))
      (tree-block-pred-set! blk pred)
      (tree-block-add-succ! pred blk)
      (F cexp cont blk)
      (tree-make-label (tree-block-name blk))))

  (define (tree-atom x)
    (cond
     ((hil-const? x)
      (tree-make-constant 'i64 (hil-const-value x)))
     ((hil-var? x)
      (tree-make-temp (hil-var-name x)))
     (else
      (assert-not-reached))))

  (define (F node cont blk)
    (node-case node
      ((hil-fn (name params body cxt))
       (tree-convert-fn node mod)
       (F cxt cont blk))
      ((hil-cont (name params body cxt))
       (F cxt (car params) blk)
       (F body cont blk))
      ((hil-retcont (name value cxt))
       (F cxt value blk)
       (tree-block-add-statement! blk (tree-build-return (tree-make-temp value))))
      ((hil-ref (name rec index cxt))
       (let* ((rec  (tree-atom rec))
              (t1   (tree-build-load  'i64 (tree-build-add 'i32 rec (tree-constant-get 'i32 (* 8 index)))))
              (t2   (tree-build-assign name t1)))
         (tree-block-add-statement! blk t2)
         (F cxt cont blk)))
      ((hil-rec (name values cxt))
       (let* ((t0 (tree-build-assign
                    name
                    (tree-build-call 'ccall
                      (tree-make-label '__sc_record)
                      (map (lambda (arg)
                             (tree-atom arg))
                           values)))))
         (tree-block-add-statement! blk t0)
         (F cxt cont blk)))
      ((hil-appcont (cont value))
       (let ((t0 (tree-build-return (tree-atom value))))
         (tree-block-add-statement! blk t0)))
      ((hil-app (fn args))
       (let* ((tgt    (tree-make-label (hil-var-name fn)))
              (args*  (map (lambda (arg)
                            (tree-atom arg))
                            (cdr args)))
              (t0     (tree-build-call 'sc tgt args*)))
          (cond
            ((not cont)
            (let* ((tmp  (gensym 't))
                   (t1   (tree-build-assign tmp t0))
                   (t2   (tree-build-return (tree-make-temp tmp))))
              (tree-block-add-statement! blk t1)
              (tree-block-add-statement! blk t2)))
           (else
            (let ((t1  (tree-build-assign cont t0)))
              (tree-block-add-statement! blk t1))))))
      ((hil-if (test x y))
       (let* ((test (tree-atom test))
              (blkx (branch x cont blk))
              (blky (branch y cont blk))
              (t0 (tree-build-cmp 'eq test (tree-constant-get 'i64 *false-value*)))
              (t1 (tree-build-brc t0 blkx blky)))
         (tree-block-add-statement! blk t1)))
      ((hil-prim (name params))
         (cond
           ((prim? name)
            (let* ((tgt    (tree-make-label (cdr (assq name *prim-table*))))
                   (args   (map (lambda (arg)
                                 (tree-atom arg))
                                (cdr params)))
                   (t0     (tree-build-call  'ccall tgt args)))
              (cond
               ((not cont)
                (let* ((tmp  (gensym 't))
                       (t1   (tree-build-assign tmp t0))
                       (t2   (tree-build-return (tree-make-temp tmp))))
                  (tree-block-add-statement! blk t1)
                  (tree-block-add-statement! blk t2)))
               (else
                (let ((t1  (tree-build-assign cont t0)))
                  (tree-block-add-statement! blk t1))))))
           (else (assert-not-reached))))))

  (F cexp cont blk))

(define (tree-convert-fn node mod)
  (node-case node
    ((hil-fn (name params body cxt))
     (let* ((params* (map (lambda (arg)
                            (tree-make-temp arg))
                          (cdr params)))
            (fun      (tree-make-function name params* '() mod))
            (blk      (tree-make-block (gensym 'b) '() '() '() '() fun)))
       (tree-module-add-function! mod fun)
       (tree-function-entry-set! fun blk)
       (tree-convert-cexp body #f blk mod)))))

(define (tree-convert node)
  (node-case node
    ((hil-mod (entry))
     (let* ((mod    (tree-make-module))
            (fun    (tree-make-function '__begin '() '() mod))
            (blk    (tree-make-block    (gensym 'b) '() '() '() '() fun)))
       (tree-module-add-function! mod fun)
       (tree-function-entry-set! fun blk)
       (tree-convert-cexp entry #f blk mod)
       mod))
    (else (assert-not-reached))))

       ;; ((hil-cont (name params body cxt))
       ;;  (let* ((mod    (tree-make-module))
       ;;         (fun    (tree-make-function '__begin '() '() mod))
       ;;         (blk    (tree-make-block    (gensym 'b) '() '() '() '() fun)))
       ;;    (tree-module-add-function! mod fun)
       ;;    (tree-function-entry-set! fun blk)
       ;;    (tree-convert-cexp cxt (car params) blk mod)
       ;;    (tree-block-add-statement! blk (tree-build-return (tree-make-temp (car params))))
       ;;    mod))
       ;;   (else (assert-not-reached))))))

(define (select-instructions mod)
  (struct-case mod
    ((tree-module functions)
     (let* ((mc-mod (mc-make-module))
            (cxts   (map (lambda (fun)
                            (select-function mc-mod fun))
                       functions)))
       (arch-generate-bridge-context mc-mod)
       mc-mod))
    (else (assert-not-reached))))

(define (select-function mc-mod fun)
  (define (walk-block block mcxt mblk)
    (let ((succ (map (lambda (succ)
                       (walk-block
                         succ
                         mcxt
                         (mc-make-block mcxt (tree-block-name succ))))
                     (tree-block-succ block))))

      (mc-block-succ-set! mblk succ)

      (tree-for-each-statement (lambda (stm)
                                 (arch-emit-statement mblk stm))
                               block)
      mblk))

  (struct-case fun
    ((tree-function name params entry module)
     (let* ((mc-cxt (mc-make-context name (map (lambda (p) (tree-temp-name p)) params) mc-mod)))
       (walk-block entry mc-cxt (mc-context-start mc-cxt))
       mc-cxt))
    (else (assert-not-reached))))

(define (immediate-rep x)
  (cond
   ((integer? x)
    (bitwise-ior
      (arithmetic-shift x *fixnum-shift*)
      *fixnum-tag*))
   ((boolean? x)
    (bitwise-ior
      (arithmetic-shift (if x 1 0) *boolean-shift*)
      *boolean-tag*))
    ((null? x)
     *null-value*)
   (else (error 'immediate-rep "type not recognized"))))

(define *fixnum-shift*    2)
(define *fixnum-mask*   #x3)
(define *fixnum-tag*    #x0)

(define *boolean-shift*   3)
(define *boolean-mask*  #x7)
(define *boolean-tag*   #x3)

(define *null-value*    #x1)
(define *false-value* (immediate-rep #f))
(define *true-value*  (immediate-rep #t))

(define *tree-boolean-shift* (tree-constant-get 'i8 (immediate-rep *boolean-shift*)))
(define *tree-boolean-tag*   (tree-constant-get 'i8 (immediate-rep *boolean-tag*)))
