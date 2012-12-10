
(declare (unit pass)
         (uses nodes arch tree utils))

(use matchable)
(use srfi-1)

(include "hil-syntax")
(include "struct-syntax")

(define build-sexp
  (lambda (node)
    (node-case node
     ((hil-mod (entry))
      `(module ,(build-sexp entry)))
     ((hil-letfn (defs body))
      `(letfn ,(map build-sexp defs) ,(build-sexp body)))
     ((hil-if (x y z))
      `(if ,(build-sexp x)
           ,(build-sexp y)
           ,(build-sexp z)))
     ((hil-fn (name params body))
      `(fn ,name ,params ,(build-sexp body)))
     ((hil-cont (name body))
      `(cont (,name) ,(build-sexp body)))
   ;;  ((hil-fix defs body)
   ;;   `(fix ,(map write-sexp defs) ,(write-sexp body)))
     ((hil-app (fn args))
      `(app ,(build-sexp fn) ,@(map build-sexp args)))
 ;;    ((app name args)
 ;;     `(app ,(write-sexp name) ,(map write-sexp args)))
 ;;    ((prim name args result cexp)
 ;;     `(prim ,(write-sexp name) ,(map write-sexp args) ,(write-sexp result) ,(write-sexp cexp)))
 ;;    ((nil)
 ;;     `())
     ((hil-const (value)) value)
     ((hil-var   (name))  name)
     ((hil-nil   ())     'nil)
     ((hil-prim  (name params cont)) `(prim ,(build-sexp name) ,(map build-sexp params) ,(build-sexp cont)))
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
     (else (assert-not-reached)))))

;;(define-node lil-mod      (cxts))

;;(define-node lil-const    (type value))
;;(define-node lil-call     (proc args))
;;(define-node lil-brc      (test tl fl))
;;(define-node lil-block    (head tail))
;;(define-node lil-context  (name params start))


(define-node hil-mod    (entry))
(define-node hil-if     (x y z))
(define-node hil-app    (fn args))
(define-node hil-const  (value))
(define-node hil-var    (name))
(define-node hil-prim   (name params cont))
(define-node hil-fn     (name params body))
(define-node hil-cont   (name body))
(define-node hil-nil    ())
(define-node hil-letfn  (defs body))
(define-node hil-letfn  (defs body))

(define (data-set node key val)
 (node-data-set! (cons (cons key val) (node-data node))))

(define (data-ref node key val)
  (cond
    ((assq key (node-data node)) => cdr)
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

(define *prims*
  '(fx+ fx- fx* fx/ fx< fx> fx<= fx>= fx= car cdr cons null? pair? list? boolean? integer? string? __return))

(define (prim? name)
  (and (memq name *prims*) #t))

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
       (hil-fn (gensym 'f) bindings (H body)))
      (('lambda _ ...)
       (error cs "ill-formed conditional expression"))
      ((? null?)
       (hil-const ex))
      ((? boolean?)
       (hil-const ex))
      ((? number?)
       (hil-const ex))
      ((fn ex* ...)
       (hil-app (H fn) (map H ex*)))
      ((? symbol?)
       (hil-var ex))))
  (hil-mod (H ex)))

(define (cps-convert node)
  (define (cps-convert node cont)
    (node-case node
      ((hil-const (value))
       (if (null? cont)
           node
           (hil-app cont (list node))))
      ((hil-var (name))
       (if (null? cont)
           node
           (hil-app cont (list node))))
      ((hil-fn (name params body))
       (let* ((cn       (gensym 'c))
              (node-cps (hil-fn
                          name
                          (cons cn params)
                          (cps-convert body (hil-var cn)))))
         (if (null? cont)
             node-cps
             (hil-app cont (list node-cps)))))
      ((hil-app (fn args))
       ;; x - combination args
       ;; y - new args for combination (x -> y)
       ;; z - args which are not atoms
       ;; w - names for continuations
       (let f ((x (cons fn args)) (y '()) (z '()) (u '()))
         (cond
          ((null? x)
           (let ((form-args (reverse y)))
             (let g ((form (hil-app (car form-args) (cons cont (cdr form-args)))) (y y) (z z) (u u))
                     (if (null? z)
                         form
                         (g
                          (cps-convert (car z)
                                       (hil-cont (car u) form))
                          (cdr y)
                          (cdr z)
                          (cdr u))))))
          ((hil-const? (car x))
           (f (cdr x) (cons (car x) y) z u))
          ((hil-var? (car x))
           (f (cdr x) (cons (car x) y) z u))
          ((hil-fn? (car x))
           (f (cdr x) (cons (cps-convert (car x) '()) y) z u))
          (else
           (let ((nm (gensym 't)))
             (f (cdr x) (cons (hil-var nm) y) (cons (car x) z) (cons nm u)))))))
      ((hil-if (test conseq altern))
       (let ((kn (gensym 't))
             (fn (gensym 'f))
             (tn (gensym 't)))
         (hil-app
           (hil-cont kn
                       (cps-convert test
                                    (hil-cont tn
                                          (hil-if
                                            (hil-var tn)
                                            (cps-convert conseq (hil-var kn))
                                            (cps-convert altern (hil-var kn))))))
            (list cont))))
      (else (error 'cps-convert "not an AST node" node))))
  (let ((cn (gensym 'f))
        (tn (gensym 't)))
    (hil-mod
    (cps-convert
      (hil-mod-entry node)
      (hil-cont tn
        (hil-app
         (hil-var '__return)
         (list (hil-var tn))))))))


;;
;; reduce the administrative reducible expressions produced by the CPS transform
;;
;; Example: ((lambda (x) (... x ...)) v)
;;                                        =>   (... v ...)
;;

(define (beta-reduce node)
  (define (walk-cexp cexp)
    (node-case cexp
     ((hil-mod (body))
      (hil-mod (walk-cexp body)))
     ((hil-var ())
      cexp)
     ((hil-const ())
      cexp)
     ((hil-if (x y z))
      (hil-if
        x
        (walk-cexp y)
        (walk-cexp z)))
     ((hil-fn (name params body))
      (hil-fn
        name
        params
        (walk-cexp body)))
     ((hil-cont (name body))
      (hil-cont name (walk-cexp body)))
     ((hil-app (fn args))
      (let* ((fn   (walk-cexp fn))
             (args (map (lambda (arg)
                          (walk-cexp arg))
                        args)))
        (cond
         ;; Rules for reducing applications
         ;; 1. Reduce if the applied object is a literal lambda AND if no other argument is a literal lambda
         ;; 2. Need to think of some more...
         ((and (hil-fn? fn)
               (fold (lambda (arg x)
                       (if (or (hil-fn? arg) (hil-cont? arg)) #f x))
                     #t
                     args))
           (substitute (hil-fn-body fn) (hil-fn-params fn) args))
         ((and (hil-cont? fn)
               (fold (lambda (arg x)
                       (if (or (hil-fn? arg) (hil-cont? arg)) #f x))
                     #t
                     args))
          (substitute (hil-cont-body fn) (list (hil-cont-name fn)) args))
         (else (hil-app fn args)))))
     ((hil-primc (name params cont))
      (hil-primc name
                 params
                 (walk-cexp cont)))
     ((hil-nil ())
      cexp)
     (else (assert-not-reached))))

 (walk-cexp node))

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

(define (identify-prims cexp)
  (define (F cexp)
    (node-case cexp
      ((hil-mod (body))
       (hil-mod (F body)))
      ((hil-var ())
       cexp)
      ((hil-const ())
       cexp)
      ((hil-if (test conseq altern))
       (hil-if
        (F test)
        (F conseq)
        (F altern)))
      ((hil-fn (name params body))
       (hil-fn
        name
        params
        (F body)))
      ((hil-cont (name body))
       (hil-cont
        name
        (F body)))
      ((hil-app (fn args))
       (if (and (hil-var? fn) (prim? (hil-var-name fn)))
           (let* ((cont     (car args))
                  (cont-new (cond
                             ((hil-fn? cont)
                              (hil-fn (hil-fn-name cont)
                                      (hil-fn-params cont)
                                      (F (hil-fn-body cont))))
                             ((hil-cont? cont)
                              (hil-cont (hil-cont-name cont)
                                        (F (hil-cont-body cont))))
                             ((hil-var? cont)
                              cont)
                             (else (print cont)  (assert-not-reached)))))
             (hil-prim
               fn
               (cdr args)
               cont-new))
           (hil-app (F fn) (map F args))))
      (else (error 'identify-primitives "not an AST node" cexp))))
  (F cexp))

(define select-matching
  (lambda (fn lst)
    (reverse
     (fold (lambda (x acc)
             (if (fn x)
                 (cons x acc)
                 acc))
           (list)
           lst))))

(define (raise-fns node)
  ;; raise all lambda definitions to the top of the this lambda body
  (define (filter lst)
    (select-matching
      (lambda (x)
        (or (hil-fn? x) (hil-cont? x)))
      lst))
  (define (collect node)
    ;; collect nested lambda nodes in this scope
    (node-case node
      ((hil-fn ())
       (list node))
      ((hil-cont ())
       (list node))
      ((hil-prim (name args cexp))
       (append (filter args) (collect cexp)))
      ((hil-if (test conseq altern))
       (append (collect test) (collect conseq) (collect altern)))
      ((hil-app (name args))
       (filter (cons name args)))
      (else (list))))
  (define (rewrite node)
    ;; rewrite tree, replacing lambda nodes with their names
    (node-case node
      ((hil-fn (name))
       (hil-var name))
      ((hil-cont (name))
       (hil-var name))
      ((hil-prim (name args cexp))
       (hil-prim name (map rewrite args) (rewrite cexp)))
      ((hil-if (test conseq altern))
       (hil-if (rewrite test) (rewrite conseq) (rewrite altern)))
      ((hil-app (fn args))
       (hil-app (rewrite fn) (map rewrite args)))
      (else node)))
  (define (normalize node)
    (cond
      ((hil-fn? node)
        (let* ((name (hil-fn-name node))
               (args (hil-fn-params node))
               (body (hil-fn-body node))
               (defs (map normalize (collect body))))
          (if (null? defs)
              node
              (hil-fn name args (hil-letfn defs (rewrite body))))))
      ((hil-cont? node)
       (let* ((name (hil-cont-name node))
              (body (hil-cont-body node))
              (defs (map normalize (collect body))))
         (if (null? defs)
             node
             (hil-cont name (hil-letfn defs (rewrite body))))))
      (else
        (let* ((name (gensym 'f))
               (args (list))
               (body node)
               (defs (map normalize (collect body))))
          (if (null? defs)
              node
              (hil-letfn defs (rewrite body)))))))
  (let ((node (normalize (hil-mod-entry node))))
    (cond
      ((hil-fn? node)
       (hil-letfn (list node)
         (hil-app
           (hil-var (hil-fn-name node)) (list))))
      ((hil-cont? node)
       (hil-letfn (list node)
                  (hil-app
                   (hil-var (hil-cont-name node)) (list))))
      (else node))))

(define (analyze-free-vars node)
  (let ((union (lambda lists
                 (apply lset-union (cons eq? lists))))
        (diff  (lambda lists
                 (apply lset-difference (cons eq? lists)))))
    (struct-case node
      ((variable name)
       (diff (list name) *primitives*))
      ((constant) '())
      ((if test conseq altern)
       (union
        (analyze-free-vars test)
        (analyze-free-vars conseq)
        (analyze-free-vars altern)))
      ((app name args)
       (let f ((x '()) (args (cons name args)))
         (if (null? args)
             x
             (f (union x (analyze-free-vars (car args))) (cdr args)))))
      ((prim name args result cexp)
       (let f ((x (list)) (args args))
         (if (null? args)
             (union x (diff (analyze-free-vars cexp) (list (variable-name result))))
             (f (union x (analyze-free-vars (car args))) (cdr args)))))
      ((fix defs body)
       (apply union (append (map analyze-free-vars defs)
                            (list (diff (analyze-free-vars body)
                                        (map (lambda (def)
                                               (lambda-name def))
                                             defs))))))
      ((lambda name args body)
       (lambda-free-vars-set! node
                              (diff (analyze-free-vars body) args))
       (lambda-free-vars node))
      ((label)
       (list))
      ((nil)
       (list))
      (else (error 'analyze-free-vars "not an AST node" node)))))

(define (closure-index name names)
  (let f ((i 1) (names names))
    (if (null? names)
        (error "should not reach here")
        (if (eq? name (car names))
            i
            (f (+ i 1) (cdr names))))))


(define (primitive-application args)
  (let ((fun (car args)))
    (if (memq (variable-name fun) *primitives*)
        (error 'primitive-application "should not reach here" (variable-name fun))
        (let ((fn (gensym 't)))
          (make-select 0 fun (make-variable fn)
                (make-app (make-variable fn) (cons fun (cdr args))))))))

;;
;; assume all functions escape -> each function takes a closure arg
;; optimize application of known functions by jumping directly to the function's label instead of using the function ptr stored in the function's closure.
;;

#| * 1. create closure record for each lambda
   * 2. In fix body, replace each lambda reference (those not in the operator position) with closure reference
   * 3. rewrite lambda applications. Extract function label from closure and apply function to closure + args
 |#

(define (closure-convert-body node c-name free-vars)
  (struct-case node
    ((fix defs body)
     (let ((old-names (map (lambda (def)
                             (lambda-name def))
                           defs))
           (x (map (lambda (def)
                     (closure-convert-body def c-name free-vars))
                   defs)))
       (make-fix
         x
         (closure-convert-body
          (let f ((x x) (y old-names) (form body))
            (if (null? x)
                form
                (f (cdr x)
                   (cdr y)
                   #| when making the closure record, we use a label node to represent the lambda's function ptr |#
                   (make-record
                     (cons (make-label (lambda-name (car x)))
                           (map (lambda (v) (make-variable v))
                                (lambda-free-vars (car x))))
                     (make-variable (car y))
                     form))))
          c-name free-vars))))
    ((lambda name args body free-vars)
     (let* ((cn (gensym 'c))
            (converted (make-lambda (gensym 'f) (cons cn args) (closure-convert-body body cn free-vars) '())))
       (lambda-free-vars-set! converted free-vars)
       converted))
    ((app name args)
     (let f ((x (cons name args)) (y '()) (z '()))
       (cond
        ((null? x)
         (let g ((cexp (primitive-application (reverse y))) (z z))
           (if (null? z)
               cexp
               (g (make-select
                    (closure-index (caar z) free-vars)
                    (make-variable c-name)
                    (make-variable (cdar z))
                    cexp)
                  (cdr z)))))
         ((variable? (car x))
          (let ((name (variable-name (car x))))
            (if (memq name free-vars)
                (let ((exists (assq name z)) (temp (gensym 't)))
                  (if exists
                    (f (cdr x) (cons (make-variable (cdr exists)) y) z)
                    (f (cdr x) (cons (make-variable temp) y) (cons (cons name temp) z))))
                (f (cdr x) (cons (car x) y) z))))
         (else (f (cdr x) (cons (closure-convert-body (car x) #f '()) y) z)))))
   ((prim name args result cexp)
     (let f ((x args) (y (list)) (z (list)))
       (if (null? x)
           (let g ((cexp (make-prim name (reverse y) result (closure-convert-body cexp c-name free-vars))) (z z))
             (if (null? z)
                 cexp
                 (g (make-select
                      (closure-index (caar z) free-vars)
                      (make-variable c-name)
                      (make-variable (cdar z))
                      cexp)
                    (cdr z))))
           (if (variable? (car x))
               (let ((name (variable-name (car x))))
                 (if (memq name free-vars)
                     (let ((exists (assq name z)) (temp (gensym 't)))
                       (if exists
                    (f (cdr x) (cons (make-variable (cdr exists)) y) z)
                    (f (cdr x) (cons (make-variable temp) y) (cons (cons name temp) z))))
                     (f (cdr x) (cons (car x) y) z)))
               (f (cdr x) (cons (closure-convert-body (car x) #f '()) y) z)))))
   ((record values name cexp)
     (let f ((x values) (y (list)) (z (list)))
       (if (null? x)
           (let g ((cexp (make-record (reverse y) name (closure-convert-body cexp c-name free-vars))) (z z))
             (if (null? z)
                 cexp
                 (g (make-select
                      (closure-index (caar z) free-vars)
                      (make-variable c-name)
                      (make-variable (cdar z))
                      cexp)
                    (cdr z))))
           (struct-case (car x)
             ((variable name)
              (if (memq name free-vars)
                  (let ((exists (assq name z)) (temp (gensym 't)))
                    (if exists
                        (f (cdr x) (cons (make-variable (cdr exists)) y) z)
                        (f (cdr x) (cons (make-variable temp) y) (cons (cons name temp) z))))
                  (f (cdr x) (cons (car x) y) z)))
             ((label name)
              (f (cdr x) (cons (car x) y) z))
             (else (error 'closure-convert-body (car x)))))))
    ((variable name)
     (if (memq name free-vars)
         (let ((temp (gensym 't)))
           (make-select
             (closure-index name free-vars)
             (make-variable c-name)
             (make-variable temp)
             (make-variable temp)))
         node))
    ((if test conseq altern)
     (make-if
       (closure-convert-body test c-name free-vars)
       (closure-convert-body conseq c-name free-vars)
       (closure-convert-body altern c-name free-vars)))
    ((constant)
     node)
    ((label)
     node)
    ((nil)
     node)
    (else (error 'closure-convert "node" node))))

(define (closure-convert node)
  (analyze-free-vars node)
  (closure-convert-body node #f '()))

(define (flatten node)
  (letrec ((queue (list))
           (queue-push! (lambda (node)
                          (set! queue
                                (append queue (list node)))))
           (queue-pop!  (lambda ()
                          (let ((front (car queue)))
                            (set! queue
                                  (cdr queue))
                           front)))
           (queue-empty? (lambda ()
                           (null? queue)))
           (flatten-node (lambda (node)
                           (struct-case node
                             ((fix defs body)
                              (let f ((x defs))
                                (if (null? x)
                                    body
                                    (begin
                                      (queue-push! (car x))
                                      (f (cdr x))))))
                             (else node)))))
    (cond
      ((fix? node)
       (map queue-push! (fix-defs node))
       (let f ((labels (list)))
        (if (queue-empty?)
          (make-fix labels (fix-body node))
          (let* ((node (queue-pop!)))
            (f (cons (make-lambda
                       (lambda-name node)
                       (lambda-args node)
                       (flatten-node (lambda-body node))
                       '())
                     labels))))))
      (else (make-fix (list) node)))))



(define (selection-convert-lambda node mod)

  (define (start-new-block expr pred-block)
    ;; process the code path referenced by `expr' and return a label to the start block of the newly created subtree
    (let* ((block (tree-make-block (gensym) '() '() '() '() (tree-block-function pred-block))))
      (tree-block-pred-set! block pred-block)
      (tree-block-add-succ! pred-block block)
      (walk-node expr block)
      (tree-make-label (tree-block-name block))))

  (define (convert-atom x)
    (cond
      ((constant? x)
       (tree-make-constant 'i32 (constant-value x)))
      ((variable? x)
       (tree-make-temp (variable-name x)))
      ((label? x)
       (tree-make-label (label-name x)))
      (else
       (assert-not-reached))))

  (define *tree-boolean-shift* (tree-constant-get 'i8 (immediate-rep *boolean-shift*)))
  (define *tree-boolean-tag*   (tree-constant-get 'i8 (immediate-rep *boolean-tag*)))

  (define (convert-prim-binop op e1 e2 block)
    (case op
      ((fx+)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (tree-build-add 'i64 e1 e2)))
         t1))
      ((fx-)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (tree-build-sub 'i64 e1 e2)))
         t1))
      ((fx*)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (tree-build-mul 'i64 e1 e2)))
         t1))
      ((fx<=)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (tree-build-cmp 'le e1 e2))
              (t2    (tree-build-shl 'i64 *tree-boolean-shift* t1))
              (t3    (tree-build-ior 'i64 *tree-boolean-tag*   t2)))
         t3))
      ((fx>=)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (tree-build-cmp 'ge e1 e2))
              (t2    (tree-build-shl 'i64 *tree-boolean-shift* t1))
              (t3    (tree-build-ior  'i64 *tree-boolean-tag*   t2)))
         t3))
      ((fx<)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (tree-build-cmp 'lt e1 e2))
              (t2    (tree-build-shl 'i64 *tree-boolean-shift* t1))
              (t3    (tree-build-ior 'i64 *tree-boolean-tag*   t2)))
         t3))
      ((fx>)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (tree-build-cmp 'gt e1 e2))
              (t2    (tree-build-shl 'i64 *tree-boolean-shift* t1))
              (t3    (tree-build-ior 'i64 *tree-boolean-tag*   t2)))
         t3))
      ((fx=)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (tree-build-cmp 'eq e1 e2))
              (t2    (tree-build-shl 'i64 *tree-boolean-shift* t1))
              (t3    (tree-build-ior 'i64 *tree-boolean-tag*   t2)))
         t3))
      (else (assert-not-reached))))

  (define (emit-stores block values base)
    (let f ((values values) (i 0))
      (match values
        (() '())
        ((v . v*)
           (tree-block-add-statement! block
             (cond
               ((tree-constant? v)
                (tree-build-store 'i64 v (tree-build-add 'i32 base (tree-constant-get 'i32 (* 8 i)))))
               ((tree-temp? v)
                (tree-build-store 'i64
                  v (tree-build-add 'i32 base (tree-constant-get 'i32 (* 8 i)))))
               ((tree-label? v)
                (tree-build-store 'i64
                  (tree-build-load 'ptr64 v) (tree-build-add 'i32 base (tree-constant-get 'i32 (* 8 i)))))
               (else (assert-not-reached))))

             (f v* (+ i 1))))))

  (define (walk-node node block)
    (struct-case node
      ((select index record name cexp)
       (let* ((record (convert-atom record))
              (t1     (tree-build-load 'i64 (tree-build-add 'i32 record (tree-constant-get 'i32 (* 8 index)))))
              (t2     (tree-build-assign (variable-name name) t1)))
         (tree-block-add-statement! block t2)
         (walk-node cexp block)))
      ((record values name cexp)
       (let* ((heap-ptr (gensym 't))
              (t1   (tree-build-assign heap-ptr (tree-build-load 'i64 (tree-make-label 'heap_ptr))))
              (t2   (tree-build-assign (variable-name name) (tree-make-temp heap-ptr)))
              (t3   (tree-build-store 'i64
                      (tree-build-add 'i64
                        (tree-make-temp heap-ptr)
                        (tree-constant-get 'i32 (* 8 (length values))))
                      (tree-make-label 'heap_ptr))))
         (tree-block-add-statement! block t1)
         (tree-block-add-statement! block t2)
         (tree-block-add-statement! block t3)
         (emit-stores block (map (lambda (v) (convert-atom v)) values) (convert-atom name))
         (walk-node cexp block)))
      ((app name args)
       ;; remember to support label targets in future
       (let* ((target (convert-atom name))
              (args (map (lambda (arg)
                           (convert-atom arg))
                         args))
              (len  (length args))
              (t0 (tree-build-call 'tail target args)))
        (tree-block-add-statement! block t0)
         '()))
      ((nil) '())
      ((if test conseq altern)
       (let* ((test (convert-atom test))
              (block1 (start-new-block conseq block))
              (block2 (start-new-block altern block))
              (t0 (tree-build-cmp 'eq test (tree-constant-get 'i64 *false-value*)))
              (t1 (tree-build-brc t0 block1 block2)))
         (tree-block-add-statement! block t1)
         '()))
      ((prim name args result cexp)
       (let ((name   (variable-name name))
             (result (variable-name result)))
         (case name
           ((fx+ fx- fx* fx<= fx>= fx< fx> fx=)
            (let* ((t0 (convert-prim-binop name (first args) (second args) block))
                   (t1 (tree-build-assign result t0)))
             (tree-block-add-statement! block t1)
              (walk-node cexp block)))
           ((return)
            (let* ((e1 (convert-atom (first args)))
                   (t0 (tree-build-return e1)))
              (tree-block-add-statement! block t0)))
           (else (assert-not-reached)))))))

  (struct-case node
    ((lambda name args body)
     (let* ((new-args (map (lambda (arg)
                            (tree-make-temp arg))
                          args))
            (fun (tree-make-function name new-args '() mod))
            (entry (tree-make-block name '() '() '() '() fun)))

       (tree-function-entry-set! fun entry)

       ;; walk the lambda body
       (walk-node body entry)
       fun))))

(define (tree-convert node)
  (struct-case node
    ((fix defs body)
     (let* ((mod  (tree-make-module))
            (defs (cons (make-lambda 'begin '() body '()) defs)))

       ;; convert the definitions into function bodies
       (for-each (lambda (def)
               (tree-module-add-function! mod (selection-convert-lambda def mod)))
                 defs)
      mod
       ))))

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
