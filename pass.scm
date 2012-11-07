
(declare (unit pass)
         (uses nodes munch sl utils))

(use matchable)
(use srfi-1)

(include "struct-syntax")

;; do some macro expansion

(define (expand e)
   (match e
     (('let (bindings ...) body ...)
      (expand
       `((lambda ,(map car bindings)
           (begin ,@body))
         ,@(map (lambda (binding) (car (cdr binding))) bindings))))
     (('begin) '())
     (('begin body)
      (expand body))
     (('begin body1 body2 ...)
      (expand
       (let ((a (gensym)))
         `((lambda (,a)
             (begin ,@body2)) ,body1))))
     (('or) #f)         
     (('or clause)
      (expand
       (let ((a (gensym)))
         `(let ((,a ,clause))
            (if ,a ,a ,a)))))
     (('or clause1 clause2 ...)
      (expand
       (let ((a (gensym)))
         `(let ((,a ,clause1))
            (if ,a
                ,a
                (or ,@clause2))))))
     (('and) #t)
     (('and clause)
      (expand
       (let ((a (gensym)))
         `(let ((,a ,clause))
            (if ,a ,a ,a)))))
     (('and clause1 clause2 ...)
      (expand
       (let ((a (gensym)))
         `(let ((,a ,clause1))
            (if ,a
                (and ,@clause2)
                ,a)))))
     (('lambda (bindings ...) e1 e2 rest ...)
      (expand `(lambda (,@bindings)
                 (begin ,e1 ,e2 ,@rest))))
     ((combination ...)
      (map expand combination))
     (_ e)))


;; convert to high-level AST
;;
  
(define *primitives*
  '(fx+ fx- fx* fx/ fx< fx> fx<= fx>= fx= car cdr cons null? pair? list? boolean? integer? string?))

(define (primitive? op)
  (if (memq op *primitives*) #t #f))

(define (convert-source e)
  (define (cs e)
    (match e
      (('if x y)
       (make-if (cs x) (cs y) '()))
      (('if x y z)
       (make-if (cs x) (cs y) (cs z)))
      (('if _ ...)
       (error cs "ill-formed conditional expression"))
      (('lambda (bindings ...) body)
       (make-lambda (gensym 'f) bindings (cs body) '()))
      (('lambda _ ...)
       (error cs "ill-formed conditional expression"))
      ((? null?)
       (make-constant e))
      ((? boolean?)
       (make-constant e))
      ((? number?)
       (make-constant e))
      ((_ ...)
       (make-comb (map cs e)))
      ((? symbol?)
       (make-variable e))))
  (cs e))


(define select-matching
  (lambda (fn lst)
    (reverse
     (fold (lambda (x acc)
             (if (fn x)
                 (cons x acc)
                 acc))
           (list)
           lst))))

(define (find-mapping name scopes)
  (let f ((scopes scopes))
    (if (null? scopes)
        (if (primitive? name)
            name
            (error 'alpha-convert "symbol not found" name))
        (let ((mapping (assq name (car scopes))))
          (if mapping
              (cdr mapping)
              (f (cdr scopes)))))))

(define (alpha-convert node)
  (define (alpha-convert node scopes)
    (struct-case node
      ((lambda name args body)
       (let ((mappings (map (lambda (arg) (cons arg (gensym 't))) args)))
         (make-lambda
           name
           (map cdr mappings)
           (alpha-convert body (cons mappings scopes))
           '())))
      ((if test conseq altern)
       (make-if
         (alpha-convert test scopes)
         (alpha-convert conseq scopes)
         (alpha-convert altern scopes)))
      ((prim name args cexp)
       (make-prim
         name
         (map (lambda (arg) (alpha-convert arg scopes)) args)
         (alpha-convert cexp scopes)))
      ((comb args)
       (make-comb
         (map (lambda (arg) (alpha-convert arg scopes)) args)))
      ((variable name)
       (make-variable (find-mapping name scopes)))
      (else node)))
  (alpha-convert node (list)))

(define (cps-convert node)
  (define (cps-convert node cont)
    (struct-case node
      ((constant)
       (if (null? cont)
           node
           (make-comb (list cont node))))
      ((variable)
       (if (null? cont)
           node
           (make-comb (list cont node))))
      ((lambda name args body)
       (let* ((cn (gensym 'c))
              (node-cps (make-lambda
                          name
                          `(,@args ,cn)
                          (cps-convert body (make-variable cn))
                          '())))
         (if (null? cont)
             node-cps
             (make-comb (list cont node-cps)))))
      ((comb args)
       ;; x - combination args
       ;; y - new args for combination (x -> y)
       ;; z - args which are not atoms
       ;; w - names for continuations
       (let f ((x args) (y '()) (z '()) (u '()) (w '()))
         (cond
          ((null? x)
           (let g ((form (make-comb (reverse (cons cont y)))) (y y) (z z) (u u) (w w))
             (if (null? z)
                 form
                 (g
                  (cps-convert (car z)
                               (make-lambda
                                 (car w)
                                 (list (car u))
                                 form
                                 '()))
                  (cdr y)
                  (cdr z)
                  (cdr u)
                  (cdr w)))))
          ((constant? (car x))
           (f (cdr x) (cons (car x) y) z u w))
          ((variable? (car x))
           (f (cdr x) (cons (car x) y) z u w))
          ((lambda? (car x))
           (f (cdr x) (cons (cps-convert (car x) '()) y) z u w))
          (else
           (let ((nm (gensym 't)))
             (f (cdr x) (cons (make-variable nm) y) (cons (car x) z) (cons nm u) (cons (gensym 't) w)))))))
      ((if test conseq altern)
       (let ((kn (gensym 't))
             (fn (gensym 'f))
             (cn (gensym 'f))
             (tn (gensym 't)))
         (make-comb
           (list (make-lambda fn (list kn)
                       (cps-convert test
                                    (make-lambda cn (list tn)
                                          (make-if
                                            (make-variable tn)
                                            (cps-convert conseq (make-variable kn))
                                            (cps-convert altern (make-variable kn)))
                                          '()))
                       '())
                 cont))))
      (else (error 'cps-convert "not an AST node" node))))
  (let ((cn (gensym 'f))
        (tn (gensym 't)))
    (cps-convert
      node
      (make-lambda cn (list tn)
            (make-prim
              (make-variable 'return)
              (list (make-variable tn))
              (make-variable tn) (make-nil))
            '()))))

(define (identify-primitives cexp)
  (struct-case cexp
    ((variable)
     cexp)
    ((constant)
     cexp)
    ((if test conseq altern)
     (make-if
        (identify-primitives test)
        (identify-primitives conseq)
        (identify-primitives altern)))
    ((lambda name args body)
     (make-lambda
      name
      args
      (identify-primitives body)
      '()))
    ((comb args)
     (if (and (variable? (car args)) (primitive? (variable-name (car args))))
         (let* ((cont (car (reverse args)))
                (result (cond
                         ((lambda? cont)
                          (make-variable (car (lambda-args cont))))
                         ((variable? cont)
                          (make-variable (gensym 't)))
                         (else (error 'identify-primitives))))
                (new-cont (cond
                           ((lambda? cont)
                            (identify-primitives (lambda-body cont)))
                           ((variable? cont)
                            (make-app cont (list result)))
                           (else (error 'identify-primitives)))))
           (make-prim
             (car args)
             (cdr (reverse (cdr args)))
             result
             new-cont))
         (make-app (identify-primitives (car args)) (map identify-primitives (cdr args)))))
      ((prim)
       cexp)
      (else (error 'identify-primitives "not an AST node" cexp))))


(define (basic-lambda-lift node)
  ;; raise all lambda definitions to the top of the this lambda body
  (define (filter lst)
    (select-matching
      (lambda (x)
        (lambda? x))
      lst))
  (define (collect node)
    ;; collect nested lambda nodes in this scope
    (struct-case node
      ((lambda)
       (list node))
      ((prim name args result cexp)
       (append (filter args) (collect cexp)))
      ((if test conseq altern)
       (append (collect test) (collect conseq) (collect altern)))
      ((app name args)
       (filter (cons name args)))
      (else (list))))
  (define (rewrite node)
    ;; rewrite tree, replacing lambda nodes with their names
    (struct-case node
      ((lambda name)
       (make-variable name))
      ((prim name args result cexp)
       (make-prim name (map rewrite args) result (rewrite cexp)))
      ((if test conseq altern)
       (make-if (rewrite test) (rewrite conseq) (rewrite altern)))
      ((app name args)
       (make-app (rewrite name) (map rewrite args)))
      (else node)))
  (define (normalize node)
    (if (lambda? node)
        (let* ((name (lambda-name node))
               (args (lambda-args node))
               (body (lambda-body node))
               (defs (map normalize (collect body))))
          (if (null? defs)
              node
              (make-lambda name args (make-fix defs (rewrite body)) '())))
        (let* ((name (gensym 'f))
               (args (list))
               (body node)
               (defs (map normalize (collect body))))
          (if (null? defs)
              node
              (make-lambda name args (make-fix defs (rewrite body)) '())))))
  (let ((node (normalize node)))
    (make-fix (list node)
          (make-app
            (make-variable (lambda-name node)) (list)))))


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
                     labels)))))))


(define (selection-convert-lambda node mod)
 
  (define (start-new-block expr pred-block)
    ;; process the code path referenced by `expr' and return a label to the start block of the newly created subtree
    (let* ((block (sl-make-block (gensym) '() '() '() '() (sl-block-function pred-block))))
      (sl-block-pred-set! block pred-block)
      (sl-block-add-succ! pred-block block)
      (walk-node expr block)
      (sl-make-label (sl-block-name block))))

  (define (convert-atom x)
    (cond
      ((constant? x)
       (sl-make-constant 'i32 (constant-value x)))
      ((variable? x)
       (sl-make-temp (variable-name x)))
      ((label? x)
       (sl-make-label (label-name x)))
      (else
       (assert-not-reached))))

  (define *sl-boolean-shift* (sl-constant-get 'i8 (immediate-rep *boolean-shift*)))
  (define *sl-boolean-tag*   (sl-constant-get 'i8 (immediate-rep *boolean-tag*)))
  
  (define (convert-prim-binop op e1 e2 block)
    (case op
      ((fx+)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (sl-build-add 'i64 e1 e2)))
         t1))
      ((fx-)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (sl-build-sub 'i64 e1 e2)))
         t1))
      ((fx*)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (sl-build-mul 'i64 e1 e2)))
         t1))
      ((fx<=)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (sl-build-cmp 'le e1 e2))
              (t2    (sl-build-shl 'i64 *sl-boolean-shift* t1))
              (t3    (sl-build-ior 'i64 *sl-boolean-tag*   t2)))
         t3))
      ((fx>=)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (sl-build-cmp 'ge e1 e2))
              (t2    (sl-build-shl 'i64 *sl-boolean-shift* t1))
              (t3    (sl-build-ior  'i64 *sl-boolean-tag*   t2)))
         t3))
      ((fx<)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (sl-build-cmp 'lt e1 e2))
              (t2    (sl-build-shl 'i64 *sl-boolean-shift* t1))
              (t3    (sl-build-ior 'i64 *sl-boolean-tag*   t2)))
         t3))
      ((fx>)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (sl-build-cmp 'gt e1 e2))
              (t2    (sl-build-shl 'i64 *sl-boolean-shift* t1))
              (t3    (sl-build-ior 'i64 *sl-boolean-tag*   t2)))
         t3))
      ((fx=)
       (let* ((e1    (convert-atom e1))
              (e2    (convert-atom e2))
              (t1    (sl-build-cmp 'eq e1 e2))
              (t2    (sl-build-shl 'i64 *sl-boolean-shift* t1))
              (t3    (sl-build-ior 'i64 *sl-boolean-tag*   t2)))
         t3))
      (else (assert-not-reached))))

  (define (emit-stores block values base)
    (let f ((values values) (i 0))
      (match values
        (() '())
        ((v . v*)
         (let* ((st  (sl-build-store 'i64 v (sl-build-add 'i32 base (sl-constant-get 'i32 i)))))
          (sl-block-add-statement! block st)
           (f v* (+ i 1)))))))
  
  (define (walk-node node block)
    (struct-case node
      ((select index record name cexp)
       (let* ((record (convert-atom record))
              (t1     (sl-build-load 'i64 (sl-build-add 'i32 record (sl-constant-get 'i32 (* 8 index)))))
              (t2     (sl-build-assign (variable-name name) t1)))
         (sl-block-add-statement! block t2)
         (walk-node cexp block)))
      ((record values name cexp)
       (let* ((heap-ptr (gensym 't))
              (t1   (sl-build-assign heap-ptr (sl-build-load 'i64 (sl-make-label 'heap_ptr))))
              (t2   (sl-build-assign (variable-name name) (sl-make-temp heap-ptr)))
              (t3   (sl-build-store 'i64 
                      (sl-build-add 'i64 
                        (sl-make-temp heap-ptr)
                        (sl-constant-get 'i32 (* 8 (length values))))
                      (sl-make-label 'heap_ptr))))
         (sl-block-add-statement! block t1)
         (sl-block-add-statement! block t2)
         (sl-block-add-statement! block t3)
         (emit-stores block (map (lambda (v) (convert-atom v)) values) (convert-atom name))
         (walk-node cexp block)))
      ((app name args)
       ;; remember to support label targets in future
       (let* ((target (convert-atom name))
              (args (map (lambda (arg)
                           (convert-atom arg))
                         args))
              (len  (length args))
              (t0 (sl-build-call 'tail target args)))
        (sl-block-add-statement! block t0)
         '()))
      ((nil) '())
      ((if test conseq altern)
       (let* ((test (convert-atom test))
              (block1 (start-new-block conseq block))
              (block2 (start-new-block altern block))
              (t0 (sl-build-cmp 'eq test (sl-constant-get 'i32 *false-value*)))
              (t1 (sl-build-brc t0 block1 block2)))
         (sl-block-add-statement! block t1)
         '()))
      ((prim name args result cexp)
       (let ((name   (variable-name name))
             (result (variable-name result)))
         (case name
           ((fx+ fx- fx* fx<= fx>= fx< fx> fx=)
            (let* ((t0 (convert-prim-binop name (first args) (second args) block))
                   (t1 (sl-build-assign result t0)))
             (sl-block-add-statement! block t1)
              (walk-node cexp block)))
           ((return)
            (let* ((e1 (convert-atom (first args)))
                   (t0 (sl-build-add 'i64 e1 (sl-constant-get 'i32 0))))
              (sl-block-add-statement! block t0)
              '()))
           (else (assert-not-reached)))))))

  (struct-case node
    ((lambda name args body)
     (let* ((new-args (map (lambda (arg)
                            (sl-make-temp arg))
                          args))
            (fun (sl-make-function name new-args '() mod))
            (entry (sl-make-block name '() '() '() '() fun)))

       (sl-function-entry-set! fun entry)

       ;; walk the lambda body
       (walk-node body entry)
       fun))))
           
(define (selection-convert node)
  (struct-case node
    ((fix defs body)
     (let* ((mod  (sl-make-module))
            (defs (cons (make-lambda 'MAIN '() body '()) defs)))
       
       ;; convert the definitions into function bodies
       (for-each (lambda (def)
               (sl-module-add-function! mod (selection-convert-lambda def mod)))
                 defs)

    ;;  (sl-module-print mod (current-output-port))
      mod
       ))))

(define (select-instructions module)
  (struct-case module
    ((sl-module functions)
     `(module ,(map select-instructions-for-function functions)))
    (else (error 'select-instructions))))

(define (select-instructions-for-function function)

  (define (walk-block block)
    (let* ((code (box '())))
      ;; munch each statement
      (sl-for-each-statement 
        (lambda (stm)
          (munch-statement stm code))
        block)
      (list 'block (sl-block-name block)
        ;; munch successor blocks
        (map (lambda (succ)
               (walk-block succ))
             (sl-block-succ block))
        (box-ref code)))) 

  (struct-case function
    ((sl-function name args entry module)
     (let ((args (map (lambda (tmp) (sl-temp-name tmp)) args)))
      `(context ,name ,args ,(walk-block entry))))
    (else (error 'select-instructions-for-function))))

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
