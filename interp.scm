(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        ;(let-exp (var exp1 body)       
        ;  (let ((val1 (value-of exp1 env)))
        ;    (value-of body
        ;      (extend-env var val1 env))))

        ;; Exercise 3.11*
        (unop-exp (rator rand)
          (let ((val1 (value-of rand env)))
            (cases unary-op rator
              (arith-1-op (rator)
                (let ((num1 (expval->num val1)))
                  (cases unary-arith-op rator
                    (zero-op () (bool-val (zero? num1)))
                    (neg-op () (num-val (- num1)))
                    )))
              (list-1-op (rator)
                (let ((lst1 (expval->list val1)))
                  (cases unary-list-op rator
                    (car-op () (car lst1))
                    (cdr-op () (list-val (cdr lst1)))
                    (null-op () (bool-val (null? lst1)))
                    )))
              (print-op ()
                (cases expval val1
                  (num-val (num1) (display num1))
                  (bool-val (flg) (display flg))
                  (list-val (lst) (display lst))
                  (ratio-val (num den) (display (num / den)))
                  )
                (newline)
                (num-val 1))
              (log-1-op (rator)
                (let ((z (expval->bool val1)))
                  (cases unary-logic-op rator
                    (not-op () (bool-val (not z)))
                    )))
              (rat-1-op (rator)
                (let ((rat (expval->rational val1)))
                  (cases unary-ratio-op rator
                    (prop-op ()
                      (bool-val (< (numerator rat) (denominator rat))))
                    (improp-op ()
                      (bool-val (>= (numerator rat) (denominator rat))))
                    (recip-op ()
                      (ratio-val (denominator rat)
                                 (numerator rat)))
                     )))
                  )))
        ;; Exercise 3.11*
        (binop-exp (rator rand1 rand2)
          (let ((val1 (value-of rand1 env))
                (val2 (value-of rand2 env)))
            (cases binary-op rator
              (arith-2-op (rator)
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (cases bin-arith-op rator
                    (diff-op () (num-val (- num1 num2)))
                    (plus-op () (num-val (+ num1 num2)))
                    (mult-op () (num-val (* num1 num2)))
                    (quot-op () (num-val (quotient num1 num2)))
                    )))
              (reln-op (rator)
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (cases bin-reln-op rator
                    (eq-op () (bool-val (= num1 num2)))
                    (lt-op () (bool-val (< num1 num2)))
                    (gt-op () (bool-val (> num1 num2)))
                    )))
              (pair-op ()
                (let ((lst2 (expval->list val2)))
                  (list-val (cons val1 lst2))))
              (log-2-op (rator)
                (let ((z1 (expval->bool val1))
                      (z2 (expval->bool val2)))
                  (cases bin-logic-op rator
                    (and-op () (bool-val (and z1 z2)))
                    (or-op () (bool-val (or z1 z2)))
                    )))
              )))
        ;; Exercise 3.11*
        (nop-exp (rator rands)
          (let ((vals (map (lambda (exp) (value-of exp env)) rands)))
            (cases nary-op rator
              (list-op (list-val vals))
            )))

        ;; Exercise 3.6*
        ;;
        ;; SPEC: NumVal -> NumVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;    < exp1, ρ > ⇓ val1
        ;;   --------------------------------
        ;;    < minus(exp1), ρ > ⇓ ⌈-⌊val1⌋⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;    (value-of exp1 env) = val1
        ;;    (expval->num val1) = num1
        ;;   ----------------------------------------------------
        ;;    (value-of (neg-exp exp1) env) = (num-val (- num1))
        
        ;; Exercise 3.7*
        ;;
        ;; SPEC: NumVal x NumVal -> NumVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;    < exp1, ρ > ⇓ val1
        ;;    < exp2, ρ > ⇓ val2
        ;;   ---------------------------------------
        ;;    < +(exp1,exp2), ρ > ⇓ ⌈⌊val1⌋+⌊val2⌋⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;    (value-of exp1 env) = val1
        ;;    (value-of exp2 env) = val2
        ;;    (expval->num val1) = num1
        ;;    (expval->num val2) = num2
        ;;   --------------------------------------------------------------
        ;;    (value-of (sum-exp exp1 exp2) env) = (num-val (+ num1 num2))
        ;; SPEC: NumVal x NumVal -> NumVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;    < exp1, ρ > ⇓ val1
        ;;    < exp2, ρ > ⇓ val2
        ;;   ---------------------------------------
        ;;    < *(exp1,exp2), ρ > ⇓ ⌈⌊val1⌋*⌊val2⌋⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;    (value-of exp1 env) = val1
        ;;    (value-of exp2 env) = val2
        ;;    (expval->num val1) = num1
        ;;    (expval->num val2) = num2
        ;;   --------------------------------------------------------------
        ;;    (value-of (prod-exp exp1 exp2) env) = (num-val (* num1 num2))
        ;; SPEC: NumVal x NumVal -> NumVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;    < exp1, ρ > ⇓ val1
        ;;    < exp2, ρ > ⇓ val2
        ;;   ----------------------------------------------
        ;;    < /(exp1,exp2), ρ > ⇓ ⌈floor(⌊val1⌋/⌊val2⌋)⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;    (value-of exp1 env) = val1
        ;;    (value-of exp2 env) = val2
        ;;    (expval->num val1) = num1
        ;;    (expval->num val2) = num2
        ;;   ----------------------------------------------------------------------
        ;;    (value-of (quot-exp exp1 exp2) env) = (num-val (quotient num1 num2))
        ;; Exercise 3.8
        ;;
        ;; SPEC: NumVal x NumVal -> BoolVal
        ;;
        ;; Op Sem Rules [source-notation]:
        ;;    < exp1, ρ > ⇓ val1
        ;;    < exp2, ρ > ⇓ val2
        ;;    ⌊val1⌋ = ⌊val2⌋
        ;;   ---------------------------------------
        ;;    < equal?(exp1,exp2), ρ > ⇓ ⌈T⌉
        ;;
        ;;    < exp1, ρ > ⇓ val1
        ;;    < exp2, ρ > ⇓ val2
        ;;    ⌊val1⌋ ≠ ⌊val2⌋
        ;;   ---------------------------------------
        ;;    < equal?(exp1,exp2), ρ > ⇓ ⌈F⌉
        ;;
        ;; Op Sem Rules [host-notation]:
        ;;    (value-of exp1 env) = val1
        ;;    (value-of exp2 env) = val2
        ;;    (expval->num val1) = num1
        ;;    (expval->num val2) = num2
        ;;    (= num1 num2) = #t
        ;;   ---------------------------------------------------
        ;;    (value-of (eq-exp exp1 exp2) env) = (bool-val #t)
        ;;
        ;;    (value-of exp1 env) = val1
        ;;    (value-of exp2 env) = val2
        ;;    (expval->num val1) = num1
        ;;    (expval->num val2) = num2
        ;;    (= num1 num2) = #f
        ;;   ---------------------------------------------------
        ;;    (value-of (eq-exp exp1 exp2) env) = (bool-val #f)
        ;; SPEC: NumVal x NumVal -> BoolVal
        ;;
        ;; Op Sem Rules [source-notation]:
        ;;    < exp1, ρ > ⇓ val1
        ;;    < exp2, ρ > ⇓ val2
        ;;    ⌊val1⌋ < ⌊val2⌋
        ;;   ---------------------------------------
        ;;    < less?(exp1,exp2), ρ > ⇓ ⌈T⌉
        ;;
        ;;    < exp1, ρ > ⇓ val1
        ;;    < exp2, ρ > ⇓ val2
        ;;    ⌊val1⌋ ≮ ⌊val2⌋
        ;;   ---------------------------------------
        ;;    < less?(exp1,exp2), ρ > ⇓ ⌈F⌉
        ;;
        ;; Op Sem Rules [host-notation]:
        ;;    (value-of exp1 env) = val1
        ;;    (value-of exp2 env) = val2
        ;;    (expval->num val1) = num1
        ;;    (expval->num val2) = num2
        ;;    (< num1 num2) = #t
        ;;   ---------------------------------------------------
        ;;    (value-of (lt-exp exp1 exp2) env) = (bool-val #t)
        ;;
        ;;    (value-of exp1 env) = val1
        ;;    (value-of exp2 env) = val2
        ;;    (expval->num val1) = num1
        ;;    (expval->num val2) = num2
        ;;    (< num1 num2) = #f
        ;;   ---------------------------------------------------
        ;;    (value-of (lt-exp exp1 exp2) env) = (bool-val #f)
        ;; SPEC: NumVal x NumVal -> BoolVal
        ;;
        ;; Op Sem Rules [source-notation]:
        ;;    < exp1, ρ > ⇓ val1
        ;;    < exp2, ρ > ⇓ val2
        ;;    ⌊val1⌋ > ⌊val2⌋
        ;;   ---------------------------------------
        ;;    < greater?(exp1,exp2), ρ > ⇓ ⌈T⌉
        ;;
        ;;    < exp1, ρ > ⇓ val1
        ;;    < exp2, ρ > ⇓ val2
        ;;    ⌊val1⌋ ≯ ⌊val2⌋
        ;;   ---------------------------------------
        ;;    < greater?(exp1,exp2), ρ > ⇓ ⌈F⌉
        ;;
        ;; Op Sem Rules [host-notation]:
        ;;    (value-of exp1 env) = val1
        ;;    (value-of exp2 env) = val2
        ;;    (expval->num val1) = num1
        ;;    (expval->num val2) = num2
        ;;    (> num1 num2) = #t
        ;;   ---------------------------------------------------
        ;;    (value-of (gt-exp exp1 exp2) env) = (bool-val #t)
        ;;
        ;;    (value-of exp1 env) = val1
        ;;    (value-of exp2 env) = val2
        ;;    (expval->num val1) = num1
        ;;    (expval->num val2) = num2
        ;;    (> num1 num2) = #f
        ;;   ---------------------------------------------------
        ;;    (value-of (gt-exp exp1 exp2) env) = (bool-val #f)
        ;; Exercise 3.9
        ;;
        ;; SPEC: ExpVal x ListVal -> ListVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;   < exp1, ρ > ⇓ val1
        ;;   < exp2, ρ > ⇓ ⌈(...)⌉
        ;;  ---------------------------------------
        ;;   < cons(exp1,exp2), ρ > ⇓ ⌈(val1,...)⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of exp1 env) = val1
        ;;   (value-of exp2 env) = val2
        ;;   (expval->list val2) = lst2
        ;;  -------------------------------------------------------------------
        ;;   (value-of (cons-exp exp1 exp2) env) = (list-val (cons val1 lst2))
        ;; SPEC: ListVal -> ExpVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;   < exp1, ρ > ⇓ ⌈(val1,...)⌉
        ;;  ----------------------------
        ;;   < car(exp1), ρ > ⇓ val1
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of exp1 env) = val1
        ;;   (expval->list val1) = lst1
        ;;  -------------------------------------------------------------------
        ;;   (value-of (car-exp exp1) env) = (car lst1)
        ;; SPEC: ListVal -> ListVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;   < exp1, ρ > ⇓ ⌈(val1,val2,...)⌉      < exp1, ρ > ⇓ ⌈(val1)⌉
        ;;  ---------------------------------    -------------------------
        ;;   < cdr(exp1), ρ > ⇓ ⌈(val2,...)⌉      < cdr(exp1), ρ > ⇓ ⌈()⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of exp1 env) = val1
        ;;   (expval->list val1) = lst1
        ;;  -------------------------------------------------------
        ;;   (value-of (car-exp exp1) env) = (list-val (cdr lst1))
        ;; SPEC: ListVal -> BoolVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;   < exp1, ρ > ⇓ ⌈(val1,...)⌉      < exp1, ρ > ⇓ ⌈()⌉
        ;;  ----------------------------    --------------------------
        ;;   < null?(exp1), ρ > ⇓ ⌈F⌉        < null?(exp1), ρ > ⇓ ⌈T⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of exp1 env) = val1
        ;;    (expval->list val1) = lst1
        ;;  -----------------------------------------------------------
        ;;   (value-of (null?-exp exp1) env) = (bool-val (null? lst1))
        ;; SPEC: ListVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;   < emptylist, ρ > ⇓ ⌈()⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of (empty-exp) env) = (list-val '())
        (empty-exp () (list-val '()))

        ;; Exercise 3.10
        ;;
        ;; SPEC: ExpVal x ExpVal ... -> ListVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;   < list(), ρ > ⇓ ⌈()⌉
        ;;
        ;;   < exp1, ρ > ⇓ val1
        ;;   < list(...), ρ > ⇓ ⌈(...)⌉
        ;;  --------------------------------------
        ;;   < list(exp1,...), ρ > ⇓ ⌈(val1,...)⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (null? exps) = #t
        ;;  -------------------------------------------------
        ;;   (value-of (list-exp exps) env) = (list-val '())
        ;;
        ;;   (value-of (car exps) env) = val1
        ;;   (value-of (list-exp (cdr exps)) env) = val2
        ;;  --------------------------------------------------------------------------------
        ;;   (value-of (list-exp exps) env) = (list-val (cons val1 (expval->list val2)))

        ;; Exercise 3.12
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;   < test1, ρ > ⇓ ⌈T⌉
        ;;   < exp1, ρ > ⇓ val1
        ;;  ----------------------------------------------------------------------------------------
        ;;   < cond test1 ==> exp1 test2 ==> exp2 ... end, ρ > ⇓ val1
        ;;
        ;;   < test1, ρ > ⇓ ⌈F⌉
        ;;  ----------------------------------------------------------------------------------------
        ;;   < cond test1 ==> exp1 test2 ==> exp2 ... end, ρ > ⇓ < cond test2 ==> exp2 ... end, ρ >
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of (car tests) env) = result1
        ;;   (expval->bool result1) = #t
        ;;   (value-of (car exps) env) = val1
        ;;  ---------------------------------------------
        ;;   (value-of (cond-exp tests exps) env) = val1
        ;;
        ;;   (value-of (car tests) env) = result1
        ;;   (expval->bool result1) = #f
        ;;  -----------------------------------------------------------------------------------------
        ;;   (value-of (cond-exp tests exps) env) = (value-of (cond-exp (cdr tests) (cdr exps)) env)
        (cond-exp (tests exps)
                  (let ((result1 (value-of (car tests) env)))
                    (if (expval->bool result1)
                        (value-of (car exps) env)
                        (value-of (cond-exp (cdr tests) (cdr exps)) env))))

        ;; Exercise 3.15
        ;;
        ;; SPEC: ExpVal -> NumVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;   < exp1, ρ > ⇓ val1
        ;;  --------------------------  but what about the side-effect?
        ;;   < print(exp1), ρ > ⇓ ⌈1⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of exp1 env) = val1
        ;;  -----------------------------------------------
        ;;   (value-of (print-exp exp1) env) = (num-val 1)

        ;; Exercise 3.16**
        ;;
        ;; SPEC: (Id,ExpVal)* -> ExpVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;   < exp1, ρ > ⇓ ⌈val1⌉
        ;;     ...
        ;;   < expn, ρ > ⇓ ⌈valn⌉
        ;;   < body, [idn=valn]...[id1=val1]ρ >  ⇓ ⌈val⌉
        ;;  ---------------------------------------------
        ;;   < let id1 = exp1 ... idn = expn in body, ρ > ⇓ ⌈val⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (map (lambda (exp) (value-of exp env)) exps) = vals
        ;;   (value-of body (extend-env* ids vals env)) = val
        ;;  ---------------------------------------------------------------------------------------
        ;;   (value-of (let-exp ids exps body) env) = val
        (let-exp (vars exps body)
                 (if (null? vars)
                     (value-of body env)
                     (let ((vals (map (lambda (exp) (value-of exp env)) exps)))
                       (value-of body (extend-env* vars vals env)))))

        ;; Exercise 3.17
        ;; Op Sem Rule [source-notation]:
        ;;   < body, ρ >  ⇓ ⌈val1⌉
        ;;  -------------------------------------------
        ;;   < let* in body, ρ > ⇓ ⌈val1⌉
        ;;
        ;;   < exp1, ρ > ⇓ ⌈val1⌉
        ;;   < let* ... in body, [id1=val1]ρ >  ⇓ ⌈val2⌉
        ;;  -------------------------------------------
        ;;   < let* id1 = exp1 ... in body, ρ > ⇓ ⌈val2⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   ids = '()
        ;;   exps = '()
        ;;   (value-of body env) = val1
        ;;  --------------------------------------------------------------
        ;;   (value-of (let*-exp ids exps body) env) = val1
        ;;
        ;;   (value-of (car exps) env) = val1
        ;;   (value-of (let*-exp (cdr ids) (cdr exps) body) (extend-env (car ids) val1 env)) = val2
        ;;  ---------------------------------------------------------------------------------------
        ;;   (value-of (let*-exp ids exps body) env) = val2
        (let*-exp (vars exps body)
                 (if (null? vars)
                     (value-of body env)
                     (let ((val1 (value-of (car exps) env)))
                       (value-of (let*-exp (cdr vars) (cdr exps) body) (extend-env (car vars) val1 env)))))

        ;; Exercise 3.18
        ;;   < exp1, ρ > ⇓ ⌈()⌉
        ;;   < body, ρ > ⇓ val
        ;;  -------------------------------------
        ;;   < unpack = exp1 in body, ρ > ⇓ val
        ;;
        ;;   < exp1, ρ > ⇓ ⌈(val1,...)⌉
        ;;   < unpack ... = ist(...), ρ > ⇓ ⌈(...)⌉
        ;;  ---------------------------------------------------
        ;;   < unpack id1 ... = exp1 in body, ρ > ⇓ ⌈(val1,...)⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of rhs env) = val1
        ;;   (expval->list val1) = vals
        ;;   (null? vals) = #t
        ;;   (value-of body env) = val2
        ;;  -------------------------------------------------
        ;;   (value-of (unpack-exp ids rhs body) env) = val2
        ;;
        ;;   (value-of rhs env) = val1
        ;;   (expval->list val1) = vals
        ;;   (null? vals) = #f
        ;;   (value-of body (extend-env* ids vals env)) = val2
        ;;  ---------------------------------------------------
        ;;   (value-of (unpack-exp ids rhs body) env) = val2
        ;;
        ;;  where extend-env* is specified as in environments.scm
        (unpack-exp (ids rhs body)
                 (let ((lst (value-of rhs env)))
                   (let ((vals (expval->list lst)))
                     (value-of body (extend-env* ids vals env)))))

        ;; In-Class Exercise 3.A
        ;; Spec: BoolVal
        ;; Op Sem Rule [source-notation]:
        ;;   < true, ρ > ⇓ ⌈True⌉
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of (true-exp) env) = (bool-val #t)
        (true-exp () (bool-val #t))
        ;; Spec: BoolVal
        ;; Op Sem Rule [source-notation]:
        ;;   < false, ρ > ⇓ ⌈False⌉
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of (false-exp) env) = (bool-val #f)
        (false-exp () (bool-val #f))

        ;; In-Class Exercise 3.B
        ;; Spec: BoolVal -> BoolVal
        ;;
        ;; Op Sem Rule [source-notation]:
        ;;   < exp1, ρ > ⇓ ⌈True⌉
        ;;   =========================
        ;;   < not(exp1), ρ > ⇓ ⌈False⌉
        ;;
        ;;   < exp1, ρ > ⇓ ⌈False⌉
        ;;   =========================
        ;;   < not(exp1), ρ > ⇓ ⌈True⌉
        ;;
        ;; or
        ;;   < exp1, ρ > ⇓ ⌈z⌉
        ;;   =========================
        ;;   < not(exp1), ρ > ⇓ ⌈¬z⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of exp1 env) = (bool-val #t)
        ;;   =============================================
        ;;   (value-of (not-exp exp1) env) = (bool-val #f)
        ;;
        ;;   (value-of exp1 env) = (bool-val #f)
        ;;   =============================================
        ;;   (value-of (not-exp exp1) env) = (bool-val #t)
        ;; or
        ;;   (value-of exp1 env) = (bool-val z)
        ;;   ==================================================
        ;;   (value-of (not-exp exp1) env) = (bool-val (not z))
        ;; or
        ;;   (value-of exp1 env) = val
        ;;   (expval->bool val) = z
        ;;   ==================================================
        ;;   (value-of (not-exp exp1) env) = (bool-val (not z))
        ;; Spec: BoolVal x BoolVal -> BoolVal
        ;; Op Sem Rule [source-notation]:
        ;;   < exp1, ρ > ⇓ ⌈A⌉
        ;;   < exp2, ρ > ⇓ ⌈B⌉
        ;;   =============================
        ;;   < and(exp1,exp2), ρ > ⇓ ⌈A∧B⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of exp1 env) = (bool-val z1)
        ;;   (value-of exp2 env) = (bool-val z2)
        ;;   ===========================================================
        ;;   (value-of (and-exp exp1 exp2) env) = (bool-val (and z1 z2))
        ;; Spec: BoolVal x BoolVal -> BoolVal
        ;; Op Sem Rule [source-notation]:
        ;;   < exp1, ρ > ⇓ ⌈A⌉
        ;;   < exp2, ρ > ⇓ ⌈B⌉
        ;;   =============================
        ;;   < or(exp1,exp2), ρ > ⇓ ⌈A∨B⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of exp1 env) = (bool-val z1)
        ;;   (value-of exp2 env) = (bool-val z2)
        ;;   ===========================================================
        ;;   (value-of (or-exp exp1 exp2) env) = (bool-val (or z1 z2))

        ;; In-Class Exercise 3.C
        ;; Spec: Number x Number -> RatioVal
        ;; Op Sem Rule [source-notation]:
        ;;   < #x:y, ρ > ⇓ ⌈x/y⌉
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of (ratio-exp num1 num2) env) = (ratio-val num1 num2)
        (ratio-exp (exp1 exp2)
                   (let ((val1 (value-of exp1 env))
                         (val2 (value-of exp2 env)))
                     (let ((num1 (expval->num val1))
                           (num2 (expval->num val2)))
                       (ratio-val num1 num2))))
        ;; Spec: RatioVal -> BoolVal
        ;; Op Sem Rule [source-notation]:
        ;;   < exp1, ρ > ⇓ ⌈x/y⌉     x < y
        ;;   =============================
        ;;   < proper?(exp1), ρ > ⇓ ⌈True⌉
        ;;
        ;;   < exp1, ρ > ⇓ ⌈x/y⌉     x ≥ y
        ;;   ==============================
        ;;   < proper?(exp1), ρ > ⇓ ⌈False⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of exp1 env) = (ratio-val num1 num2)
        ;;   =========================================================
        ;;   (value-of (prop-exp exp1) env) = (bool-val (< num1 num2))
        ;; Spec: RatioVal -> BoolVal
        ;; Op Sem Rule [source-notation]:
        ;;   < exp1, ρ > ⇓ ⌈x/y⌉      x < y
        ;;   ================================
        ;;   < improper?(exp1), ρ > ⇓ ⌈False⌉
        ;;
        ;;   < exp1, ρ > ⇓ ⌈x/y⌉      x ≥ y
        ;;   ===============================
        ;;   < improper?(exp1), ρ > ⇓ ⌈True⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of exp1 env) = (ratio-val num1 num2)
        ;;   ============================================================
        ;;   (value-of (improp-exp exp1) env) = (bool-val (>= num1 num2))
        ;; Spec: RatioVal -> BoolVal
        ;; Op Sem Rule [source-notation]:
        ;;   < exp1, ρ > ⇓ ⌈x/y⌉
        ;;   ===============================
        ;;   < reciprocal(exp1), ρ > ⇓ ⌈y/x⌉
        ;;
        ;; Op Sem Rule [host-notation]:
        ;;   (value-of exp1 env) = (ratio-val num1 num2)
        ;;   ========================================================
        ;;   (value-of (reciprocal exp1) env) = (ratio-val num2 num1)

        )))


  )

