(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for let-lang.

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    ;; Exercise 3.9
    ;; list-val: ListOf(ExpVal) -> ExpVal
    (list-val
      (lst (list-of expval?)))
    ;; In-Class Exercise 3.C
    ;; ratio-val: Number x Number -> ExpVal
    (ratio-val
      (numer number?)
      (denom number?))
    )

  ;; Exercise 3.9 - Auxiliary procedure from Chapter 2, p.48
  (define list-of
    (lambda (pred)
      (lambda (val)
        (or (null? val)
            (and (pair? val)
                 (pred (car val))
                 ((list-of pred) (cdr val)))))))

;;; extractors:

  ;; expval->num : ExpVal -> Int
  ;; Page: 70
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  ;; Page: 70
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; Exercise 3.9
  ;; expval->list : ExpVal -> List
  (define expval->list
    (lambda (v)
      (cases expval v
    (list-val (lst) lst)
    (else (expval-extractor-error 'list v)))))

  ;; In-Class Exercise 3.C
  ;; expval->rational : ExpVal -> Rational
  (define expval->rational
    (lambda (v)
      (cases expval v
	(ratio-val (numer denom) (/ numer denom))
	(else (expval-extractor-error 'bool v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;; example of a data type built without define-datatype

  (define empty-env-record
    (lambda () 
      '()))

  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))
  
  (define empty-env-record? null?)
  
  (define environment?
    (lambda (x)
      (or (empty-env-record? x)
          (and (pair? x)
               (symbol? (car (car x)))
               (expval? (cadr (car x)))
               (environment? (cdr x))))))

  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))

  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))

  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))

)
