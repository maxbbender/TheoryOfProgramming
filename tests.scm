(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ;; Exercise 3.6
      ;; check arithmetic negation
      (simple-minus "minus(5)" -5)
      (nested-minus "minus(-(minus(5),9))" 14)

      ;; Exercise 3.7
      ;; check other arithmetic
      (simple-plus "+(1,2)" 3)
      (simple-mult "*(2,3)" 6)
      (simple-quot "/(5,3)" 1)
      (compound-arithmetic "/(*(10,-(6,2)),+(5,3))" 5)

      ;; Exercise 3.8
      ;; check comparisons
      (simple-eq-1 "equal?(2,2)" #t)
      (simple-eq-2 "equal?(1,2)" #f)
      (simple-lt-1 "less?(3,5)" #t)
      (simple-lt-2 "less?(5,3)" #f)
      (simple-lt-1 "greater?(5,3)" #t)
      (simple-lt-1 "greater?(3,5)" #f)

      ;; Exercise 3.9
      ;; check list operations
      (check-cons "cons(5, emptylist)" (5))
      (check-car "car(cons(5,emptylist))" 5)
      (check-cdr "cdr(cons(5,emptylist))" ())
      (check-null?-1 "null?(emptylist)" #t)
      (check-null?-2 "null?(cons(5,emptylist))" #f)
      (check-empty "emptylist" ())
      (check-all-list-ops "let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist))" (4 (3)))

      ;; Exercise 3.10
      ;; check list operations
      (simple-list "list(1,2,3,4,5)" (1 2 3 4 5))

      ;; Exercise 3.12
      ;; check list operations
      (single-cond "cond zero?(0) ==> 1 end" 1)
      (double-cond "cond zero?(1) ==> 1 zero?(0) ==> 2 end" 2)
      (triple-cond "cond zero?(2) ==> 1 zero?(1) ==> 2 zero?(0) ==> 3 end" 3)
      (failed-cond "cond zero?(2) ==> 1 end" error)

      ;; Exercise 3.15
      ;; check print operation
      (simple-print "print(5)" 1)

      ;; Exercise 3.16
      ;; check new let with arbno bindings
      (multi-let-1 "let x = 2 y = 3 in +(x,y)" 5)
      (multi-let-2 "let x = 30 in let x = -(x,1) y = -(x,2) in -(x,y)" 1)

      ;; Exercise 3.16
      ;; check new let* with arbno bindings
      (multi-let*-1 "let* x = 2 y = +(x,1) in +(x,y)" 5)
      (multi-let*-2 "let x = 30 in let* x = -(x,1) y = -(x,1) in -(x,y)" 1)

      ;; Exercise 3.18
      ;; check list operations
      (simple-unpack "unpack x y z = list(2,5,7) in -(x,-(y,z))" 4)

      ;; In-Class Exercise 3.A
      ;; check boolean literals
      (literal-true "true" #t)
      (literal-false "false" #f)

      ;; In-Class Exercise 3.B
      ;; check logical operations
      (simple-not "not(true)" #f)
      (simple-and "and(true,false)" #f)
      (simple-or "or(true,false)" #t)

      ;; In-Class Exercise 3.C
      ;; check ratio values
      (simple-ratio "#5:4" 5/4)
      (ratio-in-let "let x = 5 y = 4 in #x:y" 5/4)
      (proper-ratio "proper?(#5:4)" #f)
      (improper-ratio "improper?(#5:4)" #t)
      (reciprocal-ratio "reciprocal(#5:4)" 4/5)
      (proper-reciprocal "proper?(reciprocal(#5:4))" #t)

      ))
  )
