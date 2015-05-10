(module lang
  
  ;; grammar for the LET language
  
  (lib "eopl.ss" "eopl")                
  
  (require "drscheme-init.scm")
  
  (provide (all-defined-out))
  
  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)
      
      (expression (number) const-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)
      
      (expression (identifier) var-exp)
      
      ;(expression
      ; ("let" identifier "=" expression "in" expression)
      ; let-exp)   
            
      ;; Exercise 3.9
      ;; BNF: <Expression> ::= emptylist
      ;; AST:                  [ empty-exp () ]
      (expression ("emptylist") empty-exp)

      ;; Exercise 3.11
      ;; BNF: <UnaryOp> ::= <UnaryArithOp> | <UnaryListOp> | "print"
      ;;                  | <UnaryLogOp>
      (unary-op (unary-arith-op) arith-1-op)
      ;; Exercise 3.9
      (unary-op (unary-list-op) list-1-op)
      ;; Exercise 3.15
      (unary-op ("print") print-op)
      (unary-op (unary-logic-op) log-1-op)
      (unary-op (unary-ratio-op) rat-1-op)
      ;; BNF: <UnaryArithOp> ::= "zero?" | "minus"
      (unary-arith-op ("zero?") zero-op)
      ;; Exercise 3.6
      (unary-arith-op ("minus") neg-op)
      ;; Exercise 3.9
      ;; BNF: <UnaryListOp> ::= "car" | "cdr" | "null?"
      (unary-list-op ("car") car-op)
      (unary-list-op ("cdr") cdr-op)
      (unary-list-op ("null?") null-op)
      ;; In-Class Exercise 3.B
      ;; BNF: <UnaryLogOp> ::= "not"
      (unary-logic-op ("not") not-op)
      ;; In-Class Exercise 3.C
      ;; BNF: <UnaryRatioOp> ::= "proper?" | "improper?" | "reciprocal"
      (unary-ratio-op ("proper?") prop-op)
      (unary-ratio-op ("improper?") improp-op)
      (unary-ratio-op ("reciprocal") recip-op)
      ;; BNF: <BinaryOp> ::= <BinArithOp> | <BinRelnOp> | <BinLogOp> | "cons"
      (binary-op (bin-arith-op) arith-2-op)
      (binary-op (bin-reln-op) reln-op)
      ;; Exercise 3.9
      (binary-op ("cons") pair-op)
      (binary-op (bin-logic-op) log-2-op)
      ;; Exercise 3.7
      ;; BNF: <BinArithOp> ::= "-" | "+" | "*" | "/"
      (bin-arith-op ("-") diff-op)
      (bin-arith-op ("+") plus-op)
      (bin-arith-op ("*") mult-op)
      (bin-arith-op ("/") quot-op)
      ;; Exercise 3.8
      ;; BNF: <BinRelnOp> ::= "equal?" | "less?" | "greater?"
      (bin-reln-op ("equal?") eq-op)
      (bin-reln-op ("less?") lt-op)
      (bin-reln-op ("greater?") gt-op)
      ;; In-Class Exercise 3.B
      ;; BNF: <BinLogOp> ::= "and" | "or"
      (bin-logic-op ("and") and-op)
      (bin-logic-op ("or") or-op)
      ;; Exercise 3.10
      ;; BNF: <NaryOp> ::= "list"
      (nary-op ("list") list-op)
      ;; BNF: <Expression> ::= <UnaryOp> ( <Expression> )
      ;; AST:                 [ unop-exp ( rator rand ) ]
      (expression
       (unary-op "(" expression ")")
       unop-exp)
      ;; BNF: <Expression> ::= <BinaryOp> ( <Expression>, <Expression> )
      ;; AST:                 [ binop-exp ( rator rand1 rand2 ) ]
      (expression
       (binary-op "(" expression "," expression ")")
       binop-exp)
      ;; BNF: <Expression> ::= <NaryOp> ( {<Expression>}*(,) )
      ;; AST:                 [ nop-exp ( rator rands ) ]
      (expression
       (nary-op "(" (separated-list expression ",") ")")
       nop-exp)

      ;; Exercise 3.12
      ;; BNF: <Expression> ::= cond { <Expression> ==> <Expression> }* end )
      ;; AST:                  [ cond-exp (tests exps) ]
      (expression
       ("cond" (arbno expression "==>" expression) "end")
       cond-exp)

      ;; Exercise 3.16**
      ;; BNF: <Expression> ::= let { <Identifier> = <Expression>}* in <Expression>
      ;; AST:                  [ let-exp (ids exps body) ]
      (expression
       ("let" (arbno identifier "=" expression ) "in" expression)
       let-exp)

      ;; Exercise 3.17
      ;; BNF: <Expression> ::= let* { <Identifier> = <Expression> }* in <Expression>
      ;; AST:                  [ let*-exp (ids exps body) ]
      (expression
       ("let*" (arbno identifier "=" expression ) "in" expression)
       let*-exp)

      ;; Exercise 3.18
      ;; BNF: <Expression> ::= unpack {<Identifier>}* = <Expression> in <Expression>
      ;; AST:                  [ unpack-exp (ids rhs body) ]
      (expression
       ("unpack" (arbno identifier) "=" expression "in" expression)
       unpack-exp)

      ;; In-Class Exercise 3.A
      ;; BNF: <Expression> ::= true
      ;; AST:                  [true-exp]
      (expression ("true") true-exp)
      ;; BNF: <Expression> ::= false
      ;; AST:                  [false-exp]
      (expression ("false") false-exp)

      ;; In-Class Exercise 3.C
      ;; BNF: <Expression> ::= #<Expression>:<Expression>
      ;; AST:                  [ratio-exp (exp1 exp2)]
      (expression ("#" expression ":" expression) ratio-exp)

      ))
  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
