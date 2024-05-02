; Jonathan Zoccola - Assignment 3 PL
#lang typed/racket
(require typed/rackunit)

; defined syntax for tstruct, eliminates the need
; to put #:transparent at the end of every struct.
(define-syntax tstruct
    (syntax-rules ()
      [(_ name fields)
       (struct name fields #:transparent)]))

(provide tstruct)

; -- ZODE3 Language --

(tstruct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]))

(define-type ExprC (U NumC BinOpC ifleq0?))
(tstruct NumC ([n : Real]))
(tstruct BinOpC ([op : Symbol] [l : ExprC] [r : ExprC]))
(tstruct ifleq0? ([guard : ExprC] [then : ExprC] [else : ExprC]))


; Syntax for the ZODE3 Language in EBNF Notation:

;;   EXPR	 =	        num
;;  	 	|	 	{+ EXPR EXPR}
;;  	 	|	 	{- EXPR EXPR}
;;  	 	|	 	{* EXPR EXPR}
;;  	 	|	 	{/ EXPR EXPR}
;;  	 	|	 	{id EXPR ...}
;;  	 	|	 	{ifleq0? : EXPR : EXPR : EXPR}
;;  	 	|	 	id
;;   DEFN	 =	        {def : id : id ... : EXPR}


; valid-binop takes in an operator and returns true if the operator
; is valid in the ZODE3 language and false otherwise
(define (valid-binop? [op : Symbol]) : Boolean
  (match op
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    [else #f]))


; TODO: ----- Need to add prevention for division by zero -----


; -- Parser for ZODE3 --

; parse takes in an s-expression and returns an ExprC if the
; given s-expression can be transformed into one. Error otherwise.
(define (parse [sexp : Sexp]) : ExprC
  (match sexp
    [(? real? r) (NumC r)]
    [(list op a b)
     (if (and (symbol? op) (valid-binop? op)) ; guard, (we also check in interp for division by zero and an invalid operator)
        (BinOpC op (parse a) (parse b)) ; then
        (error 'parse "ZODE : invalid binary operator, got ~e" op))] ; else
    [(list 'ifleq0? guard then else) (ifleq0? (parse guard) (parse then) (parse else))]
    [else (error 'parse "ZODE : expected s-expression, got ~e" sexp)]))


; interp takes in an ExprC and returns a Real
(define (interp [e : ExprC]) : Real
  (match e
    [(NumC n) n]
    [(BinOpC op a b)
       (match op
       ['+ (+ (interp a) (interp b))]
       ['- (- (interp a) (interp b))]
       ['* (* (interp a) (interp b))]
       ['/ (if (zero? (interp b)) ; guard
              (error 'interp "ZODE: interp: unable to perform division by zero") ;then
              (/ (interp a) (interp b)))] ;else
       [other (error 'interp "ZODE: interp: unsupported operator")])]
    [(ifleq0? guard then else)
        (if (<= (interp guard) 0)
        (interp then)
        (interp else))]))

; top-interp takes in an s-expression and calls the parser and interp
(define (top-interp [s : Sexp]) : Real
  (interp (parse s)))

; --------- test-cases for all functions -----------

; test-cases for valid-binop:
(check-equal? (valid-binop? '+) #t)
(check-equal? (valid-binop? '-) #t)
(check-equal? (valid-binop? '*) #t)
(check-equal? (valid-binop? '/) #t)
(check-equal? (valid-binop? 'a) #f)
(check-equal? (valid-binop? '//) #f)


; test-cases for parse:
(check-equal? (parse 5) (NumC 5))
(check-equal? (parse '(+ 6 5)) (BinOpC '+ (NumC 6) (NumC 5)))
(check-equal? (parse '(ifleq0? 4 5 6)) (ifleq0? (NumC 4) (NumC 5) (NumC 6)))
(check-exn (regexp (regexp-quote "ZODE : expected s-expression"))
           (lambda () (parse 'a)))
(check-exn (regexp (regexp-quote "ZODE : invalid binary"))
           (lambda () (parse '(4 5 6))))

; test-cases for interp:


; test-cases for top-interp:
(check-equal? (top-interp 4) 4)
