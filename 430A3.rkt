#lang typed/racket
(require typed/rackunit)

(define-syntax tstruct
    (syntax-rules ()
      [(_ name fields)
       (struct name fields #:transparent)]))

(provide tstruct)

; Through 3.4 done


#|

Syntax

EXPR	 	=	 	num
 	 	|	 	{+ EXPR EXPR}
 	 	|	 	{- EXPR EXPR}
 	 	|	 	{* EXPR EXPR}
 	 	|	 	{/ EXPR EXPR}
 	 	|	 	{id EXPR ...}
 	 	|	 	{ifleq0? : EXPR : EXPR : EXPR}
 	 	|	 	id
DEFN	 	=	 	{def : id : id ... : EXPR}


|#


; ZODE4
(define-type ExprC (U NumC BinopC IfLeqO))
(tstruct NumC ([n : Real]))
(tstruct BinopC ([op : Sexp][l : ExprC] [r : ExprC])) ; single syntatic rule for all binary and arithmetic operators
;(tstruct AppC ([fun : Symbol][arg : ExprC])) ; pretty sure this is needed in order for the interpreter to accept 0, 1, or more arguments
(tstruct IfLeqO ([test : ExprC] [then : ExprC] [else : ExprC]))

(tstruct FunDefC ([name : Symbol]
                  [param : Symbol]
                  [body : ExprC]))


; my-divide is a helper function for the op table to throw a
; divide by zero error in the case of a divide by zero attempt
(define (my-divide [dividend : Real] [divisor : Real]): Real
  (match divisor
    [0 (error "ZODE: Unable to perform division by 0")]
    [_ (/ dividend divisor)]))


; -- Operator function lookup table --
(define op-table
  (hash '+ +
        '- -
        '* *
        '/ my-divide))


; -- Parser --

; parse takes in an s-expression and returns abstract syntax ExprC 
(define (parse [sexp : Sexp]) : ExprC
  (match sexp
    [(? real? n) (NumC n)]
    [(list op a b) (BinopC op (parse a) (parse b))]
    [(list 'ifleq0? test then else) (IfLeqO (parse test) (parse then) (parse else))]
    [_ (error "ZODE: Invalid syntax")]))


; -- Parse a fundef s-expression --
; parse-fundef takes in an s-expression and returns a FunDefC
(define (parse-fundef [sexp : Sexp]) : FunDefC
  (match sexp
    [(list 'funct (list (? symbol? fun) (? symbol? param))
                   body)
                   (FunDefC fun param (parse body))]
    [other (error 'parse-fundef "ZODE: Invalid s-expression to parse to FunDefC ~e" other)]))


; -- Interpreter -- 
; (Use op-func to match the mult symbol)
; interp takes in abstract syntax (ExprC) and returns the value that the abstract syntax evaluates to
(define (interp [a : ExprC]) : Real
    (match a
      [(NumC n) n]
      [(BinopC op left right) (let ([op-func (hash-ref op-table op (lambda () (error "ZODE: Unknown operator")))]) (op-func (interp left) (interp right)))]
      [(IfLeqO test then else)
       (if (<= (interp test) 0)
           (interp then)
           (interp else))]))


; -- Top-interp --
; top-interp takes in an s-expression and returns a value that the s-expression evaluates to.
(define (top-interp [s : Sexp]) : Real
  (interp (parse s)))

; Test Cases

; test-cases for parse:
(check-equal? (parse 5) (NumC 5))
(check-equal? (parse '(+ 2 3)) (BinopC '+ (NumC 2) (NumC 3)))
(check-equal? (parse '(* 4 5)) (BinopC '* (NumC 4) (NumC 5)))
(check-equal? (parse '(- 6 3)) (BinopC '- (NumC 6) (NumC 3)))
(check-equal? (parse '(/ 9 3)) (BinopC '/ (NumC 9) (NumC 3)))
(check-exn #px"ZODE" (λ () (parse '(/ 9 0)))) ; check for error when attempting to do a division by 0

(check-equal? (interp (parse '(ifleq0? 1 1 (- 1 1)))) 0 "Positive x decremented")

; test-cases for interp:
(check-equal? (interp (BinopC '+ (NumC 2) (NumC 3))) 5)
(check-equal? (interp (BinopC '- (NumC 10) (NumC 3))) 7)
(check-equal? (interp (BinopC '* (NumC 2) (NumC 3))) 6)
(check-equal? (interp (BinopC '/ (NumC 6) (NumC 3))) 2)

; test-cases for top-interp:
(check-equal? (top-interp 8) 8)
(check-equal? (top-interp '(+ 4 5)) 9)
