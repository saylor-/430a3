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
(tstruct BinopC ([op : Sexp][l : ExprC] [r : ExprC]))
(tstruct IfLeqO ([test : ExprC] [then : ExprC] [else : ExprC]))

; Operation function lookup table
(define op-table
  (hash '+ +
        '- -
        '* *
        '/ /))

; Parser, read an sexP 
(define (parse [sexp : Sexp]) : ExprC
  (match sexp
    [(? real? n) (NumC n)]
    [(list op a b) (BinopC op (parse a) (parse b))]
    [(list 'ifleq0? test then else) (IfLeqO (parse test) (parse then) (parse else))]
    [_ (error "ZODE: Invalid syntax")]))

; Interpreter Method
; Use op-func to match the mult symbol
(define (interp [a : ExprC]) : Real
    (match a
      [(NumC n) n]
      [(BinopC op left right)
       (let ([op-func (hash-ref op-table op (lambda () (error "ZODE: Unknown operator")))])
(op-func (interp left) (interp right)))]
      [(IfLeqO test then else)
       (if (<= (interp test) 0)
           (interp then)
           (interp else))]))


; Top-interp
(define (top-interp [s : Sexp]) : Real
  (interp (parse s)))

; Test Cases
(check-equal? (parse 5) (NumC 5))
(check-equal? (parse '(+ 2 3)) (BinopC '+ (NumC 2) (NumC 3)))
(check-equal? (parse '(* 4 5)) (BinopC '* (NumC 4) (NumC 5)))
(check-equal? (parse '(- 6 3)) (BinopC '- (NumC 6) (NumC 3)))
(check-equal? (parse '(/ 9 3)) (BinopC '/ (NumC 9) (NumC 3)))

(check-equal? (interp (parse '(ifleq0? 1 1 (- 1 1)))) 0 "Positive x decremented")

(check-equal? (interp (BinopC '+ (NumC 2) (NumC 3))) 5)
(check-equal? (interp (BinopC '- (NumC 10) (NumC 3))) 7)
(check-equal? (interp (BinopC '* (NumC 2) (NumC 3))) 6)
(check-equal? (interp (BinopC '/ (NumC 6) (NumC 3))) 2)

(check-equal? (top-interp 8) 8)
(check-equal? (top-interp '(+ 4 5)) 9)
