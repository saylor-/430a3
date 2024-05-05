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


; -- ZODE3 Language --

(tstruct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]))

(define-type ExprC (U NumC BinOpC ifleq0? AppC IdC Real))
(tstruct NumC ([n : Real]))
(tstruct AppC  ([fun : Symbol] [args : (Listof ExprC)]))
(tstruct BinOpC ([op : Symbol] [l : ExprC] [r : ExprC]))
(tstruct ifleq0? ([guard : ExprC] [then : ExprC] [else : ExprC]))
(tstruct IdC ([id : Symbol]))

; --- defined variables ---:

; fundefList is a variable with a list of fundef's so we can
; pass in a list of fundef's into our functions for test cases easily
(define fundefList (list (FunDefC 'foo '(a b c) (NumC 4))
                     (FunDefC 'throw_ball '(d e f) (BinOpC '+ (NumC 5) (NumC 8)))
                     (FunDefC 'main '() (NumC 4))))


(define fundefList_hard (list (FunDefC 'foo '(a b) (BinOpC '+ (IdC 'a) (IdC 'b)))
                     (FunDefC 'throw_ball '(d e f) (BinOpC '+ (NumC 5) (NumC 8)))
                     (FunDefC 'main '() (AppC 'foo (list (NumC 5) (NumC 4))))))



; valid-binop takes in an operator and returns true if the operator
; is valid in the ZODE3 language and false otherwise
(define (valid-binop? [op : Symbol]) : Boolean
  (match op
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    [else #f]))

; test-cases for valid-binop:
(check-equal? (valid-binop? '+) #t)
(check-equal? (valid-binop? '-) #t)
(check-equal? (valid-binop? '*) #t)
(check-equal? (valid-binop? '/) #t)
(check-equal? (valid-binop? 'a) #f)
(check-equal? (valid-binop? '//) #f)


; -- Parser for ZODE3 --

; parse takes in an s-expression and returns an ExprC if the
; given s-expression can be transformed into one. Error otherwise.
(define (parse [sexp : Sexp]) : ExprC
  (match sexp
    [(? real? r) (NumC r)]
    [(? symbol? s) (IdC s)]
    [(list (and (? symbol? op) (? valid-binop? op)) a b) (BinOpC op (parse a) (parse b))]
    [(list 'ifleq0? guard then else) (ifleq0? (parse guard) (parse then) (parse else))]
    [(list (? symbol? fname) args ...)(AppC fname (map parse args))]
    [else (error 'parse "ZODE : expected s-expression, got ~e" sexp)]))

; test-cases for parse:
(check-equal? (parse 5) (NumC 5))
(check-equal? (parse '(+ 6 5)) (BinOpC '+ (NumC 6) (NumC 5)))
(check-equal? (parse '(ifleq0? 4 5 6)) (ifleq0? (NumC 4) (NumC 5) (NumC 6)))
(check-equal? (parse '(foo 5 6 7)) (AppC 'foo (list (NumC 5) (NumC 6) (NumC 7))))

(check-exn (regexp (regexp-quote "ZODE : expected s-expression"))
           (lambda () (parse '(4 15 4))))

; parse-fundef takes in an s-expression and returns a FunDefC
(define (parse-fundef [sexp : Sexp]) : FunDefC
  (match sexp
    ['() (error 'parse-fundef "ZODE : Unable to parse and empty list (parse-fundef)")]
    [(list 'def ': (? symbol? fname) ': (? symbol? args) ... ': body)
     (if (and (andmap symbol? args) (not (check-duplicates args)) (symbol? fname) (not (member fname '(+ - * /)))) ; guard
         (FunDefC fname args (parse body))
         (error 'parse-fundef "ZODE : Invalid parameters in function definition"))]
    [else (error 'parse-fundef "ZODE : You gave me a busted fundef ~e" sexp)]))

; test-cases for parse-fundef:
(check-exn #px"ZODE : Unable" (λ () (parse-fundef '())))
(check-exn #px"ZODE : Invalid" (λ () (parse-fundef '{def : f : x y y : {+ x y}})))
(check-exn #px"ZODE : You" (λ () (parse-fundef '{f : x y y : {+ x y}})))
(check-equal? (parse-fundef '{def : f : x y : {+ x y}}) (FunDefC 'f '(x y) (BinOpC '+ (IdC 'x) (IdC 'y))))
(check-equal? (parse-fundef '{def : main : : {f 1 2}}) (FunDefC 'main '() (AppC 'f (list (NumC 1) (NumC 2)))))


; Parse a list of fundefs
(define (parse-prog [sexp : Sexp]) : (Listof FunDefC)
  (match sexp
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]))

; test-cases for parsing a list of FunDefC:
(check-equal? (parse-prog '()) '())
(check-equal? (parse-prog '{{def : f : x y : {+ x y}} {def : main : : {f 1 2}}}) (list (FunDefC 'f '(x y) (BinOpC '+ (IdC 'x) (IdC 'y))) (FunDefC 'main '() (AppC 'f (list (NumC 1) (NumC 2))))))


; subst takes in an ExprC (argument), a Symbol (parameter), and another ExprC
; (argument that will replace parameter in the function body) and returns the updated function body.
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(BinOpC op a b)(BinOpC op (subst what for a) (subst what for b))]
    [(ifleq0? g t e)(ifleq0? (subst what for g) (subst what for t) (subst what for e))]
    [(AppC f args) (AppC f (map (lambda ([a : ExprC]) (subst what for a)) args))]))
    ;[else (error 'parse "ZODE : unable to substitute into function body.")]))

; TODO : subst should probably throw an error if it is unable to substitute. Not really sure of how that could happen assuming a correct parse.

; test-cases for subst:
(check-equal? (subst (NumC 4) 'a (IdC 'a)) (NumC 4))
(check-equal? (subst (NumC 5) 'a (NumC 6)) (NumC 6))
(check-equal? (subst (NumC 5) 'a (BinOpC '+ (IdC 'a) (NumC 7))) (BinOpC '+ (NumC 5) (NumC 7)))
(check-equal? (subst (NumC 5) 'x (ifleq0? (IdC 'x) (NumC 6) (NumC 5))) (ifleq0? (NumC 5) (NumC 6) (NumC 5)))
(check-equal? (subst (NumC 10) 'y (AppC 'bar (list (IdC 'y)))) (AppC 'bar (list (NumC 10))))
(check-equal? (subst (NumC 10) 'y (AppC 'bar (list (IdC 'y) (IdC 'var)))) (AppC 'bar (list (NumC 10) (IdC 'var))))
;(check-exn #px"ZODE" (λ () (subst  '())))


; multi-subst takes in a list of ExprC, a List of Symbols, and an ExprC (body) and returns an ExprC (body) with the argument substituted 
(define (multi-subst [lst : (Listof ExprC)] [for : (Listof Symbol)] [in : ExprC]): ExprC
    (cond
    [(and (empty? lst) (empty? for)) in] ; when both lists are empty, return the body with all of the substitutions done
    [(or (empty? lst) (empty? for)) (error 'multi-subst "ZODE : Lists of expressions and symbols are not of equal length")] ;; If one list is empty and the other is not, throw an error
    [else ; otherwise, we need to apply substitution and recurse on the rest of the two lists.
     (let ([body (subst (first lst) (first for) in)])
       (multi-subst (rest lst) (rest for) body))]))


; test-cases for multi-subst:
(check-equal? (multi-subst (list (NumC 10) (NumC 11) (NumC 12)) (list 'x 'y 'z) (AppC 'bar (list (IdC 'x) (IdC 'y) (IdC 'z)))) (AppC 'bar (list (NumC 10) (NumC 11) (NumC 12))))


; get-fundef takes in a function name and a
; list of FunDefC and returns the FunDefC that matches n
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds)
     (error 'get-fundef "ZODE : reference to undefined function")]
    [(cons? fds)
     (cond
       [(equal? n (FunDefC-name (first fds))) (first fds)]
       [else (get-fundef n (rest fds))])]))

; test-cases for get-fundef:
(check-exn #px"ZODE" (λ () (get-fundef 'a '())))
(check-equal? (get-fundef 'foo fundefList) (FunDefC 'foo '(a b c) (NumC 4)))
(check-equal? (get-fundef 'throw_ball fundefList) (FunDefC 'throw_ball '(d e f) (BinOpC '+ (NumC 5) (NumC 8))))


; interp takes in an ExprC, a list of FunDefC and returns a Real value.
(define (interp [e : ExprC] [fds : (Listof FunDefC)]) : Real
  (match e
    [(NumC n) n]
    [(BinOpC op a b)
       (match op
       ['+ (+ (interp a fds) (interp b fds))]
       ['- (- (interp a fds) (interp b fds))]
       ['* (* (interp a fds) (interp b fds))]
       ['/ (if (zero? (interp b fds)) ; guard
              (error 'interp "ZODE : unable to perform division by zero") ;then
              (/ (interp a fds) (interp b fds)))] ;else
       [other (error 'interp "ZODE : unknown operator")])]
    [(ifleq0? guard then else)
        (if (<= (interp guard fds) 0)
        (interp then fds)
        (interp else fds))]
    [(AppC f a) (define interpArgs (map (lambda([r : Real]) (NumC r))(map (lambda([arg : ExprC]) (interp arg fds)) a))) ; 1) Evaluate the arguments (eager)
                (define fd (get-fundef f fds)) ; 2) Look up the function body
                (define substFunBody (multi-subst interpArgs (FunDefC-args fd) (FunDefC-body fd))) ; 3) substitute the arguments for the parameters
                (interp substFunBody fds)] ; 4) execute the function body
    [(IdC sym) (error 'interp "ZODE : Got unbound id ~e" sym)]
    ))

; NOTE: AppC looks horrible, pretty much what is happening is args is a list of ExprC that needs to be evaluated, so I evaluate each argument, and then turn the
; resulting list of reals into a list of NumC so that subst works correctly. The map on the right is evaluated first to a list of reals.

; test-cases for interp:
(check-equal? (interp (NumC 4) fundefList) 4)
(check-equal? (interp (BinOpC '+ (NumC 6) (NumC 4)) fundefList) 10)
(check-equal? (interp (BinOpC '- (NumC 6) (NumC 4)) fundefList) 2)
(check-equal? (interp (BinOpC '* (NumC 6) (NumC 4)) fundefList) 24)
(check-equal? (interp (BinOpC '/ (NumC 6) (NumC 3)) fundefList) 2)
(check-equal? (interp (BinOpC '/ (NumC 0) (NumC 3)) fundefList) 0)
(check-equal? (interp (ifleq0? (NumC 4) (NumC 1) (NumC 0)) fundefList) 0)
(check-equal? (interp (ifleq0? (NumC 0) (NumC 1) (NumC 0)) fundefList) 1)
(check-equal? (interp (AppC 'foo (list (NumC 3) (NumC 4))) fundefList_hard) 7)
(check-exn (regexp (regexp-quote "ZODE : unable"))
           (lambda () (interp (BinOpC '/ (NumC 6) (NumC 0)) fundefList)))
(check-exn (regexp (regexp-quote "ZODE : unknown operator"))
           (lambda () (interp (BinOpC '// (NumC 5) (NumC 5)) fundefList)))
(check-exn (regexp (regexp-quote "ZODE : Got"))
           (lambda () (interp (IdC 'x) fundefList)))


; interp-fns takes in a list of FunDefC and returns a Real
(define (interp-fns [func : (Listof FunDefC)]) : Real
  (match (get-fundef 'main func)
    [(FunDefC 'main '() body) (interp body func)]))


; test-cases for interp-fns:
(check-equal? (interp-fns fundefList) 4)
(check-equal? (interp-fns fundefList_hard) 9)

; professor given test cases:
(check-equal? (interp-fns (parse-prog '{{def : f : x y : {+ x y}} {def : main : : {f 1 2}}})) 3)
(check-equal? (interp-fns (parse-prog '{{def : f : : 5} {def : main : : {+ {f} {f}}}})) 10)
(check-exn #px"ZODE" (λ () (interp-fns (parse-prog '{{def : f : x y : {+ x y}} {def : main : : {f 1}}}))))


; --------- test-cases for all functions ----------- (copy and paste test cases under here at the end of all of implementation)