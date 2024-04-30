#lang typed/racket
(require typed/rackunit)

(define-syntax tstruct
    (syntax-rules ()
      [(_ name fields)
       (struct name fields #:transparent)]))

(provide tstruct)

; ZODE4
; ExpressionC
(define-type ExprC (U NumC BinOpC ifLeq0? AppC FunDefC IdC))
; Number, operator, AppC, IdC, ifLeq0?
(tstruct NumC ([n : Real]))
(tstruct BinOpC ([op : Sexp][l : ExprC] [r : ExprC])) 
(tstruct AppC ([fun : Symbol](args : ExprC)))
(tstruct IdC ([s : Symbol]))
(tstruct ifLeq0? ([cond : ExprC] [then : ExprC] [else : ExprC]))

; Function Definition
(tstruct FunDefC ([name : Symbol]
                  [arg : Symbol]
                  [body : ExprC]))


; Operator function lookup table 
; Checks if a valid operator, returns True/False
(define (operator? [name : Symbol]) : Boolean
  (match name
    ['/ #t]
    ['+ #t]
    ['- #t]
    ['* #t]
    [other #f]))

; Checks if is an ifLeq0? 
(define (is-an-if? [f : Sexp]) : Boolean
  (equal? f 'ifLeq0?))

; Parser 
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)] ; num
    [(? symbol? s) (if (keyword? s)
                       (error 'parse "ZODE: parse: symbol is a keyword. got ~e" s)
                       (IdC s))] ; id, used for func arguments
    [(list (? symbol? s)) (if (keyword? s)
                              (error 'parse "ZODE: parse: symbol is a keyword. got ~e" s)
                              (IdC s))] ; list with only an id
    [(list (? symbol? op) l r) (if (operator? op)
                                   (BinOpC op (parse l) (parse r))
                                   (error 'parse "ZODE: parse: invalid operation in BinOp, got ~e" op))] ; expr
    [(list (? symbol? f) (list a)) (if (keyword? f)
                                (error 'parse "ZODE: parse: function name is a keyword. got ~e" s)
                                (AppC f (parse a)))]
    [(list (? is-an-if? f) c t e) (ifLeq0? (parse c) (parse t) (parse e))] ;
    [other (error 'parse "ZODE: parse: bad expression. got ~e" other)]))

; Test Cases for Parse
(check-equal? (parse '{+ 2 {* 3 4}})
              (BinOpC '+ (NumC 2) (BinOpC '* (NumC 3) (NumC 4))))


; Check Exceptions
(check-exn (regexp (regexp-quote "ZODE: parse: bad expression. got '()"))
           (lambda () (parse '())))



; Parse FundefC

(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'fun (list (? symbol? name) (list (? symbol? arg))) body)
     (if (or (keyword? name) (keyword? arg))
         (error 'parse-fundef "ZODE: Expected non-keyword for name and arg, got ~a and ~a" name arg)
         (FunDefC name arg (parse body)))]
    [(list 'fun (list (? symbol? name) (list (? symbol? arg))) body extra ...)
     (error 'parse-fundef "ZODE: Too many arguments in function definition, ~s" s)]
    [other
     (error 'parse-fundef "ZODE: Invalid function definition format, got ~a" other)]))

; Parse-prog (list of funcitons in ZODE, call parse-fundefC recursively)

(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]))

; Test cases for parse-prog
(check-equal? (parse-prog '{{fun {f (x)} {+ x 2}}}) (list (FunDefC 'f 'x (BinOpC '+ (IdC 'x) (NumC 2)))))
(check-equal? (parse-prog '{{fun {g (y)} {- y 3}} {fun {h (z)} {* z 4}}})
              (list (FunDefC 'g 'y (BinOpC '- (IdC 'y) (NumC 3)))
                    (FunDefC 'h 'z (BinOpC '* (IdC 'z) (NumC 4)))))


; get fundef
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
    (cond
      [(empty? fds)
       (error 'get-fundef "ZODE: reference to undefined function")]
      [(cons? fds)
       (cond
         [(equal? n (FunDefC-name (first fds))) (first fds)]
         [else (get-fundef n (rest fds))])]))


; subst
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(BinOpC op l r) (BinOpC op
                             (subst what for l)
                             (subst what for r))]
    [(IdC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(AppC f a) (AppC f (subst what for a))]
    [(ifLeq0? c t e) (ifLeq0? (subst what for c)
                              (subst what for t)
                              (subst what for e))]
    [other (error 'parse "ZODE subst: bad expression. got ~e" other)]))


(check-equal? (subst (NumC 2) ;what
                     'x       ;for
                     (IdC 'x));expr
                     (NumC 2));eval

(check-equal? (subst (NumC 2)
                     'x
                     (BinOpC '* (BinOpC '+ (IdC 'x) (NumC 2)) (NumC 2)))
                     (BinOpC '* (BinOpC '+ (NumC 2) (NumC 2)) (NumC 2)))



; Interp
(define (interp [e : ExprC][fds : (Listof FunDefC)]) : Real
  (match e
    [(NumC n) n]
    [(BinOpC op l r)
     (match op
       ['+ (+ (interp l fds) (interp r fds))]
       ['- (- (interp l fds) (interp r fds))]
       ['* (* (interp l fds) (interp r fds))]
       ['/ (if (zero? (interp r fds))
              (error 'interp "ZODE: interp: division by zero")
              (/ (interp l fds) (interp r fds)))]
       [other (error 'interp "ZODE: interp: unsupported operator")])]
    [(AppC f a) (define fd (get-fundef f fds))
                
                (interp (subst (NumC (interp a fds))
                               (FunDefC-arg fd)
                               (FunDefC-body fd))
                        fds)]
    [(IdC _) (error 'interp "ZODE: interp: shouldn't get here")]
    [(ifLeq0? cond then else)
     (if (<= (interp cond fds) 0)
     (interp then fds)
     (interp else fds))]
   [other (error 'interp "ZODE: interp: bad expression. got ~e" other)]))

(check-exn exn:fail? (lambda () (interp (BinOpC '> (NumC 1) (NumC 1)) '())))


; Find target function in list of FunDefC
(define (lookup-function [target : Symbol] [funs : (Listof FunDefC)]) : (U FunDefC False)
  (match funs
    ['() #f]
    [(cons (FunDefC name arg body) rest) 
     (if (symbol=? name target)
         (FunDefC name arg body)
         (lookup-function target rest))]))

; Tester
(define fun-list 
  (list (FunDefC 'f 'x (BinOpC '+ (IdC 'x) (NumC 2)))
        (FunDefC 'g 'y (BinOpC '* (IdC 'y) (NumC 3)))
        (FunDefC 'main 'z (NumC 3))))


(check-equal? (lookup-function 'f fun-list)
              (FunDefC 'f 'x (BinOpC '+ (IdC 'x) (NumC 2))))



; interp-fns
(define (interp-fns [funs : (Listof FunDefC)]) : Real
  (let ([main-function (lookup-function 'main funs)])
    (if main-function
        (interp (subst (NumC 0)
                       (FunDefC-arg main-function)
                       (FunDefC-body main-function))
                funs)
        (error 'interp-fns "ZODE interp-fns: No main function found"))))

; Test
(check-equal? (interp-fns (list (FunDefC 'f 'x (BinOpC '+ (IdC 'x) (NumC 2)))
                                (FunDefC 'main 'init (AppC 'f (NumC 2))))) 4)


; top interp
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

; More testing

(define double-fun (FunDefC 'double-fun 'x (BinOpC '* (IdC 'x) (NumC 2))))

(check-equal? (interp (AppC 'double-fun (BinOpC '* (NumC 2) (NumC 2))) (list double-fun))
              8)

(check-equal? (interp (parse '{* {+ 2 4} 5}) '())
              30) 
