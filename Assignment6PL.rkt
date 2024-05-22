; Assignment 6 - Programming Languages
#lang typed/racket
(require typed/rackunit)

; Fully implemented program, passing all test cases.

; defined syntax for tstruct, eliminates the need
; to put #:transparent at the end of every struct.
(define-syntax tstruct
  (syntax-rules ()
    [(_ name fields)
     (struct name fields #:transparent)]))

(provide tstruct)


; Syntax for the ZODE 6 Language in EBNF Notation:
; <expr>                ::= <num>
;                        |  <id>
;                        |  { <id> := <expr> }
;                        |  <string>
;                        |  { if : <expr> : <expr> : <expr> }
;                        |  { locals : <clauses> : <expr> }
;                        |  { lamb : <id>* : <expr> }
;                        |  { <expr> <expr>* }
; <clauses>             ::= <id> = <expr>
;                        |  <id> = <expr> : <clauses>
; <top-level-constants> ::= true
;                        |  false
;                        |  null
; <top-level-functions> ::= +
;                        |  -
;                        |  *
;                        |  /
;                        |  equal?
;                        |  <=
;                        |  array
;                        |  make-array
;                        |  aref
;                        |  aset!
;                        |  seq
;                        |  substring
;                        |  error

; ... where an id is not if, lamb, locals, :, :=, or =.



; -- ZODE4 Language --
(define-type ExprC (U NumC IdC StringC AppC lamC IfC))
(tstruct NumC ([n : Real]))
(tstruct IdC ([id : Symbol]))
(tstruct StringC ([s : String]))
(tstruct lamC ([args : (Listof Symbol)] [body : ExprC]))
(tstruct AppC  ([fun : ExprC] [args : (Listof ExprC)]))
(tstruct IfC ([guard : ExprC] [then : ExprC] [else : ExprC]))

(tstruct FunDefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]))

; -- Values --
(define-type Value (U NumV BoolV StringV ClosV PrimOpV))
(tstruct NumV ([n : Real]))
(tstruct BoolV ([b : Boolean]))
(tstruct StringV ([s : String]))
(tstruct ClosV ([args : (Listof Symbol)] [body : ExprC] [env : Env]))
(tstruct PrimOpV ([op : ((Listof Value) -> Value)]))


; -- defined env --
(tstruct Binding ([name : Symbol] [val : Value]))
(define-type Env (Listof Binding))
(define mt-env '())


; -- Serialize Function --

; serialize takes in a ZODE4 Value and returns the serialized version of the Value (String)
; example -> (NumV 34) : "34"
(define (serialize [val : Value]) : String
  (match val
    [(StringV s) (format "~v" s)]
    [(NumV n) (format "~v" n)]
    [(BoolV b) (if b "true" "false")]
    [(ClosV a b e) "#<procedure>"]
    [(PrimOpV f) "#<primop>"]))


; -- PrimOpV Functions for top-level env (Listof Value) -> (Value) --

; add_func takes in a (Listof Value) and returns a NumV result of the two values added together.
(define (add_func [lst : (Listof Value)]) : NumV
  (match lst
    ['() (error 'add_func "ZODE : empty add list, no arguments to add.")]
    [(list (NumV n1) (NumV n2)) (NumV (+ n1 n2))]
    [else (error 'add_func "ZODE : unable to add arguments ~e" lst)]))


; sub_func takes in a (Listof Value) and returns a NumV result of the two values substracted.
(define (sub_func [lst : (Listof Value)]) : NumV
  (match lst
    ['() (error 'sub_func "ZODE : empty sub list, no arguments to subtract.")]
    [(list (NumV n1) (NumV n2)) (NumV (- n1 n2))]
    [else (error 'sub_func "ZODE : unable to subtract arguments ~e" lst)]))



; mult_func takes in a (Listof Value) and returns a NumV result of the two values multiplied together.
(define (mult_func [lst : (Listof Value)]) : NumV
  (match lst
    ['() (error 'mult_func "ZODE : empty multiply list, no arguments to multiply.")]
    [(list (NumV n1) (NumV n2)) (NumV (* n1 n2))]
    [else (error 'mult_func "ZODE : unable to multiply arguments ~e" lst)]))



; div_func takes in a (Listof Value) and returns a NumV result of the two values divided.
(define (div_func [lst : (Listof Value)]) : NumV
  (match lst
    ['() (error 'div_func "ZODE : empty division list, no arguments to divide.")]
    [(list (NumV n1) (NumV n2)) (if (not (= 0 n2))
                                    (NumV (/ n1 n2))
                                    (error 'div_func "ZODE : unable to perform division by zero."))
                                ]
    [else (error 'div_func "ZODE : unable to divide arguments ~e" lst)]))


; ltoreq_func takes in a (Listof Value) and returns a boolean.
(define (ltoreq_func [lst : (Listof Value)]) : BoolV
  (match lst
    ['() (error 'ltoreq_func "ZODE : unable to determine boolean value given an empty list.")]
    [(list (NumV n1) (NumV n2)) (BoolV (<= n1 n2))]
    [else (error 'ltoreq_func "ZODE : cannot determine <= with given values. ~e" lst)]))


; equal?_func takes in a (Listof Value) and returns a boolean.
(define (equal?_func [lst : (Listof Value)]) : BoolV
  (match lst
    [(list val1 val2) (BoolV (equal? val1 val2))]
    [else (error 'equal?_func "ZODE : unable to determine boolean value given an empty list.")]))


; error_func takes in a (Listof Value) and returns nothing. (halts the program)
(define (error_func [lst : (Listof Value)])
  (match lst
    ['() (error 'error_func "ZODE : no arguments given to error function.")]
    [(list v) (error 'error_func "ZODE : user-error ~e" (serialize v))]))

; -- ZODE5 Functions --

; println takes in string s and prints it to stdout followed by a newline.
; returns true on success, error on failure
(define (println [s : (Listof Value)]) : BoolV
  (match s
    ['() (printf "\n") (BoolV #t)]
    [(list (StringV s)) (printf s) (printf "\n") (BoolV #t)]
    [else (error 'println "ZODE : Unexpected number of arguments given to println, got ~e" s)]))



; read-num reads a line of numeric input from the terminal and returns the read number.
; an error is signaled if the input is not a real number.
(define (read-num [null : (Listof Value)]) : NumV
  (display "> ")
  (define user-input (read-line))
  (match user-input
    [(? string? s) (define num (string->number s))
                   (if (and num (real? num))
                       (NumV num)
                       (error 'read-num "ZODE : unable to read the number. Expected a real"))]
    [else (error 'read-num "ZODE : Unexpected end of input. EOF")]))



; read-str reads a line of alphabetic input from the terminal and returns the read string.
(define (read-str [null : (Listof Value)]) : StringV
  (display "> ")
  (define user-input (read-line))
  (match user-input
    [(? string? s) (StringV s)]
    [else (error 'read-num "ZODE : Unexpected end of input. EOF")]))


; seq takes in a number of expressions and returns the value of the last one.
(define (seq [lst : (Listof Value)]) : Value
  (match lst
    ['() (error 'seq "ZODE : invalid number of arguments given to seq.")]
    [(list vals ...) (last vals)]))


; ++ takes in a (Listof Value) and returns a StringV.
; the function joins together the arguments into a single string.
(define (++ [lst : (Listof Value)]) : StringV
  (define strs
    (map (lambda ([s : Value]) : String
           (match s
             [(StringV str) str]
             [(NumV n) (number->string n)]
             [(BoolV b) (if b "true" "false")]
             [else (error '++ "ZODE : Unsupported value given for concatenation, got ~e" s)]))
         lst))
  (StringV (string-join strs "")))


; -- top-level-env --
(define top-env (list
                 (Binding 'true (BoolV #t))
                 (Binding 'false (BoolV #f))
                 (Binding '+ (PrimOpV add_func))
                 (Binding '- (PrimOpV sub_func))
                 (Binding '* (PrimOpV mult_func))
                 (Binding '/ (PrimOpV div_func))
                 (Binding '<= (PrimOpV ltoreq_func))
                 (Binding 'equal? (PrimOpV equal?_func))
                 (Binding 'error (PrimOpV error_func))
                 (Binding 'seq (PrimOpV seq))
                 (Binding 'println (PrimOpV println))
                 (Binding 'read-num (PrimOpV read-num))
                 (Binding 'read-str (PrimOpV read-str))
                 (Binding '++ (PrimOpV ++))))


; --- defined variables ---:

; testenv is a variable that is an Env so that we can test our environment functions.
(define testenv (list (Binding 'x (NumV 7)) (Binding 'y (NumV 8)) (Binding 'z (NumV 9))))
(define testenv2 (list (Binding 'func (ClosV '(a b c) (NumC 5) mt-env))))
(define testenv3 (list (Binding 'x (NumV 3)) (Binding 'y (NumV 4)) (Binding 'z (NumV 5))))


; lookup takes in an a Symbol and a (Listof Binding) and returns a Real
; (the value that maps to the symbol)
(define (lookup [id : Symbol] [env : Env]) : Value
  (cond
    [(empty? env)
     (error 'lookup "ZODE : empty environment or unable to find value in environment.")]
    [(cons? env)
     (cond
       [(equal? id (Binding-name (first env))) (Binding-val (first env))]
       [else (lookup id (rest env))])]))



; add-to-env takes in a Symbol, a Real, and an environment and adds the Symbol Real pair to the environment.
; on success, the updated environment is returned.
(define (add-to-env [param : Symbol] [arg : Value] [env : Env]) : Env
  (cons (Binding param arg) env))


; extend-env takes in a new environment to be created for the static scope
; a list of parameters given by the function definitions, and a list of arguments from the function call
; extend-env returns a new environment for the static scope of the function
(define (extend-env [n-env : Env] [parameters : (Listof Symbol)] [arguments : (Listof Value)]) : Env
  (cond
    ; when both lists are empty, return the body with all of the substitutions done
    [(and (empty? parameters) (empty? arguments)) n-env]
    ;; If one list is empty and the other is not, throw an error
    [(or (empty? parameters) (empty? arguments))
     (error 'create-env "ZODE : Lists of parameters and arguments are not of equal length")]
    [else ; otherwise, we need to populate the new environment and recurse.
     (let ([n-env (add-to-env (first parameters) (first arguments) n-env)])
       (extend-env n-env (rest parameters) (rest arguments)))]))


; interp takes in an ExprC and an environment and returns a Value.
(define (interp [expr : ExprC] [env : Env]) : Value
  (match expr
    [(NumC n) (NumV n)]
    [(AppC f args) (define func (interp f env))
                   (define arguments (map (lambda([a : ExprC]) (interp a env)) args))
                   (match func
                     [(PrimOpV op) (op arguments)]
                     [else (error 'interp "ZODE : unexpected first expression in AppC, got ~e" func)])]
    [other (error 'interp "ZODE : unimplemented")]))



; parse-ids takes in a (Listof Sexp) and returns a (Listof Symbol),
; the returning list is a list of id's
(define (parse-ids [s : (Listof Sexp)]) : (Listof Sexp)
  (match s
    [(list id '= expr) (cons id '())]
    [(list id '= expr ': rest ...) (cons id (parse-ids rest))]
    [else (error 'parse-ids "ZODE : invalid clause given to parse-ids, got ~e" s)]))

(check-equal? (parse-ids '(z = 7 : y = 3)) '(z y))
(check-equal? (parse-ids '(z = {+ z 6} : y = 3)) '(z y))
(check-exn #px"ZODE : invalid" (λ () (parse-ids '())))



; parse-exprs takes in a (Listof Sexp) and returns a (Listof exprC)
(define (parse-exprs [s : (Listof Sexp)]) : (Listof Sexp)
  (match s
    [(list id '= expr) (cons expr '())]
    [(list id '= expr ': rest ...) (cons expr (parse-exprs rest))]
    [else (error 'parse-exprs "ZODE : invalid clause given to parse-exprs, got ~e" s)]))

(check-equal? (parse-exprs '(z = {+ z 6} : y = 3 : x = {- 4 5})) '({+ z 6} 3 {- 4 5}))
(check-exn #px"ZODE : invalid" (λ () (parse-exprs '())))



; parse takes in an s-expression and returns an ExprC if the
; given s-expression can be transformed into one. Error otherwise.
(define (parse [sexp : Sexp]) : ExprC
  (match sexp
    [(? real? r) (NumC r)]
    [(? string? s) (StringC s)]
    [(? symbol? s) (if (member s '(if lamb locals))
                       (error 'parse "ZODE : s-expression failed to match EBNF syntax.")
                       (IdC s))]
    [(list 'if ': g ': t ': e) (IfC (parse g) (parse t) (parse e))]
    [(list 'lamb ': (? symbol? args) ... ': body)
     (if (and (andmap symbol? args)
              (not (check-duplicates args))
              (not (ormap (lambda (arg) (member arg '(if lamb locals : =))) args)))
         (lamC args (parse body))
         (error 'parse "ZODE : Unable to parse lamb function."))]
    
    [(list 'locals ': clauses ... ': body)
     (define ids (parse-ids (cast clauses (Listof Sexp))))
     (define exprs (parse-exprs (cast clauses (Listof Sexp))))
     (parse `{{lamb : ,@ids : ,body} ,@exprs})]
    [(list f args ...) (AppC (parse f) (map parse args))]
    [else (error 'parse "ZODE : expected s-expression, got ~e" sexp)]))


; top-interp takes in an s-expression and returns a String.
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))


; ZODE5 Program of our own - This program takes an integer from
; the user and calculates the factorial of the user's number
(define prog '{locals
               : cons = {lamb : f r :
                              {lamb : key :
                                    {if : {equal? key 0}
                                        : f
                                        : r}}}
               : first = {lamb : pair :
                               {pair 0}}
               : rest = {lamb : pair :
                              {pair 1}}
               : {locals
                  : fact = {lamb : n self :
                                 {if : {equal? n 1}
                                     : 1
                                     : {* n {self {- n 1} self}}}}
                  : {seq
                     {println "Enter an integer to calculate its factorial: "}
                     {locals : user-num = {read-num}
                             : {println {++ "The factorial of " user-num " is " {fact user-num fact} "."}}}}}})


;(top-interp prog)

; ------------------- Test-Cases -------------------

;(top-interp '{locals : your-number = {read-num} : y = 7 : {+ your-number y}})

; test-cases for println:
;(check-equal? (println '()) (BoolV #t))
;(check-equal? (println (list (StringV "foo"))) (BoolV #t))
(check-exn #px"ZODE : Unexpected" (λ () (println (list (StringV "foo") (StringV "bob")))))


; test-cases for seq:
(check-equal? (seq (list (StringV "ok") (BoolV #t) (NumV 5))) (NumV 5))
(check-exn #px"ZODE : invalid" (λ () (seq '())))


; test-cases for ++:
(check-equal? (++ (list (StringV "hello") (StringV "world"))) (StringV "helloworld"))
(check-equal? (++ (list (StringV "hello") (NumV 4) (StringV "world"))) (StringV "hello4world"))
(check-equal? (++ (list (StringV "hello") (StringV "world") (NumV 5))) (StringV "helloworld5"))
(check-equal? (++ (list (StringV "hello") (StringV "world") (NumV 5) (BoolV #t))) (StringV "helloworld5true"))
(check-equal? (++ (list (StringV "hello") (StringV "world") (NumV 5) (BoolV #f))) (StringV "helloworld5false"))
(check-exn #px"ZODE : Unsupported" (λ () (++ (list (StringV "hello")
                                                   (StringV "world") (NumV 5)
                                                   (BoolV #f) (ClosV (list 'a) (NumC 5) testenv)))))
(check-equal? (++ '()) (StringV ""))



; test-cases for serialize:
(check-equal? (serialize (StringV "hello")) "\"hello\"")
(check-equal? (serialize (ClosV (list 'a) (NumC 5) testenv)) "#<procedure>")
(check-equal? (serialize (PrimOpV add_func)) "#<primop>")
(check-equal? (serialize (NumV 5)) "5")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")

; test-cases for top-interp:
;(check-equal? (top-interp '{{lamb : a b c : {+ a b}} 4 5 6}) "9")
;(check-equal? (top-interp '{{lamb : a b c : 3} 4 5 6}) "3")
(check-equal? (top-interp '{+ 1 2}) "3")
;(check-equal? (top-interp '{locals : z = 4 : y = 9 : {+ z y}}) "13")


; test-cases for parse:
(check-equal? (parse 5) (NumC 5))
(check-equal? (parse '{+ 1 2}) (AppC (IdC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse "hello") (StringC "hello"))
(check-equal? (parse '(foo 5 6 7)) (AppC (IdC 'foo) (list (NumC 5) (NumC 6) (NumC 7))))
(check-equal? (parse '{lamb : a : 3}) (lamC (list 'a) (NumC 3)))
(check-equal? (parse '{if : x : 5 : 0}) (IfC (IdC 'x) (NumC 5) (NumC 0)))
(check-equal? (parse '{locals : z = 4 : {+ z 5}})
              (AppC (lamC (list 'z)
                          (AppC (IdC '+)
                                (list (IdC 'z) (NumC 5)))) (list (NumC 4))))
(check-equal? (parse '{+ {h} {f 3}}) (AppC (IdC '+) (list (AppC (IdC 'h) '())
                                                          (AppC (IdC 'f) (list (NumC 3))))))


;; (check-exn (regexp (regexp-quote "ZODE : duplicate"))
;;            (lambda () (parse '{locals : {z = 4} {z = 7} : {+ z 5}})))
;; (check-exn (regexp (regexp-quote "ZODE : symbol"))
;;            (lambda () (parse '{locals : {if = 7} : {+ z 5}})))
(check-exn (regexp (regexp-quote "ZODE : s-expression failed to"))
           (lambda () (parse '{if : x : 5 :})))
(check-exn (regexp (regexp-quote "ZODE : Unable"))
           (lambda () (parse '{lamb : a a : 3})))
(check-exn (regexp (regexp-quote "ZODE : expected s-expression"))
           (lambda () (parse '())))



; added test cases, failed in turn-in orginally:
(check-exn #px"ZODE" (λ () (parse '(+ if 4))))
;(check-equal? (top-interp (quote (locals : z = (lamb : : 3) : q = 9 : (+ (z) q)))) "12")
;(check-exn #px"ZODE" (λ () (top-interp '(+ 4 (error "1234")))))

; Zode 5 ---
;(check-equal? (top-interp (quote (++ "abc"))) "\"abc\"")


; test-cases for interp:
(check-equal? (interp (NumC 5) testenv) (NumV 5))
;(check-equal? (interp (StringC "yo") testenv) (StringV "yo"))
;(check-equal? (interp (IdC 'x) testenv) (NumV 7))
;(check-equal? (interp (lamC '(x y z) (NumC 5)) testenv) (ClosV '(x y z) (NumC 5) testenv))
;(check-equal? (interp (IfC (IdC 'false) (NumC 1) (NumC 2)) top-env) (NumV 2))
;(check-equal? (interp (IfC (IdC 'true) (NumC 1) (NumC 2)) top-env) (NumV 1))
;(check-exn #px"ZODE : unable" (λ () (interp (IfC (NumC 3) (NumC 1) (NumC 2)) top-env)))
;(check-equal? (interp (AppC (lamC '(x y) (NumC 5)) (list (NumC 3) (NumC 4))) testenv) (NumV 5))
;(check-equal? (interp (AppC (lamC '(x y) (AppC (IdC '+) (list (NumC 5) (NumC 6))))
;                            (list (NumC 3) (NumC 4))) top-env) (NumV 11))
;(check-equal? (interp (AppC (lamC '(x y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))))
;                            (list (NumC 3) (NumC 4))) top-env) (NumV 7))
;(check-exn #px"ZODE : Unequal" (λ () (interp (AppC (lamC '(x x) (NumC 5)) (list (NumC 3) (NumC 4))) testenv)))
;(check-exn #px"ZODE : unexpected" (λ () (interp (AppC (NumC 5) (list (NumC 3) (NumC 4))) testenv)))


; test-cases for extend-env:
(check-exn #px"ZODE : Lists" (λ () (extend-env mt-env '(a b) (list (NumV 4) (NumV 5) (NumV 6) (NumV 7)))))


; test-cases for add-to-env:
(check-equal? (add-to-env 'a (NumV 4) '()) (list (Binding 'a (NumV 4))))
(check-equal? (add-to-env 'a (NumV 4) (list (Binding 'x (NumV 7)) (Binding 'y (NumV 14))))
              (list (Binding 'a (NumV 4))(Binding 'x (NumV 7)) (Binding 'y (NumV 14))))


; test-cases for lookup:
(check-exn #px"ZODE : empty environment" (λ () (lookup 'a mt-env)))
(check-equal? (lookup 'x testenv) (NumV 7))
(check-equal? (lookup 'z testenv) (NumV 9))

; test-cases for error_func:
(check-exn #px"ZODE : no arguments" (λ () (error_func '())))
(check-exn #px"ZODE : user-error"  (λ () (error_func (list (NumV 8)))))

; test-cases for equal?_func:
(check-equal? (equal?_func (list (NumV 2) (NumV 2))) (BoolV #t))
(check-equal? (equal?_func (list (NumV 2) (NumV 1))) (BoolV #f))
(check-equal? (equal?_func (list (StringV "hello") (StringV "world"))) (BoolV #f))
(check-equal? (equal?_func (list (BoolV #f) (BoolV #f))) (BoolV #t))
(check-exn #px"ZODE : unable" (λ () (equal?_func '())))


; test-cases for ltoreq_func:
(check-equal? (ltoreq_func (list (NumV 1) (NumV 2))) (BoolV #t))
(check-equal? (ltoreq_func (list (NumV 2) (NumV 1))) (BoolV #f))
(check-exn #px"ZODE : cannot" (λ () (ltoreq_func (list (BoolV #t) (NumV 0)))))
(check-exn #px"ZODE : unable" (λ () (ltoreq_func '())))

; test-cases for div_func:
(check-equal? (div_func (list (NumV 4) (NumV 2))) (NumV 2))
(check-exn #px"ZODE : empty" (λ () (div_func '())))
(check-exn #px"ZODE : unable to divide" (λ () (div_func (list (NumV 7) (BoolV #t)))))
(check-exn #px"ZODE : unable to perform" (λ () (div_func (list (NumV 7) (NumV 0)))))

; test-cases for mult_func:
(check-equal? (mult_func (list (NumV 4) (NumV 4))) (NumV 16))
(check-exn #px"ZODE : empty" (λ () (mult_func '())))
(check-exn #px"ZODE : unable" (λ () (mult_func (list (NumV 7) (BoolV #t)))))

; test-cases for sub_func:
(check-equal? (sub_func (list (NumV 4) (NumV 4))) (NumV 0))
(check-exn #px"ZODE : empty" (λ () (sub_func '())))
(check-exn #px"ZODE : unable" (λ () (sub_func (list (NumV 7) (BoolV #t)))))

; test-cases for add_func:
(check-equal? (add_func (list (NumV 3) (NumV 4))) (NumV 7))
(check-exn #px"ZODE : empty" (λ () (add_func '())))
(check-exn #px"ZODE : unable" (λ () (add_func (list (NumV 7) (BoolV #t)))))

