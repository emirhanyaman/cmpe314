#lang plai-typed
;; Emirhan YAMAN - 111200031
;; lang is my programming language name.
;; lang contains lang-num, lang-add, lang-Ssub, lang-mul, lang-exp, lang-min functions
;; lang-num is a function for checking that is parameter a number.
;; lang-add for adding two lang-num. example: (lang-add (lang-num 6) (lang-num 4)) --> 10
;; lang-sub for subtracting two lang-num. example: (lang-sub (lang-num 15) (lang-num 5)) --> 10
;; lang-mul for multipling two lang-num. example: (lang-mul (lang-num 5) (lang-num 2)) --> 10
;; lang-exp for exponential. lang-exp takes two lang-num. First one is main number, second one is power. example: (lang-exp (lang-num 2) (lang-num 4)) --> 16
;; lang-exp example: (lang-exp (lang-num 5) (lang-num 0)) --> 1
;; lang-exp example: (lang-exp (lang-num 0) (lang-num 0)) --> 0
;; lang-min for getting minus a lang-num. example: (lang-min (lang-num 5)) --> -5
;; Grammar is
;; S-->(a + b)S
;; S-->(a * b)S
;; S-->(a ** b)S
;; S-->(a - b)S
;; S-->a'S

(define-type lang
 [lang-num (n : number)]
 [lang-add (l : lang) (r : lang)]
 [lang-sub (l : lang) (r : lang)]
 [lang-mul (l : lang) (r : lang)]
 [lang-exp (l : lang) (r : lang)]
 [lang-min (l : lang)]
  )

(define (eval [expr : lang])
  (type-case lang expr
    [lang-num (n) n]
    [lang-add (l r) (+ (eval l) (eval r))]
    [lang-sub (l r) (- (eval l) (eval r))]
    [lang-mul (l r) (* (eval l) (eval r))]
    [lang-exp (l r) (funct (eval l) (eval r))]
    [lang-min (l) (- 0 (eval l))]
  )
)

(test (eval (lang-num 7)) 7)
(test (eval (lang-add (lang-num 3) (lang-num 4))) 7)
(test (eval (lang-sub (lang-num 10) (lang-num 5))) 5)
(test (eval (lang-mul (lang-num 10) (lang-num 5))) 50)
(test (eval (lang-min (lang-num 10) )) -10)


;;funct is a method that takes 2 parameters.
;;parameter n is number.
;;parameter p is power.
;;first of all, method checks p.
;;if p is 0, result is 1.
;;if p is 1; result is n.
;;if p bigger than 1 method do that n = n*n and p=p-1 until p=0.

(define (funct n p)
  (cond
    [(= p 0) 1]	
    [(= p 1) n]
    [else (* n (funct n (- p 1)))]))

(test (eval (lang-exp (lang-num 3) (lang-num 3))) 27)

;; parse s-expression -> lang
;; convert a quoted s expression into the equivalent lang form
;; Grammar: ((+,-,*,**) (0,1,2,3,4,5,6,7,8,9) (0,1,2,3,4,5,6,7,8,9)) --> (0,1,2,3,4,5,6,7,8,9)
;; Examples:
;; '3 -> (lang-num 3)
;; '(+ 2 4) -> (lang-add (lang-num 2) (lang-num 4))
;; '(- 3) -> (lang-min (lang-num 3))
;; '(- 3 1) -> (lang-sub (lang-num 3) (lang-num 1))
;; '(* 3 2) -> (lang-mul (lang-num 3) (lang-num 2))
;; '(** 2 5) -> (lang-exp (lang-num 2) (lang-num 5))

(define (parse [s : s-expression]) : lang
  (cond
    [(s-exp-number? s) (lang-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (lang-add (parse (second sl)) (parse (third sl)))]
         [(-) (cond [(= 2 (length sl)) (lang-min (parse (second sl)))] ;; if symbol is (-) and there is no third exp, lang-num will be minus
              [else (lang-sub (parse (second sl)) (parse (third sl)))]) ;; else do subracting
              ]
         [(*) (lang-mul (parse (second sl)) (parse (third sl)))]
         [(**) (lang-exp (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]
    ))


(test (parse '(- 3)) (lang-min (lang-num 3)))
(test (parse '(- 8)) (lang-min (lang-num 8)))
(test (parse '(- -3)) (lang-min (lang-num -3)))
(test (parse '(+ (- 3) (* 5 (- (* 7 6))))) (lang-add (lang-min (lang-num 3)) (lang-mul (lang-num 5) (lang-min (lang-mul (lang-num 7) (lang-num 6)))))) 
(test (eval (parse '(+ (- 3) (* 5 (- (* 7 6)))))) -213)

;; output-reverse-polish msl -> list of s-expression
;; output-reverse-polish msl hepls to change operator position
;; For example:
;; (+ 3 4) will be (3 4 +)
;; (* 5 2) will be (5 2 *)

(define (output-reverse-polish [expr : lang])
  (type-case lang expr
    [lang-num (num) (list (number->s-exp num))]
    [lang-add (l r) (append (append (output-reverse-polish l) (output-reverse-polish r))
                            (list (symbol->s-exp '+)))]
    [lang-mul (l r) (append (append (output-reverse-polish l) (output-reverse-polish r))
                            (list (symbol->s-exp '*)))]
    [else (error 'output-reverse-polish "invalid")]))


(test (output-reverse-polish (lang-num 5)) (s-exp->list '(5)))
(test (output-reverse-polish (lang-add (lang-num 3) (lang-num 8))) (s-exp->list '(3 8 +)))
(test (output-reverse-polish (lang-mul (lang-num 9) (lang-num 5))) (s-exp->list '(9 5 *)))
(test (output-reverse-polish (lang-add (lang-mul (lang-num 1) (lang-num 2)) (lang-num 3))) (s-exp->list '(1 2 * 3 +)))
(test (output-reverse-polish (lang-mul (lang-num 3) (lang-add (lang-num 4) (lang-num 9)))) (s-exp->list '(3 4 9 + *)))


;;Sugaring
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [subS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [expS (l : ArithS) (r : ArithS)]
  [unaryS (l : ArithS)])

;;Desugaring
(define (desugar [as : ArithS]) : lang
  (type-case ArithS as
    [numS (n) (lang-num n)]
    [plusS (l r) (lang-add (desugar l)
                           (desugar r))]
    [multS (l r) (lang-mul (desugar l)
                           (desugar r))]
    [subS (l r) (lang-sub (desugar l)
                          (desugar r))]
    [expS (l r) (lang-exp (desugar l)
                          (desugar r))]
    [unaryS (l) (lang-min (desugar l))]))

(test (desugar (plusS (numS 1) (numS 2))) (lang-add (lang-num 1) (lang-num 2)))
(test (desugar (plusS (numS 15) (unaryS (numS 13)))) (lang-add (lang-num 15) (lang-min (lang-num 13))))
(test (desugar (subS (numS 4) (numS 3))) (lang-sub (lang-num 4) (lang-num 3)))
(test (desugar (subS (numS 12) (numS 32))) (lang-sub (lang-num 12) (lang-num 32)))
(test (desugar (multS (numS 37) (numS 44))) (lang-mul (lang-num 37) (lang-num 44)))
(test (desugar (multS (numS 128) (numS 4))) (lang-mul (lang-num 128) (lang-num 4)))
(test (desugar (expS (numS 95) (numS 82))) (lang-exp (lang-num 95) (lang-num 82)))
(test (desugar (expS (numS 22) (numS 88))) (lang-exp (lang-num 22) (lang-num 88)))
(test (desugar (unaryS (numS 1))) (lang-min (lang-num 1)))
(test (desugar (unaryS (numS 18))) (lang-min (lang-num 18)))


;; convert a quoted s expression into the equivalent lang form
;; Examples:
;; '3 -> (lang-num 3)
;; '(+ 2 4) -> (plusS (numS 2) (numS 4))
;; '(- 3) -> (unaryS (numS 3))
;; '(- 3 1) -> (subS (numS 3) (numS 1))
;; '(* 3 2) -> (mults (numS 3) (numS 2))
;; '(** 2 5) -> (expS (numS 2) (numS 5))



(define (parser-infix [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond 
         [(s-exp-symbol? (first sl))
          (case (s-exp->symbol (first sl))
            [(-) (unaryS (parser-infix (second sl)))]
            [else (error 'parser-infix "invalid input")]
            )]
         [(s-exp-number? (first sl))
          (case (s-exp->symbol (second sl))
            [(+) (plusS (parser-infix (first sl)) (parser-infix (third sl)))]
            [(-) (subS (parser-infix (first sl)) (parser-infix (third sl)))]
            [(*) (multS (parser-infix (first sl)) (parser-infix (third sl)))]
            [(**) (expS (parser-infix (first sl)) (parser-infix (third sl)))]
            [else (error 'parser-infix "invalid list input")])]
         [else (error 'parser-infix "invalid input")]))]
     [else (error 'parser-infix "invalid input")]))

(test (parser-infix '(- 3)) (unaryS (numS 3)))
(test (parser-infix '(2 + 3)) (plusS (numS 2) (numS 3)))
(test (parser-infix '(4 * 2)) (multS (numS 4) (numS 2)))
(test (parser-infix '(5 ** 8)) (expS (numS 5) (numS 8)))
(test (parser-infix '(100 - 99)) (subS (numS 100) (numS 99)))




;;if-greater-than-zero function takes two parameters
;; If the value of expression1 greater than zero, the value of
;; this expression will the value of expression2, otherwise the value of expression3.

(define (if-greater-than-zero expression1 expression2 expression3)
  (cond
    [(> expression1 0) expression2]
    [else expression3]))

(test (if-greater-than-zero 6 8 1) 8)
(test (if-greater-than-zero -3 7 15) 15)
