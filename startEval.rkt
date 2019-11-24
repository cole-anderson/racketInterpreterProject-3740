
#lang racket

#|
CPSC 3740 SPRING 2019
COURSE PROJECT
"Racket-Ception"
StartEval Program
CONTRIBUTORS:
NICOLE VACHON
COLE ANDERSON
|#

(define (startEval entry)
  (SecondEval entry '()) ;start evaluation with an empty stack
  )

(define (SecondEval entry2 stack)
  (if (list? entry2)
      (let ([operator (knownOperator? (car entry2) stack)]) (let ([entry (append (list operator) (cdr entry2))])
      (cond
        ;checks if operator is already known in the stack and
        ;if so, then replaces it in the function call, renamed entry from entry2
        ;then determines what the proper operation to execute

        ;1)CONSTANTS AND VARIABLES AND QUOTE: //COMPLETE
        ;  Constants and variables are implemented last as it is the else condition on the if statement
        [(equal? 'quote operator) (cadr entry)]

        ;2)ARITHMETIC OPERATORS: //COMPLETE
        [(equal? '+ operator) (+ (SecondEval (cadr entry) stack) (SecondEval (caddr entry) stack))]
        [(equal? '- operator) (- (SecondEval (cadr entry) stack) (SecondEval (caddr entry) stack))]
        [(equal? '/ operator) (/ (SecondEval (cadr entry) stack) (SecondEval (caddr entry) stack))]
        [(equal? '* operator) (* (SecondEval (cadr entry) stack) (SecondEval (caddr entry) stack))]

        ;3)RELATIONAL OPERATORS //COMPLETE
        [(equal? '= operator) (= (SecondEval (cadr entry) stack) (SecondEval (caddr entry) stack))]
        [(equal? '<= operator) (<= (SecondEval (cadr entry) stack) (SecondEval (caddr entry) stack))]
        [(equal? '< operator) (< (SecondEval (cadr entry) stack) (SecondEval (caddr entry) stack))]
        [(equal? '>= operator) (>= (SecondEval (cadr entry) stack) (SecondEval (caddr entry) stack))]
        [(equal? '> operator) (> (SecondEval (cadr entry) stack) (SecondEval (caddr entry) stack))]
        [(equal? 'equal? operator) (eqEval (cdr entry) stack)]

        ;4)LISTS: CAR, CDR, CONS, PAIR? //COMPLETE
        [(equal? 'car operator) (if (list? cadr) (car (knownNotEval? (cadadr entry) stack)) (car (knownNotEval? (cadr entry) stack)))]
        [(equal? 'cdr operator) (if (list? cadr) (cdr (knownNotEval? (cadadr entry) stack)) (cdr (knownNotEval? (cadr entry) stack)))]
        [(equal? 'pair? operator) (pair? (knownNotEval? (cdr entry) stack))]


        [(equal? 'cons operator) (cons (known? (cadr entry) stack) (SecondEval (caddr entry) stack))]

        ;5) CONDITIONAL: IF //COMPLETE
        [(equal? 'if operator) (if (SecondEval (cadr entry) stack) ;calls starteval to evaluate function
                                   (SecondEval (caddr entry) stack) ;does this entry if true
                                   (SecondEval (cadddr entry) stack))]


        #|TODO: LAMBDA |#
        ;6)LAMBDA EXPRESSION: SINGLE EXPRESSION LAMBDA AND FUNCTION APPLICATION
        [(equal? 'lambda operator) (if (member (cadr entry) stack)
                                       (SecondEval(caddr entry) stack)
                                       (write 'function))] ;writes function if lambda without funciton application is called
        ; assumes lambda will be called only as itself or
        ; that it is a function call if formal params are in stack


        #|TODO: LOCAL BINDING |#
        ;8)LOCAL BINDING: LET LETREC
	[(equal? 'let operator) (SecondEval (caddr entry)  (append (cadr entry) stack))]
        [(equal? 'letrec operator) (SecondEval (caddr entry) (append (cadr entry) stack))]

        ; Extra conditions: operator as a list (for lambda application) and entry is a list (for lists as constants/variables
        [(list? operator) (if (pair? (caar entry))
                              (SecondEval (list (caddaar entry) (cadar entry)) (process (cadaar entry) (cdr entry) stack))
                              (SecondEval (caddar entry) (process (cadar entry) (cdr entry) stack)))]
                               ; cadar entry = formal parameters
                               ; cdr entry = list of actual parameters
        [(list? entry) (knownNotEval? operator stack)]


      ))) ;)
      ;else
      (known? entry2 stack)))
;END MAIN


#|HELPER FUNCTIONS|#

#|HELPER FUNCTION PROCESS
Parameters: param: formal parameters
            act: actual parameters
            stack: stack
Function: pair formal parameters with actual parameters then append to stack
Returns: newly made stack|#
(define (process param act stack)
  (if (or (null? param) (null? act))
      stack
     (append (list (list (car param) (SecondEval (car act) stack))) (append (processHelp (cdr param) (cdr act) stack) stack))))

#|HELPER FUNCTION PROCESSHELP
Parameters: param: formal parameters
            act: actual parameters
            stack: stack
Function: pair formal parameters with actual parameters then append to empty stack to avoid repeats in stack
Returns: newly made stack|#
(define (processHelp param act stack)
  (if (or (null? param) (null? act))
      '()
     (append (append (list (list (car param) (SecondEval (car act) stack))) (processHelp (cdr param) (cdr act) stack)) '())))

#|HELPER FUNCTION caddaar
Does exactly what you expect it to do on the parameter 'func'
|#
(define (caddaar func) (car (cddaar func)))


#|HELPER FUNCTION KNOWNNOTEVAL
Parameters: elem: element to find in stack
            stack: stack
Function: finds element in stack if known and returns statement without evaluation
(elem was sometimes given in a list when it should be evaluated as a element)
Returns: statement equivalent of element given
(like returning value of name-value pair)|#
(define (knownNotEval? elem stack)
  (if (list? elem)
      (knownNotEval? (car elem) stack)
  (if (number? elem)
      elem
      (if (null? stack)
          elem
          (if (equal? elem (caar stack))
              (cadar stack)
              (knownNotEval? elem (cdr stack)))))))

#|HELPER FUNCTION KNOWNOPERATOR
Parameters: elem: element to find in stack
            stack: stack
Function: finds element in stack if known and returns statement without evaluation
Returns: statement equivalent of element given
(like returning value of name-value pair)|#
(define (knownOperator? elem stack)
  (if (number? elem)
      elem
      (if (null? stack)
          elem
          (if (equal? elem (caar stack))
              (cadar stack)
              (knownOperator? elem (cdr stack))))))

#|HELPER FUNCTION KNOWN
Parameters: elem: element to find in stack
            stack: stack
Function: finds element in stack if known and returns evaluation of the statement
Returns: evaluation of the value of the element given
(like returning an evaluated value of name-value pair)|#
(define (known? elem stack)
  (if (list? elem)
      (known? (car elem) stack)
  (if (number? elem)
      elem
      (if (null? stack)
          elem
          (if (equal? elem (caar stack))
              (if (pair? (cadar stack))
                  (SecondEval (cadar stack) stack)
                  (cadar stack))
              (known? elem (cdr stack)))))))

#|HELPER FUNCTION EQEVAL
Parameters: entry: list of two expression to check if equal
            stack: stack
Function: checks the evaluation of both expressions to determine if they are equal
Returns: true or false|#
(define (eqEval entry stack)
  (equal? (SecondEval (car entry) stack) (SecondEval (cadr entry) stack)))


;END PROGRAM
#|

;Success: A B C E F G
;Fail: D H

(write "Test A")
 (startEval
 '(let ((inc
         (lambda (x) (+ x (quote 1)))))
        (inc (quote 5)))
 )

(print "should be 6")
(newline)

(write "Test B")
(startEval
 '(letrec ((fact
           (lambda (x)
	     (if (= x 0) (quote 1)
		(* x (fact (- x 1)))))))
	  (fact 10)))
(print "should be 3628800")
(newline)

(write "Test C")
 (startEval
  '(letrec ((fib
            (lambda (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))

           (fib 7))
 )
(print "should be 21")
(newline)

(write "Test D")
(startEval '(
              (
                (lambda (x) (lambda (y) (+ x y)))
                1
              )
              2
            )
 )

(print "should be 3")
(newline)

(write "Test E")
(startEval '(let ((+ (lambda (x) (cdr x)))
                   (- '(1 2 3 4 5)))
               (+ -))
 )

(print "should be (2 3 4 5)")
(newline)

(write "Test F")
(startEval (let ([sub1 (lambda (x) (- x 1))]
                  [not (lambda (x) (if x #f #t))])


              (letrec ([is-even? (lambda (n)
                                   (if (= n '0)
                                       #t
                                       (is-odd? (sub1 n))))]
                       [is-odd? (lambda (n)
                                  (if (not (= n '0))
                                      (is-even? (sub1 n))
                                      '#f
                                      ))])
                (is-odd? 11))))
(print "should be true")
(newline)

(write "Test G")
(startEval
 '(letrec ((fact
           (lambda (x)
	     (if (= x 0) (quote 1)
		(* x (fact (- x 1)))))))
	  (fact 10))
 )
(print "should be 3628800")
(newline)
(write "Test H")
(startEval
  '(let ((y 10))
     (let ((f (lambda (x) (+ x y))))
       (let ((y 100))
         (f 2)))))
(print "should be 12")
(newline)
(newline)
(newline)

(write "ourTest1:")
(startEval '(let ([y 5][x 3]) (let ([x 7]) (+ x y))))
(write "Expected")(let ([y 5][x 3]) (let ([x 7]) (+ x y)))
(write "ourTest2:")
(startEval '(lambda (x y) (+ x y)))
(write "Expected")(lambda (x y) (+ x y))
;This ones broken:
;(write "ourTest3:")
;(startEval '(let ([x (car '(1 4 5))][y (car '(4 5))]) (+ x y)))
;(write "Expected")(let ([x (car '(1 4 5))][y (car '(4 5))]) (+ x y))
(write "ourTest4:")
(startEval '(let ((inc(lambda (x) (+ x (quote 1)))))(inc (quote 5))))
(write "Expected")(let ((inc(lambda (x) (+ x (quote 1)))))(inc (quote 5)))
(write "ourTest5:")
(startEval '(((lambda (x) (lambda (y) (+ x y))) 1) 2))
(write "Expected")(((lambda (x) (lambda (y) (+ x y))) 1) 2)
(write "ourTest6:")
(startEval '(letrec ((fact (lambda (x) (if (= x 0) (quote 1) (* x (fact (- x 1))))))) (fact 10)))
(write "Expected")(letrec ((fact (lambda (x) (if (= x 0) (quote 1) (* x (fact (- x 1))))))) (fact 10))
(write "ourTest7:")
(startEval '(+ (quote 5) (quote 3)))
(write "Expected")(+ (quote 5) (quote 3))
(write "ourTest8:")
(startEval '(let ([+ *]) (+ 3 4)))
(write "Expected")(let ([+ *]) (+ 3 4))
(write "ourTest9:")
(startEval '(letrec ([e 5][! *]) (! e e)))
(write "Expected")(letrec ([e 5][! *]) (! e e))
(write "ourTest10:")
(startEval '(letrec ((x (- 9 5))(e (* 3 1))) (* e 5)))
(write "Expected")(letrec (( x(- 9 5))(e (* 3 1))) (* e 5))
(write "ourTest10:")
(startEval '(let ((x (+ 3 4))) (equal? x 7)))
(write "Expected")(let ((x (+ 3 4))) (equal? x 7))
(write "ourTest10:")
(startEval '(let ((x (+ 3 4))) (equal? x 7)))
(write "Expected")(let ((x (+ 3 4))) (equal? x 7))
(write "ourTest10:")
(startEval '(letrec ((y 5)(f (lambda (x) (+ x y)))) (f 4)))
(write "Expected")(letrec ((y 5)(f (lambda (x) (+ x y)))) (f 4))
|#
