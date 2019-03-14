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

;Terms used:
;-> entry(user input)
; 



(define (startEval entry)
  (if (list? entry) ;checks for valid input into program ie: (startEval '(entry))
      
      (cond
        ;Checks for what operation is placed as the first element of input
        ;ARITHMETIC OPERATORS:
        [(equal? "+" (symbol->string (car entry))) (write (+ (cadr entry) (caddr entry)))];condition for addition
        [(equal? "-" (symbol->string (car entry))) (write (- (cadr entry) (caddr entry)))];conditions for subtraction
        [(equal? "/" (symbol->string (car entry))) (write (/ (cadr entry) (caddr entry)))];condition for division
        [(equal? "*" (symbol->string (car entry))) (write (* (cadr entry) (caddr entry)))];condition for multiplication
        ;RELATIONAL OPERATORS
        [(equal? "=" (symbol->string (car entry))) (write (= (cadr entry) (caddr entry)))]
        [(equal? "<=" (symbol->string (car entry))) (write (<= (cadr entry) (caddr entry)))]
        [(equal? "<" (symbol->string (car entry))) (write (< (cadr entry) (caddr entry)))]
        [(equal? ">=" (symbol->string (car entry))) (write (>= (cadr entry) (caddr entry)))]
        [(equal? ">" (symbol->string (car entry))) (write (> (cadr entry) (caddr entry)))]

        ;TODO: CONSTANTS AND VARIABLES
        ;TODO: EQUAL? **
        
        #|TODO: LISTS |#
        ;CAR, CDR, CONS, PAIR?
        [(equal? "car" (symbol->string (car entry))) (listEval entry)]
        [(equal? "cdr" (symbol->string (car entry))) (listEval entry)]
        

        #|TODO: IF |#

        #|TODO: LAMBDA |#
        ;SINGLE EXPRESSION LAMBDA

        #|TODO: FUNCTION APPLICATION |#
        ;APPLYING LAMBDA EXP TO ARGS
        
        #|TODO: LOCAL BINDING |#
        ;LET LETREC

        ;
        )
        
      
      (write "INVALID");condition
      ))

;LIST EVAL FUNCTION
(define (listEval entry)
  (cond
    [(equal? "car" (symbol->string (car entry)))
     (if (list? (cadadr entry))
         (write (car (cadadr entry)))
     (write "test"))]
    ;doesnt work properly lol
    [(equal? "cdr" (symbol->string (car entry)))
     (if (list? (cadadr entry))
         (write (cdr (cadadr entry)))
     (write "test"))]

    ));gonna have to nest this different to incorporate nested loops better. get test cases from nicole