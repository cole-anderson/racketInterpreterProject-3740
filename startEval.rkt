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
        ;TODO: CONSTANTS AND VARIABLES

        ;Checks for what operation is placed as the first element of entry
        ;ARITHMETIC OPERATORS:
        [(equal? "+" (symbol->string (car entry))) (+ (cadr entry) (caddr entry))];condition for addition
        [(equal? "-" (symbol->string (car entry))) (- (cadr entry) (caddr entry))];conditions for subtraction
        [(equal? "/" (symbol->string (car entry))) (/ (cadr entry) (caddr entry))];condition for division
        [(equal? "*" (symbol->string (car entry))) (* (cadr entry) (caddr entry))];condition for multiplication
        ;RELATIONAL OPERATORS
        [(equal? "=" (symbol->string (car entry))) (= (cadr entry) (caddr entry))]
        [(equal? "<=" (symbol->string (car entry))) (<= (cadr entry) (caddr entry))]
        [(equal? "<" (symbol->string (car entry))) (< (cadr entry) (caddr entry))]
        [(equal? ">=" (symbol->string (car entry))) (>= (cadr entry) (caddr entry))]
        [(equal? ">" (symbol->string (car entry))) (> (cadr entry) (caddr entry))]

        ;TODO: EQUAL? **
        [(equal? "equal?" (symbol->string (car entry))) (equal? (cadr entry) (caddr entry))]

        #|TODO: LISTS |#
        ;CAR, CDR, CONS, PAIR?
        [(equal? "car" (symbol->string (car entry))) (car (cadadr entry))]
        [(equal? "cdr" (symbol->string (car entry))) (cdr (cadadr entry))]
        [(equal? "pair?" (symbol->string (car entry))) (pair? (cdr entry))]
        [(equal? "quote" (symbol->string (car entry))) (write entry)]
        ;^move to list eval function for recursion^ - cole

        #|TODO: IF |#
        [(equal? "if" (symbol->string (car entry))) (ifEval entry)]


        #|TODO: LAMBDA |#
        ;SINGLE EXPRESSION LAMBDA

        #|TODO: FUNCTION APPLICATION |#
        ;APPLYING LAMBDA EXP TO ARGS

        #|TODO: LOCAL BINDING |#
        ;LET LETREC

        ;
        )


      ;(write "INVALID");condition
      (write entry)
      ))
;END MAIN

;HELPER FUNCTIONS:

;myEval
;(myEval (entry table))





;LIST EVAL FUNCTION
;(listEval entry)
(define (listEval entry)
  (cond
    [(equal? "car" (symbol->string (car entry)))
     (if (list? (cadadr entry))
         (write (car (cadadr entry)))
     (write "test"))]

    [(equal? "cdr" (symbol->string (car entry)))
     (if (list? (cadadr entry))
         (write (cdr (cadadr entry)))
     (write "test"))]

    ));gonna have to nest this different to incorporate nested loops better.

;IF EVAL FUNCTION
;(ifEval entry))
(define (ifEval entry)

  (if (cadr entry);i think this is a "hack" i feel like howard wants more
      (caddr entry)
      (cadddr entry)))
;Gotta ask howard if this is like "allowed"

#|
  ;Nest Read Test
  (write (car entry));if
  (write-char #\newline)
  (write (cadr entry));conditions
  (write-char #\newline)
  (write (caddr entry));if true
  (write-char #\newline)
  (write (cadddr entry)));if false
|#
