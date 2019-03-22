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
<<<<<<< HEAD
        ;TODO: CONSTANTS AND VARIABLES

=======
>>>>>>> refs/remotes/origin/master
        ;Checks for what operation is placed as the first element of entry
        ;and determines what the proper operation to execute

        ;1)CONSTANTS AND VARIABLES: //90% : NEED: VARIABLES
        [(number? (car entry)) (write entry)]
        ;if variable see end of main
        [(equal? "quote" (symbol->string (car entry))) (write entry)]
        
        ;2)ARITHMETIC OPERATORS: //COMPLETE
        [(equal? "+" (symbol->string (car entry))) (+ (cadr entry) (caddr entry))];condition for addition
        [(equal? "-" (symbol->string (car entry))) (- (cadr entry) (caddr entry))];conditions for subtraction
        [(equal? "/" (symbol->string (car entry))) (/ (cadr entry) (caddr entry))];condition for division
        [(equal? "*" (symbol->string (car entry))) (* (cadr entry) (caddr entry))];condition for multiplication
        ;3)RELATIONAL OPERATORS //90%
        [(equal? "=" (symbol->string (car entry))) (= (cadr entry) (caddr entry))]
        [(equal? "<=" (symbol->string (car entry))) (<= (cadr entry) (caddr entry))]
        [(equal? "<" (symbol->string (car entry))) (< (cadr entry) (caddr entry))]
        [(equal? ">=" (symbol->string (car entry))) (>= (cadr entry) (caddr entry))]
        [(equal? ">" (symbol->string (car entry))) (> (cadr entry) (caddr entry))]
        ;TODO: EQUAL? **NEED TO SUPPORT MULT VALUES
        [(equal? "equal?" (symbol->string (car entry))) (equal? (cadr entry) (caddr entry))]

        #|TODO: LISTS |#
        ;4)LISTS: CAR, CDR, CONS, PAIR?
        [(equal? "car" (symbol->string (car entry))) (car (cadadr entry))]
        [(equal? "cdr" (symbol->string (car entry))) (cdr (cadadr entry))]
        ;finish pair? cons?
        [(equal? "pair?" (symbol->string (car entry))) (pair? entry)]
        ;cadr 1 caddr 2
                                                                   
        ;[(equal? "cons?" (symbol->string (car entry))) (cons?  entry)]
        ;^move to list eval function for recursion^ - cole ** :) **

        
        #|TODO: IF |#
        ;5) CONDITIONAL: IF
        [(equal? "if" (symbol->string (car entry))) (ifEval entry)]


        #|TODO: LAMBDA |#
        ;6)LAMBDA EXPRESSION: SINGLE EXPRESSION LAMBDA

        #|TODO: FUNCTION APPLICATION |#
<<<<<<< HEAD
        ;APPLYING LAMBDA EXP TO ARGS
=======
        ;7)FUNCTION APPLICATION: APPLYING LAMBDA EXP TO ARGS
>>>>>>> refs/remotes/origin/master

        #|TODO: LOCAL BINDING |#
        ;8)LOCAL BINDING: LET LETREC

<<<<<<< HEAD
        ;
        )


      ;(write "INVALID");condition
      (write entry)
      ))
=======
   
        
        
        )

             


      ;if input is not in form: startEval '(your input here)
      (and(and (write "invalid input")(write-char #\newline))(write "Try: startEval '(your input here)")
      )))
>>>>>>> refs/remotes/origin/master
;END MAIN


#|HELPER FUNCTIONS|#

;myEval
;(myEval (entry table))





;LIST EVAL FUNCTION
;(listEval entry)
#|
(define (listEval entry)
  (cond
    [(equal? "car" (symbol->string (car entry)))
     (if (list? (cadadr entry)) ; recursively call listEval if second element list
         (write (car (cadadr entry)))
     (write "test"))]

    [(equal? "cdr" (symbol->string (car entry)))
     (if (list? (cadadr entry))
         (write (cdr (cadadr entry)))
     (write "test"))]

    ));gonna have to nest this different to incorporate nested loops better.
|#
;;;;;;;;;;;;;;;;;;
(define (listEval entry)
  (cond
    [(equal? "car" (symbol->string (car entry)))
     (if (list? (cadadr entry)) ; recursively call listEval if second element list
         (write (car (cadadr entry)))
     (write "test"))]

    [(equal? "cdr" (symbol->string (car entry)))
     (if (list? (cadadr entry))
         (write (cdr (cadadr entry)))
     (write "test"))]
))  

;;;;;;;;;;;;;;;;;;




;IF EVAL FUNCTION
;(ifEval entry))
(define (ifEval entry)
<<<<<<< HEAD
  (if (cadr entry);i think this is a "hack" i feel like howard wants more
      (caddr entry)
      (cadddr entry)))
;Gotta ask howard if this is like "allowed"
;gotta check if list and if function
;if function call starteval and if not just calculate
=======
  (write (cdr entry)))
>>>>>>> refs/remotes/origin/master



;;GARBAGE TEST INFORMATION:
  ;(if (cadr entry);i think this is a "hack" i feel like howard wants more
      ;(caddr entry)
      ;(cadddr entry)))
;Gotta ask howard if this is like "allowed"
;Test Text Code:
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
<<<<<<< HEAD
=======


;END PROGRAM
>>>>>>> refs/remotes/origin/master
