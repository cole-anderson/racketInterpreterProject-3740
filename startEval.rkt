
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
  (if (list? entry) ;checks for valid input into program ie: (startEval '(entry))
      (cond
        ;Checks for what operation is placed as the first element of entry
        ;and determines what the proper operation to execute

        ;1)CONSTANTS AND VARIABLES: //90% : is 'x fine?
        [(number? (car entry)) (entry)]
        [(equal? "quote" (symbol->string (car entry))) (write entry)]
        
        ;2)ARITHMETIC OPERATORS: //COMPLETE
        [(equal? "+" (symbol->string (car entry))) (+ (cadr entry) (caddr entry))]
        [(equal? "-" (symbol->string (car entry))) (- (cadr entry) (caddr entry))]
        [(equal? "/" (symbol->string (car entry))) (/ (cadr entry) (caddr entry))]
        [(equal? "*" (symbol->string (car entry))) (* (cadr entry) (caddr entry))]
        
        ;3)RELATIONAL OPERATORS //COMPLETE
        [(equal? "=" (symbol->string (car entry))) (= (cadr entry) (caddr entry))]
        [(equal? "<=" (symbol->string (car entry))) (<= (cadr entry) (caddr entry))]
        [(equal? "<" (symbol->string (car entry))) (< (cadr entry) (caddr entry))]
        [(equal? ">=" (symbol->string (car entry))) (>= (cadr entry) (caddr entry))]
        [(equal? ">" (symbol->string (car entry))) (> (cadr entry) (caddr entry))]
        ;**nicole is this right lmao -cole
        [(equal? "equal?" (symbol->string (car entry))) (eqEval (cdr entry))]
        
        ;4)LISTS: CAR, CDR, CONS, PAIR? //COMPLETE
        [(equal? "car" (symbol->string (car entry))) (car (cadadr entry))]
        [(equal? "cdr" (symbol->string (car entry))) (cdr (cadadr entry))]
        [(equal? "pair?" (symbol->string (car entry))) (pair? (cadr entry))]                                                                   
        [(equal? "cons?" (symbol->string (car entry))) (cons?  entry)]
        ;**should i move this into a nested function nicole? -cole 

        
        ;5) CONDITIONAL: IF //COMPLETE
        [(equal? "if" (symbol->string (car entry))) (ifEval entry)]


        #|TODO: LAMBDA |#
        ;6)LAMBDA EXPRESSION: SINGLE EXPRESSION LAMBDA

        #|TODO: FUNCTION APPLICATION |#
        ;7)FUNCTION APPLICATION: APPLYING LAMBDA EXP TO ARGS

        #|TODO: LOCAL BINDING |#
        ;8)LOCAL BINDING: LET LETREC

   
        
        
      )
      ;else
      entry
      ;if input is not in form: startEval '(your input here)
      ;(and(and (write "invalid input")(write-char #\newline))(write "Try: startEval '(your input here)"))
      ))
;END MAIN


#|HELPER FUNCTIONS|#

;IF EVAL FUNCTION
;(ifEval entry))
(define (ifEval entry)
  (if (startEval(cadr entry));calls starteval to evaluate function
      (startEval (caddr entry));does this entry if true
      (startEval (cadddr entry))));does this entry if false

;EQUAL EVAL FUNCTION
;(eqEval entry)
(define (eqEval entry)
  (equal? (startEval (car entry)) (startEval (cadr entry))))

  
;myEval
;(myEval (entry table))



;END PROGRAM


;;GARBAGE TEST INFORMATION:
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
)) 
|#
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