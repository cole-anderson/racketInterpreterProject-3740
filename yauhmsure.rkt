
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
  (SecondEval entry '())
  )

(define (SecondEval entry stack)
  (if (list? entry) ;checks for valid input into program ie: (startEval '(entry))
      (cond
        ;Checks for what operation is placed as the first element of entry
        ;and determines what the proper operation to execute

        ;1)CONSTANTS AND VARIABLES: //90% : is 'x fine?
        [(number? (car entry)) (entry)]
        [(equal? "quote" (symbol->string (car entry))) (write entry)]
        
        ;2)ARITHMETIC OPERATORS: //COMPLETE
        [(equal? "+" (symbol->string (car entry))) (+ (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? "-" (symbol->string (car entry))) (- (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? "/" (symbol->string (car entry))) (/ (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? "*" (symbol->string (car entry))) (* (known? (cadr entry) stack) (known? (caddr entry) stack))]
        
        ;3)RELATIONAL OPERATORS //COMPLETE
        [(equal? "=" (symbol->string (car entry))) (= (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? "<=" (symbol->string (car entry))) (<= (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? "<" (symbol->string (car entry))) (< (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? ">=" (symbol->string (car entry))) (>= (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? ">" (symbol->string (car entry))) (> (known? (cadr entry) stack) (known? (caddr entry) stack))]
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
        [(equal? "let" (symbol->string (car entry))) (letEval (cdr entry))]

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
;(caar '((x 5) (y 3) (z 2))) = 'x
;(cdr '((x 5) (y 3) (z 2))) = '((y 3) (z 2))
;(cadar '((x 5) (y 3) (z 2))) = 5
(define (known? elem stack)
  (if (number? elem)
      elem
      (if (equal? elem (caar stack))
          (cadar stack)
          (known? elem (cdr stack)))))

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
(define (letEval entry)
  #|(write (caadr entry)) ;(y 5)
  (write-char #\newline)
  (write (caaadr entry)) ;y
  (write-char #\newline)
  (write (car (cdaadr entry))) ;5
  (write-char #\newline)
  (write (caddr entry)) ;x
  |#

<<<<<<< HEAD:yauhmsure.rkt
  ;(let ([me "Bob"]) me)
  (let ([{caaadr entry} 1])
    {caaadr entry})
=======
(myEval (cdr entry)  (Stacks (car entry)))
  
  ;(letrec ([(nestEval (caaadr entry)) 1]) (nestEval caaadr entry))
 
  ;(nestEval (caaadr entry))
>>>>>>> e6f9146aa334fdb221f74ebe966cedc7b88c0a5a:startEval.rkt
 
  )
;(startEval '(let ([y 5]) x))

<<<<<<< HEAD:yauhmsure.rkt
=======
(define (myEval func stack)
  stack
  )

(define (Stacks entries)
  (cdr entries)
  )

;nestEval
(define (nestEval nest)
  nest)

>>>>>>> e6f9146aa334fdb221f74ebe966cedc7b88c0a5a:startEval.rkt

;END PROGRAM

;;;;;copy paste stuff
;(write-char #\newline)
;(startEval '(let ([x 5]) x))






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