
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

        ;1)CONSTANTS AND VARIABLES: //COMPLETE
        [(number? (car entry)) (entry)]
        [(equal? 'quote (car entry)) (write entry)]
        
        ;2)ARITHMETIC OPERATORS: //COMPLETE
        [(equal? '+ (car entry)) (+ (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? '- (car entry)) (- (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? '/ (car entry)) (/ (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? '* (car entry)) (* (known? (cadr entry) stack) (known? (caddr entry) stack))]
        
        ;3)RELATIONAL OPERATORS //COMPLETE
        [(equal? '= (car entry)) (= (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? '<= (car entry)) (<= (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? '< (car entry)) (< (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? '>= (car entry)) (>= (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? '> (car entry)) (> (known? (cadr entry) stack) (known? (caddr entry) stack))]
        [(equal? 'equal? (car entry)) (eqEval (cdr entry))]
        
        ;4)LISTS: CAR, CDR, CONS, PAIR? //COMPLETE
        [(equal? 'car (car entry)) (car (cadadr entry))]
        [(equal? 'cdr (car entry)) (cdr (cadadr entry))]
        [(equal? 'pair? (car entry)) (pair? (cadr entry))]                                                                   
        [(equal? 'cons (car entry)) (cons (known? (cadr entry) stack) (known? (caddr entry) stack))]
        
        ;5) CONDITIONAL: IF //COMPLETE
        [(equal? 'if (car entry)) (ifEval entry)]


        #|TODO: LAMBDA |#
        ;6)LAMBDA EXPRESSION: SINGLE EXPRESSION LAMBDA
        [(equal? 'lambda (caar entry)) (lamEval (cadar entry) (caddar entry) (cdr entry) stack)]
        
        #|TODO: FUNCTION APPLICATION |#
        ;7)FUNCTION APPLICATION: APPLYING LAMBDA EXP TO ARGS

        #|TODO: LOCAL BINDING |#
        ;8)LOCAL BINDING: LET LETREC
	[(equal? 'let (car entry)) (SecondEval (caddr entry)  (append (cadr entry) stack))] ;(letEval (cdr entry))]
        ;letrec;
        
        
      )
      ;else
      (known? entry stack)
      ;if input is not in form: startEval '(your input here)
      ;(and(and (write "invalid input")(write-char #\newline))(write "Try: startEval '(your input here)"))
      ))
;END MAIN


#|HELPER FUNCTIONS|#
;(caar '((x 5) (y 3) (z 2))) = 'x
;(cdr '((x 5) (y 3) (z 2))) = '((y 3) (z 2))
;(cadar '((x 5) (y 3) (z 2))) = 5


(define (lamEval entry1 entry2 parama stack)
  #|
(startEval '((lambda (x y)
                 (- x y)) 1 2))
|#
  
  ;(write (car entry)) ;(x y)
  ;(write-char #\newline)
  ;(write (cadr entry)) ;(- x y)
  ;(write-char #\newline)
  ;(write param) ;(1 2)
(write entry1)
  (write-char #\newline)
  (write entry2)
  (write-char #\newline)
  (write parama)
  ;((lambda (x y) entry2) 1 2);(car parama) (cdr parama))
  ((lambda (x y) (- x y)) 1 2)
  

  
  
  )


;KNOWN? FUNCTION
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


;END PROGRAM

;;;;;copy paste stuff
;(write-char #\newline)
;(startEval '(let ([x 5]) x))



;(startEval '(let ([y 5]) x))


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
