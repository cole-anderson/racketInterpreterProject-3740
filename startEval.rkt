
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

(define (SecondEval entry2 stack)
  (if (list? entry2);checks for valid input into program ie: (startEval '(entry))
      (and (and (and (and (write entry2) (write stack)) (write-char #\newline))) (let ([operator (known2? (car entry2) stack)]) (let ([entry (append (list operator) (cdr entry2))])
      (cond
        ;Checks for what operation is placed as the first element of entry
        ;and determines what the proper operation to execute

        ;1)CONSTANTS AND VARIABLES: //COMPLETE
        [(number? operator) (entry)]
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
        [(equal? 'equal? operator) (eqEval (cdr entry))]
        
        ;4)LISTS: CAR, CDR, CONS, PAIR? //COMPLETE
        [(equal? 'car operator) (car (cadadr entry))]
        [(equal? 'cdr operator) (cdr (cadadr entry))]
        [(equal? 'pair? operator) (pair? (cadr entry))]                                                                   
        [(equal? 'cons operator) (cons (known? (cadr entry) stack) (SecondEval (caddr entry) stack))]
        
        ;5) CONDITIONAL: IF //COMPLETE
        [(equal? 'if operator) (ifEval entry)]


        #|TODO: LAMBDA |#
        ;6)LAMBDA EXPRESSION: SINGLE EXPRESSION LAMBDA
        [(equal? 'lambda operator) (SecondEval(caddr entry) stack)]
        ;[(equal? 'lambda (caar entry)) (lamEval (cadar entry) (caddar entry) (cdr entry) stack)]
        
        #|TODO: FUNCTION APPLICATION |#
        ;7)FUNCTION APPLICATION: APPLYING LAMBDA EXP TO ARGS

        #|TODO: LOCAL BINDING |#
        ;8)LOCAL BINDING: LET LETREC
	[(equal? 'let operator) (SecondEval (caddr entry)  (append (cadr entry) stack))]; (write (append (cadr entry) stack)))] ;(letEval (cdr entry))]
        ;letrec;

        ;[(equal? 'write operator) (write  (SecondEval (cdr entry) stack))]
        [(list? operator) (SecondEval operator (process (cadar entry) (cdr entry) stack))]
        [(list? entry) (SecondEval operator (process (cadr (knownfunc? operator stack)) (cdr entry) stack))]
        
        
      ))))
      ;else
      ;entry
      (known? entry2 stack)
      ;if input is not in form: startEval '(your input here)
      ;(and(and (write "invalid input")(write-char #\newline))(write "Try: startEval '(your input here)"))
      ))
;END MAIN


#|HELPER FUNCTIONS|#
;(caar '((x 5) (y 3) (z 2))) = 'x
;(cdr '((x 5) (y 3) (z 2))) = '((y 3) (z 2))
;(cadar '((x 5) (y 3) (z 2))) = 5

(define (process param act stack)
  (if (or (null? param) (null? act))
      stack
     (append (process (cdr param) (cdr act) stack) (append (list (list (car param) (car act))) stack))))
  


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
(define (known2? elem stack)
  (if (number? elem)
      elem
      (if (null? stack)
          elem
      (if (equal? elem (caar stack))
          (cadar stack)
          (known? elem (cdr stack))))))

(define (known? elem stack)
  (if (number? elem)
      elem
      (if (null? stack)
          elem
          (if (equal? elem (caar stack))
              (if (pair? (cadar stack)) 
                  ;(write (cadar stack));
                  (SecondEval (cadar stack) stack)
                  (cadar stack))
              (known? elem (cdr stack))))))

(define (knownfunc? elem stack)
  (if (number? elem)
      elem
      (if (equal? elem (caar stack))
          (cadar stack)
          (knownfunc? elem (cdr stack)))))

(define (known1? elem stack)
  (if (number? elem)
      elem
      (if (equal? elem (car stack))
          (if (pair? (cadr stack)) 
              ;(write (cadar stack));
              (SecondEval (cadr stack) stack)
              (cadr stack))
          (known? elem (cddr stack)))))

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



;(startEval '(let ([y 5][x 3]) (let ([x 7]) (+ x y))))
;(startEval '(lambda (x y) (+ x y)))
;(startEval '(let ([x (car '(1 4 5))][y (car '(4 5))]) (+ x y)))
; (startEval'(let ((inc(lambda (x) (+ x (quote 1)))))(inc (quote 5))))
;(startEval '(((lambda (x) (lambda (y) (+ x y))) 1) 2))

;(startEval '(+ (quote 5) (quote 3)))
;(startEval '(let ([+ *]) (+ 3 4)))


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
