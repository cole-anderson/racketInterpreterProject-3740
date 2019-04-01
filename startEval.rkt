
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
     ; (and (and (and (and (write entry2) (writeln stack)) (write-char #\newline))) (let ([operator (known2? (car entry2) stack)]) (let ([entry (append (list operator) (cdr entry2))])
      (let ([operator (known2? (car entry2) stack)]) (let ([entry (append (list operator) (cdr entry2))])
      (cond
        ;Checks for what operation is placed as the first element of entry
        ;and determines what the proper operation to execute

        ;1)CONSTANTS AND VARIABLES: //COMPLETE
        [(number? operator) operator]
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
        ;[(equal? 'car operator) (car (cadadr entry))]
        ;[(equal? 'cdr operator) (cdr (cadadr entry))]
        ;[(equal? 'pair? operator) (pair? (cadr entry))]

        [(equal? 'car operator) (car (known2? (cdr entry) stack))]
        [(equal? 'cdr operator) (cdr (known2? (cdr entry) stack))]
        [(equal? 'pair? operator) (pair? (known2? (cdr entry) stack))]

        
        [(equal? 'cons operator) (cons (known? (cadr entry) stack) (SecondEval (caddr entry) stack))]
        
        ;5) CONDITIONAL: IF //COMPLETE
        [(equal? 'if operator) (ifEval entry stack)]


        #|TODO: LAMBDA |#
        ;6)LAMBDA EXPRESSION: SINGLE EXPRESSION LAMBDA
        [(equal? 'lambda operator) (if (find (cadr entry) stack)
                                       (SecondEval(caddr entry) stack)
                                       (write 'function))] ; assumes lambda will be called only as itself or
                                                           ; that it is a function call if formal params are in stack
        
        #|TODO: FUNCTION APPLICATION |#
        ;7)FUNCTION APPLICATION: APPLYING LAMBDA EXP TO ARGS

        #|TODO: LOCAL BINDING |#
        ;8)LOCAL BINDING: LET LETREC
	[(equal? 'let operator) (SecondEval (caddr entry)  (append (cadr entry) stack))]
        ;letrec;
        [(equal? 'letrec operator) (SecondEval (caddr entry) (append (cadr entry) stack))];(list (list (caaadr entry) 'UNDEF) (caadr entry)) stack))]

        ;[(equal? 'write operator) (write  (SecondEval (cdr entry) stack))]
        [(list? operator) (if (pair? (caar entry))
                              (SecondEval (list (caddaar entry) (cadar entry)) (process (cadaar entry) (cdr entry) stack))
                              (SecondEval operator (process (cadar entry) (cdr entry) stack)))]
                               ; cadar entry = formal parameters
                               ; cdr entry = list of actual parameters
        ;[(list? entry) (SecondEval operator (process (cadr (knownfunc? operator stack)) (cdr entry) stack))]
        [(list? entry) (known? operator stack)]
        
        
      ))) ;)))
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
     (append (list (list (car param) (SecondEval (car act) stack))) (append (process3 (cdr param) (cdr act) '()) stack))))

(define (process3 param act stack)
  (if (or (null? param) (null? act))
      stack
     (append (append (list (list (car param) (SecondEval (car act) stack))) (process3 (cdr param) (cdr act) stack)) stack)))

(define (Evaluate List)
  (if (null? List)
      List
      (append (list (list (caar List) (SecondEval (cdar List) List))) (Evaluate (cdr List) List))))

; (Evaluate '((x 1) (y (- z 2))) '((z 9))) => '((x 1) (y 7) (z 9))
  


(define (carcdrEval entry stack)
(write "a")
  
   
  )

(define (caddaar func) (car (cddaar func)))

(define (find elem stack)
  (if (number? elem)
      #t
      (if (null? stack)
          #f
      (if (equal? elem (caar stack))
          #t
          (known? elem (cdr stack))))))


;KNOWN? FUNCTION
(define (known2? elem stack)
  (if (number? elem)
      elem
      (if (null? stack)
          elem
          (if (equal? elem (caar stack))
              (cadar stack)
              (known2? elem (cdr stack))))))

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
(define (ifEval entry stack)
  (if (SecondEval (cadr entry) stack) ;calls starteval to evaluate function
      (SecondEval (caddr entry) stack) ;does this entry if true
      (SecondEval (cadddr entry) stack))) ;does this entry if false

;EQUAL EVAL FUNCTION
;(eqEval entry)
(define (eqEval entry stack)
  (equal? (SecondEval (car entry) stack) (SecondEval (cadr entry) stack)))


;END PROGRAM


;Success: A B C D F G
;Fail: E H

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
(write "Expected")(let ([x (car '(1 4 5))][y (car '(4 5))]) (+ x y))
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
(startEval '(let ([e 5][! *]) (! e e)))
(write "Expected")(let ([e 5][! *]) (! e e))

#|
(write "Test BROKEN ONE THAT CAN GO TO HELL")
(startEval
  '(letrec ((intersect
             (lambda (s t) 
               (if (equal? s (quote ()))
                 (quote ())
                 (if (member (car s) t)
                   (cons (car s) (intersect (cdr s) t))
                   (intersect (cdr s) t)
                 )
               )
              ))
             (member
	      (lambda (x s)
                 (if (equal? s (quote ()))
                   (quote #f)
                   (if (equal? x (car s))
                     (quote #t)
                     (member x (cdr s))
                   )
	         )
              )
	     ))
           (intersect (quote (a b c d)) (quote (b c d e f)))
    )
 )
|#











;-------------------------------------------------
;DELETE ALL AFTER THIS:

;;;;;copy paste stuff
;(write-char #\newline)
;(startEval '(let ([x 5]) x))



;(startEval '(let ([y 5][x 3]) (let ([x 7]) (+ x y))))
;(startEval '(lambda (x y) (+ x y)))
;(startEval '(let ([x (car '(1 4 5))][y (car '(4 5))]) (+ x y)))
;(startEval '(let ((inc(lambda (x) (+ x (quote 1)))))(inc (quote 5))))
;(startEval '(((lambda (x) (lambda (y) (+ x y))) 1) 2))
;(startEval '(letrec ((fact (lambda (x) (if (= x 0) (quote 1) (* x (fact (- x 1))))))) (fact 10)))

;(startEval '(+ (quote 5) (quote 3)))
;(startEval '(let ([+ *]) (+ 3 4)))
;(SecondEval '(letrec ((fact (lambda (x) (if (= x 0) (quote 1) (* 3 (- x 1)))))) (fact 3)) '((* +)))
;(startEval '(let ([e 5][! *]) (! e e)))


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
