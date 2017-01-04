; Question 1
; a)
(display "Question 1a:\n")
(- (* 3
      4)
   (+ 1
      (+ 2
         (- 9
            (* 4
               2)))))
; b)
(display "Question 1b:\n")
(/ (* (* -1
         (+ (- 7
               2)
            5))
      2)
   (+ 1
      (* 2
         3)))

; Question 2
; a)

(define (testf n)
  (if (< n 4)
      n
      (f(- n 1))))

(define (f n)
  (if (< n 4)
      n
      (+ (* 1 (f(- n 1)))
         (* 2 (f(- n 2)))
         (* 3 (f(- n 3)))
         (* 4 (f(- n 4))))))
(display "Question 2 Test (f 5). Expected Value: 26\n")
(f 5)
; b)
(define (f-it n)
  (define (f-iter n1 n2 n3 n4 counter max)
    (if (> counter max)
      n1
      (f-iter (+ (- n1 1) (* 2 (- n2 2)) (* 3 (- n3 3)) (* 4 (- n4 4))) n1 n2 n3 (+ counter 1) max)))
  (if (< n 4)
      n
      (f-iter n n n n 0 n)))
      ;(f-iter (- n 1) (- n 2) (- n 3) (- n 4) 0 n)))

; Question 3
(define (pascal row column)
  (cond ((= column 0)
         1)
        ((= row 0)
         0)
        (else (+ (pascal (- row 1) column) (pascal (- row 1) (- column 1))))))
(display "Question 3 Test (pascal 4 2). Expected Value: 6\n")
(pascal 4 2)
(display "Question 3 Test (pascal 5 4). Expected Value: 10\n")
(pascal 5 3)

; Question 4
(define (sum term a next b)
  (define (sum2 term a next b tot)
    (if (> a b)
        tot
        (sum2 term (next a) next b (+ (term a) tot))))
  (sum2 term a next b 0))

; Test this using helper functions given by the assignment
(define (inc x) (+ x 1))
(define (dec x) (- x 1));
(define (identity x) x)
(define (sum-integers a b)
	(sum identity a inc b))

(display "Question 4 Test (sum-integers 0 5). Expected Value 0 + 1 + 2 + 3 + 4 + 5 = 15\n")
(sum-integers 0 5)
(display "Question 4 Test (sum identity 3 inc 7). Expected Value 3 + 4 + 5 + 6 + 7 = 25\n")
(sum identity 3 inc 7)

; Question 5
; a)
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))
(display "Question 5a Test (product identity 1 inc 5). Expected Value 1 * 2 * 3 * 4 * 5 * 1 = 120\n")
(product identity 1 inc 5)

; b)
(define (factorial n)
  (product identity 1 inc n))
(display "Question 5b Test (factorial 5). Expected Value 5! = 5 * 4 * 3 * 2 * 1 = 120\n")
(factorial 5)
(display "Question 5b Test (factorial 14). Expected Value 14! = 14 * 13 * ... * 2 * 1 = 87178291200\n")
(factorial 14)

; c)
(define (product-it term a next b)
  (define (product-it2 term a next b tot)
    (if (> a b)
        tot
        (product-it2 term (next a) next b (* (term a) tot))))
  (product-it2 term a next b 1))
(display "Question 5c Test (product-it identity 1 inc 5). Expected Value 1 * 2 * 3 * 4 * 5 = 120\n")
(product identity 1 inc 5)

; Question 6
; a)
;  When run using applicative order, the statement calling (test 0 (p)) would first evaluate p, because
;  it is in the innermost set of parentheses. This would cause an infinite loop of p calling itself over and over again.
; b)
;  When run using normal order, the call to (test 0 (p)) ignores that p is being invoked because it is running the call to the function first. The function determines that x is 0 and terminates before p can be invoked.
; c)
;  Scheme appears to run in applicative order, as the function p is called before test can be invoked. This causes Scheme to hang.

; Question 7
; This definition is based almost entirely on the sqrt solution from class.
; I've commented my changes as most of this was written by the professor.
(define (cubert x)
  (define (square x) ( * x x))
  ; Added cube function
  (define (cube x) (* x x x))
  
  (define (good-enough? guess x)
    ; Changed (square guess) to (cube guess)
    (< (abs (- (cube guess) x)) 0.001))

  (define (improve guess x)
    ; Left out average function and calculated guess for cube in (improve guess x)
    ; Uses (x/y^2+2y)/3 equation for next guess
    (/  (+ ( / x
             (square guess))
           (* guess 2))
        3))
  
  (define (cube-iteration guess x)
    (if (good-enough? guess x)
        guess
        (cube-iteration (improve guess x) x)))
  
  (cube-iteration 1.0 x))

; Question 9
(define (new-if predicate consequent alternate)
   (cond (predicate consequent)
         (else alternate)))

(define (my-sqrt x goodEnoughFunc max)
  ; User is to define goodEnoughFunc.
  (define (square x) ( * x x))
	
  (define (average x y)
    (/ (+ x y) 2))
	
  (define (improve guess x)
    (average guess ( / x guess)))
	
  (define (sqrt-iteration guess x maxIt)
    (cond ((= maxIt 0) guess) ; Replaced weird if structure with cond, much cleaner
          ((goodEnoughFunc guess x) ; a) User-defined good enough
           guess)
          (else (sqrt-iteration (improve guess x) x (- maxIt 1)))))

  (sqrt-iteration 1.0 x max))

; good-enough functions for testing the above my-sqrt
(define (good-enough1 guess x)
  (< (abs (- (* guess guess) x)) 0.00001))

(define (good-enough2 guess x)
  (< (abs (- (* guess guess) x)) 0.1))

(define (good-enough3 guess x)
  (< (abs (- (* guess guess) x)) 100))
