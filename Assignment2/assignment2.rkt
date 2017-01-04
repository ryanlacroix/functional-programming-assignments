; Ryan Lacroix, 100901696
; Assignment 2

(#%require (only racket/base random))

; Question 1
(define (make-interval a b)
  (cons a b))
(define (upper interval)
  (cdr interval))
(define (lower interval)
  (car interval))

(define (add-interval i1 i2)
  (cons (+ (car i1) (car i2)) (+ (cdr i1) (cdr i2))))

(define (subtract-interval i1 i2)
  (cons (- (car i1) (cdr i2)) (- (cdr i1) (car i2))))

(define (multiply-interval i1 i2)
  (cons (min (*(car i1) (car i2))
              (*(car i1) (cdr i2))
              (*(cdr i1) (car i2))
              (*(cdr i1) (cdr i2)))
        (max (*(car i1) (car i2))
              (*(car i1) (cdr i2))
              (*(cdr i1) (car i2))
              (*(cdr i1) (cdr i2)))))

(define (divide-interval i1 i2)
  (cond ((= (car i2) 0) (display "error"))
        ((= (cdr i2) 0) (display "error"))
        (else (multiply-interval i1
                         (make-interval (/ 1 (car i2)) (/ 1 (cdr i2)))))))

; Question 2
(define (forEach lis)
  (define inner (lambda (op) (inner2 lis op)))
  (define (inner2 li op)
    (if (null? li)
        '()
        (cons (op (car li)) (inner2 (cdr li) op))))
  inner)

; Question 3
; a)
(define (special-cons x y)
    (lambda (m) (m x y)))

(define (special-car x)
  (car (x cons)))

(define (special-cdr x)
  (cdr (x cons)))

; b)
(define (triple x y z)
  (lambda (val) (cond ((= val 1)x)
                      ((= val 2)y)
                      ((= val 3)z))))

(define (triple-first trips) ; Renamed for namespace collision
  (trips 1))
(define (second trips)
  (trips 2))
(define (third trips)
  (trips 3))


; Question 4
; Including the 'accumulate' definition from class:
(define (accumulate operator initial sequence)
    (if (null? sequence)
        initial
        (operator (car sequence) ; element
		          (accumulate operator initial (cdr sequence))))) ;rest


(define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence)) 
		       (cons (car sequence) 
                     (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

; a)
(define (my-map proc sequence)
 (accumulate (lambda (x y) (cons (proc x) y)) '() sequence))
;(define a (my-map (lambda (x) (* x x)) (list 1 2 3 4 5))) example use

; b)
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
;(define a (my-append (list 1 2 3 4 5) (list 7 8 9))) example use

; c)
(define (my-length sequence)
  (accumulate (lambda (x y) (+ (+ 1 (- x x) ) y)) 0 sequence))

; d)
(define (my-filter predicate sequence)
  (accumulate (lambda (element rest)
                (cond ((predicate element)(cons element rest))
                                          (else rest)) ) '() sequence))
; Question 5
; a)   NOTE: I *think* this counts as tail-recursive.
;            
(define (intersection-set-iter set1 set2)
  (define (innerIntersect set1 set2 origSet2 intersect)
    (if (null? set1)
        intersect
        (if (null? set2)
            (innerIntersect (cdr set1) origSet2 origSet2 intersect)
            (if (= (car set1) (car set2))
                (innerIntersect  set1 (cdr set2) origSet2 (cons (car set1) intersect))
                (innerIntersect set1 (cdr set2) origSet2 intersect)))))
  (innerIntersect set1 set2 set2 '()))

; b) Using Filter
; Including the 'filter' definition from class
(define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence)) 
		       (cons (car sequence) 
                     (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(define (intersection-set set1 set2)
  (define (inner set1 set2 intersect)
    (if (null? set1)
        (filter (lambda (x) (not (null? x))) (flattenList intersect))
          (inner (cdr set1) set2 (cons (filter (lambda(x)(= (car set1) x)) set2) intersect))))     
  (inner set1 set2 '()))
;(intersection-set '(1 2 3 4) '(3 4 5 1 7))   Note: leaves in empty set and 2D

; c)
 (define (mean lis)
   (define (mean-inner li total count)
     (cond ((null? li)(if (= count 0)
                          (display "Error: no numbers in list")
                          (/ total count)))
           ((integer? (car li))(mean-inner (cdr li) (+ total (car li)) (+ count 1)))
           (else (mean-inner (cdr li) total count))))
   (mean-inner lis 0 0))
; (mean '(10 d b 15 20 h))

; d)
(define (subsets x)
  (define (inner-subset set1 set2 superSet)
    (cond ((null? set1) '())
          ((null? set2) (inner-subset (cdr set1) x superSet)) ;maybe add origSet2 to cdr through
          (else (cons (cons (car set1) (car set2)) (inner-subset set1 (cdr set2) superSet )))))

  (inner-subset x x '()))

; Question 6
; a)
(define (depth lis)
  (define (inner-depth li currDepth maxDepth)
    (if (null? li)
        (+ maxDepth 1)
        (if (list? li)
            (if (list? (car li))
                (inner-depth (car li) (+ 1 currDepth) maxDepth)
                (inner-depth (cdr li) currDepth (max currDepth maxDepth)))
            0)))
  (inner-depth lis 0 0))

; b)
(define (treemap procedure lis)
  (if (null? lis)
      '()
      (if (list? lis)
          (cons (treemap procedure (car lis))(treemap procedure (cdr lis)))
           (procedure lis))))

;(treemap sqr '((1) 2 3 ((4 5 6) 7) 8 9))
(define (sqr x) ; for testing
  (* x x))

; c)
(define (flattenList lis)
  (if (null? lis)
      '()
      (if (list? lis)
          (append (flattenList (car lis))(flattenList (cdr lis)))
           (list lis))))
;(flattenList '(1 (2 3) ((4 5 6 (7)))(((8 (9))))))

; Question 7
; Definitions from lectures property of Andrew Runka
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)(cons a (delay b)))))
(define (stream-car s)(car s))
(define (stream-cdr s)(force (cdr s)))
(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))
(define (stream-filter predicate stream)
    (cond ((stream-null? stream) 
              the-empty-stream)
          ((predicate (stream-car stream))
              (cons-stream (stream-car stream)
                           (stream-filter predicate (stream-cdr stream))))
          (else (stream-filter predicate (stream-cdr stream)))))
  
(define (stream-null? stream )
    (null? stream))
(define integers
  (integers-starting-from 1))

; a)
; i)
(define (first n str)
  (if (= n 0)
      '()
      (cons-stream (stream-car str) (first (- n 1) (stream-cdr str)))))

; ii)
(define (list->stream lis)
  (if (null? lis)
      '()
      (cons-stream (car lis) (list->stream (cdr lis)))))
;(define strFromList (list->stream '(1 2 3 4 5 6)))

; iii)
(define (stream->list str)
  (if (null? str)
      '()
      (cons (stream-car str) (stream->list (stream-cdr str)))))
;(define 4ints (first 4 integers))
;(define listFromStr (stream->list 4ints))

; b)
; i)
(define stream-of-ones
  (cons-stream 1 stream-of-ones))

; ii)
(define stream-of-odds
  (stream-filter odd? integers))

; iii)
(define (stream-of-randoms)
  (cons-stream (random 100) (stream-of-randoms)))

; iv)
(define (iFun n)
  (if (< n 4)
      n
      (+ (iFun(- n 1)) (* 2 (iFun (- n 2))) (* 3 (iFun (- n 3))))))
(define (stream-of-function)
  (define (inner-sof n)
    (cons-stream (iFun n) (inner-sof (+ n 1))))
  (inner-sof 1))

; c)
(define (partial-sums pos-ints)
  (define (inner-part-sums pos-ints sum)
    (cons-stream (+ (stream-car pos-ints) sum)
                 (inner-part-sums (stream-cdr pos-ints) (+ (stream-car pos-ints) sum))))
  (inner-part-sums pos-ints 0))