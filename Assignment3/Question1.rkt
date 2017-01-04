; Question 1
(define (make-node val)
  val)
(define (make-edge n1 n2)
  (cons n1 n2))

(define (make-graph)
  (let ((node-list '())
        (edge-list '()))
    
    (define (add-node n)
      (if (null? (has-node? n))
          (set! node-list (cons n node-list))
          (display "Node already present in graph.\n")))

    (define (get-edges n)
      (car (cdr n)))

    (define (add-edge e)
      (cond ((null? (has-node? (car e))) (display "Node 1 not present.\n"))
            ((null? (has-node? (cdr e))) (display "Node 2 not present.\n"))
            (else (set! edge-list (cons  (cons (car e) (cdr e)) edge-list)))))
    
    (define (delete-edge e)
      (define (inner-delete e lis)
        (if (null? lis)
            '()
            (if (= (car (car lis)) (car e))
                (if (= (cdr (car lis)) (cdr e))
                    (inner-delete e (cdr lis))
                    (cons (car lis) (inner-delete e (cdr lis))))
                (cons (car lis) (inner-delete e (cdr lis))))))
      (set! edge-list (inner-delete e edge-list)))

    (define (delete-assoc-edges n)
      ; To be used by delete-node
      (define (inner-assoc n lis)
        (if (null? lis)
            '()
            (if (= (cdr (car lis)) n)
                (inner-assoc n (cdr lis))
                (if (= (car (car lis)) n)
                    (inner-assoc n (cdr lis))
                    (cons (car lis) (inner-assoc n (cdr lis)))))))
      (set! edge-list (inner-assoc n edge-list)))

    (define (delete-node n)
      (define (inner-delete n lis)
        (if (null? lis)
            '()
            (if (= n (car lis))
                (begin
                  (delete-assoc-edges n) ; delete associated edges
                  (inner-delete n (cdr lis)))
                (cons (car lis) (inner-delete n (cdr lis))))))
      (set! node-list (inner-delete n node-list)))
                    
    (define (test-print) ; diagnostic tool
      node-list)
    (define (print-edges) ; diagnostic tool
      edge-list)

    (define (has-node? n)
      (define (hn-inner n li)
        (if (null? li)
            '()
            (if (= (car li)  n)
                (car li)
                (hn-inner n (cdr li)))))
      (hn-inner n node-list))

    (define (print-graph)
      (define (inner-print lis)
        (if (null? lis)
            '()
            (cons (cons (car lis) (get-assoc-edges (car lis))) (inner-print (cdr lis))))); Attach node to all associated edges
      (inner-print node-list))

    (define (get-assoc-edges n)
      (define (inner-get n lis)
        (if (null? lis)
            '()
            (if (= (car (car lis)) n)
                (cons (cdr (car lis)) (inner-get n (cdr lis)))
                (inner-get n (cdr lis)))))
      (list (inner-get n edge-list)))
    
    (define (dispatch method)
      (cond ((eq? method 'add-node) add-node)
            ((eq? method 'print) test-print)
            ((eq? method 'print-edges) print-edges)
            ((eq? method 'add-edge) add-edge)
            ((eq? method 'delete-edge) delete-edge)
            ((eq? method 'delete-node) delete-node)
            ((eq? method 'print-graph) print-graph)
            (else (display "error"))))
  dispatch))

; Testing for Question 1
(define no (make-node 1))
(define no2 (make-node 2))
(define a (make-graph))
((a 'add-node) no)
;((a 'print))
(display "Attemp to add an edge with a non-existent node:\n")
(define edge1 (make-edge no no2))
((a 'add-edge) edge1) ; Add some nodes and edges to the graph
((a 'add-node) no2)
((a 'add-edge) edge1)
((a 'print-graph))
(define no3 (make-node 3)) ; Make some more nodes and edges
(define no4 (make-node 4))
((a 'add-node) no3)
((a 'add-node) no4)
(define edge2 (make-edge no3 no4))
((a 'add-edge) edge2)
((a 'delete-edge) edge2) ; Remove an edge
((a 'delete-node) no3)
((a 'print-graph))
((a 'add-node) no3)
((a 'add-edge) edge2)
((a 'print-graph))
(define edge3 (make-edge no no3)) ; Create an edge to test multiple edges on one node
((a 'add-edge) edge3)
((a 'print-graph))
((a 'delete-node) no4)
; Removed node 4. Note the edge from 3 to 4 is removed automatically
((a 'print-graph))
((a 'delete-node) no3)
((a 'print-graph))
((a 'delete-node) no2)
((a 'print-graph))


