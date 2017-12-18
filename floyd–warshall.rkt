;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Last Updated Date: December 18, 2017
Name: Xinchao Song
Email: contact@songxinchao.net
Project Number: SE1403
Project Name: shortest-path-problem-racket
File: floyd–warshall.rkt
Purposes: Using Floyd–Warshall algorithm to solve the shortest path problem
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/base

(require racket/list)
(require racket/vector)
(require racket/bool)
(require "graph-example.rkt")

;; A [Maybe X] is one of: 
;; – #false
;; – X

;; floyd–warshall: Node Node Graph -> [Maybe [List-of Node]]
;; produces a list of the nodes on the shortest path from s to t using Floyd–Warshall algorithm
(define (floyd–warshall source target graph)
  ; node-list is a list storing all nodes in the input graph g
  (define node-list (remove-duplicates (foldl (lambda (x y) (list* (car x) (caddr x) y)) '() graph)))
  
  ; node-index: Node -> Natural
  ; returns the index of a node in node-list
  (define (node-index node)
    (index-of node-list node))
  
  ; matrix-ref: [Vector-of [Vector-of X]] Natural Natural -> X
  ; returns the element in slot (row, column) of matrix
  ; matrix is a [Vector-of [Vector-of X]]
  (define (matrix-ref matrix row column)
    (vector-ref (vector-ref matrix row) column))

  (cond
    ; if source or target is invaild, returns #false
    [(not (and (member source node-list) (member target node-list))) #false]
    ; otherwise solves the problem
    [else
     (let (; initializes the distances between any two vertices as unknown
           [dist (build-vector (length node-list)
                               (lambda (x) (make-vector (length node-list) +inf.0)))]
           ; initializes the next node in optimal path as undefined
           [next (build-vector (length node-list)
                               (lambda (x) (make-vector (length node-list) 'UNDEFINED)))])
           
       ; the distance from a vertex to itself is 0
       (for ([i (in-range (length node-list))])
         (for ([j (in-range (length node-list))])
           (when (= i j)
             (vector-set! (vector-ref dist i) j 0))))
           
       ; initializes the dist and the next for each edge
       (for ([i graph])
         (let* ([u (car i)]
                [v (caddr i)]
                [weight (cadr i)])
           (vector-set! (vector-ref dist (node-index u)) (node-index v) weight)
           (vector-set! (vector-ref next (node-index u)) (node-index v) v)))

       ; standard Floyd-Warshall implementation
       (for ([k (in-range (length node-list))])
         (for ([i (in-range (length node-list))])
           (for ([j (in-range (length node-list))])
             (let ([alt (+ (matrix-ref dist i k) (matrix-ref dist k j))])
               ; a shorter path has been found
               (when (> (matrix-ref dist i j) alt)
                 ; updates the data of dist and next
                 (vector-set! (vector-ref dist i) j alt)
                 (vector-set! (vector-ref next i) j (matrix-ref next i k)))))))
           
       ; reads the shortest path from source to target
       (let* ([u source]
              [next-u (matrix-ref next (node-index u) (node-index target))])
         ; returns false if there is no valid next node found
         (cond [(symbol=? next-u 'UNDEFINED) #false] 
               [else
                ; constructs the shortest path with a stack
                (for/list ([i next]
                           ; pushes the target onto the stack at the end of the loop
                           #:final (symbol=? u target))
                  ; pushes the vertex onto the stack and traverse from source to target
                  (begin0 u
                          (set! next-u (matrix-ref next (node-index u) (node-index target)))
                          (set! u next-u)))])))]))