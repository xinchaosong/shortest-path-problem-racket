;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Last Updated Date: December 18, 2017
Name: Xinchao Song
Email: contact@songxinchao.net
Project Number: SE1403
Project Name: shortest-path-problem-racket
File: dijkstra.rkt
Purposes: Using Dijkstra's algorithm to solve the shortest path problem
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

;; dijkstra: Node Node Graph -> [Maybe [List-of Node]]
;; produces a list of the nodes on the shortest path from source to target using Dijkstra's algorithm
(define (dijkstra source target graph)
  ; node-list is a list storing all nodes in the input graph g
  (define node-list (remove-duplicates (foldl (lambda (x y) (list* (car x) (caddr x) y)) '() graph)))
  ; node-index: Node -> Natural
  ; returns the index of a node in node-list
  (define (node-index node)
    (index-of node-list node))
  
  (cond
    ; if source or target is invaild, returns #false
    [(not (and (member source node-list) (member target node-list))) #false]
    ; otherwise solves the problem
    [else
     (let (; distance from source to source is 0 and from source to v is unknown
           [dist (for/vector ([i node-list]) (if (symbol=? i source) 0 +inf.0))]
           ; previous node in optimal path from source and from source to source is source itself
           [prev (for/vector ([i node-list]) (if (symbol=? i source) source 'UNDEFINED))]
           ; q-bool is a [Vector-of Boolean] that shows which nodes have not been processed
           ; #true means that the corresponding node has not been processed and vice versa
           [q-bool (make-vector (length node-list) #true)]
           ; u is the node node currently processed
           [u source])

       ; processes every node in node-list until the node currently processed is the end node
       ; or all nodes have been processed
       (for ([n node-list]
             #:break (symbol=? u target))
           
         ; node with the least distance will be processed first
         (let* ([min-dist (argmin values (for/list ([i dist] [j q-bool] #:when j) i))]
                [min-index (vector-member min-dist dist)])
           (set! u (list-ref node-list min-index)))
           
         ; marks the node u as processed
         (vector-set! q-bool (node-index u) #false)
           
         ; relaxes edges for each neighbor v of u
         (for ([i graph])             
           (let* ([v (caddr i)]
                  [weight (cadr i)]
                  [alt (+ (vector-ref dist (node-index u)) weight)])
             ; a shorter path to v has been found
             (when (and (symbol=? u (car i))
                        (vector-ref q-bool (node-index v))
                        (> (vector-ref dist (node-index v)) alt))
               ; updates the data of dist and prev
               (vector-set! dist (node-index v) alt)
               (vector-set! prev (node-index v) u)))))
         
       ; reads the shortest path from source to target           
       (let* ([u target]
              [prev-u (vector-ref prev (node-index u))])
         ; returns false if there is no valid previous node found
         (cond [(symbol=? prev-u 'UNDEFINED) #false] 
               [else
                ; constructs the shortest path with a stack
                (for/fold ([tail '()])
                          ([i prev]
                           ; pushes the source onto the stack at the end of the loop
                           #:final (symbol=? prev-u u))
                  ; pushes the vertex onto the stack and traverse from target to source
                  (begin0 (cons u tail)
                          (set! u prev-u)
                          (set! prev-u (vector-ref prev (node-index u)))))])))]))