;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Last Updated Date: December 18, 2017
Name: Xinchao Song
Email: contact@songxinchao.com
Project Number: SE1403
Project Name: shortest-path-problem-racket
File: graph-for-test.rkt
Purposes: provides two graph examples for testing
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/base

(provide graph-pos)
(provide graph-neg)

;; Data Definitions

;; A Node is a Symbol 
;; INTERP: represents the name of a node in a graph

;; A Distance is a PosInt
;; INTERP: represents distance in miles

;; An Edge is (list Node Distance Node)
;; e.g. (list 'A 10 'B)
;; INTERP: represents an edge from 'A to 'B with the distance from 'A to 'B being 
;;         10 miles

;; A Path is a [List-of Edge]
;; A Graph is a [Set-of Edge]
  
; Edges
(define edge0 (list 'A 10 'B))
(define edge1 (list 'B 10 'C))
(define edge2 (list 'C 10 'D))
(define edge3 (list 'D 15 'E))
(define edge4 (list 'E 15 'A))
(define edge5 (list 'A 12 'F))
(define edge6 (list 'F 14 'E))
(define edge7 (list 'A 9 'C))
(define edge8 (list 'A 7 'G))
(define edge9 (list 'B 11 'I))
(define edge10 (list 'I 8 'C))
(define edge11 (list 'H 6 'A))
(define edge12 (list 'H 18 'J))
(define edge13 (list 'B 10 'A))
(define edge14 (list 'B 8 'G))
(define edge15 (list 'F 5 'I))
(define edge16 (list 'J 20 'F))
(define edge17 (list 'C 15 'E))
(define edge18 (list 'C -200 'E))

;; Graphs
(define graph-pos (list edge0 edge1 edge2 edge3 edge4 edge5 edge6 edge7 edge8 edge9
                             edge10 edge11 edge12 edge13 edge14 edge15 edge16 edge17))
(define graph-neg (list edge0 edge1 edge2 edge3 edge4 edge5 edge6 edge7 edge8 edge9
                             edge10 edge11 edge12 edge13 edge14 edge15 edge16 edge18))