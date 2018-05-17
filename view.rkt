;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Date: May 17, 2017
Name: Xinchao Song
Email: contact@songxinchao.com
Project Number: SE1403
Project Name: shortest-path-problem-racket
File: view.rkt
Purposes: The GUI view of the demo program
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/gui

(require racket/draw)
(require racket/math)
(require "dijkstra.rkt")
(require "graph-example.rkt")

(provide view-go)

;; A Line is a (line start-node end-node x1 y1 x2 y2)
;; Symbol Symbol Integer Integer Integer Integer)
(struct line (start-node end-node x1 y1 x2 y2))

;; Constants
(define WIDTH 520)
(define HEIGHT 520)
(define GRAPH (read-bitmap "graph1.png"))
(define LINE0 (line 'H 'J 32 60 32 181))
(define LINE1 (line 'J 'F 32 228 32 358))
(define LINE2 (line 'H 'A 55 37 213 37))
(define LINE3 (line 'A 'G 260 37 441 37))
(define LINE4 (line 'B 'G 465 183 465 62))
(define LINE5 (line 'B 'I 465 228 465 358))
(define LINE6 (line 'F 'I 55 383 441 383))
(define LINE7 (line 'I 'C 448 367 373 295))
(define LINE8 (line 'C 'D 335 288 258 327))
(define LINE9 (line 'C 'E 336 266 257 218))
(define LINE10 (line 'E 'A 237 183 237 61))
(define LINE11 (line 'A 'C 247 58 345 256))
(define LINE12 (line 'A 'F 225 57 44 362))
(define LINE13 (line 'F 'E 49 368 219 221))
(define LINE14 (line 'D 'E 237 315 237 230))
(define LINE15 (line 'A 'B 256 51 446 191))
(define LINE16 (line 'B 'C 446 218 375 265))
(define LINE17 (line 'B 'A 446 191 256 51))
(define LINE-LIST (list LINE0 LINE1 LINE2 LINE3 LINE4 LINE5 LINE6 LINE7 LINE8 LINE9 LINE10 LINE11
                        LINE12 LINE13 LINE14 LINE15 LINE16 LINE17))

(define path-list '())

;; Makes a frame by instantiating the frame% class
(define frame (new frame%
                   [label "shortest-path"]
                   [min-width WIDTH]
                   [min-height HEIGHT]
                   [alignment '(center top)]))

; Makes a horizontal panel in the frame for the canvas
(define canvas-panel
  (new horizontal-panel% [parent frame]
       [alignment '(center center)]
       [min-width 500]
       [min-height 428]))

; Makes a horizontal panel in the frame for the text fields
(define text-field-panel
  (new horizontal-panel% [parent frame]
       [alignment '(center center)]))

; Makes a horizontal panel in the frame for the buttons
(define button-panel
  (new horizontal-panel% [parent frame]
       [alignment '(center center)]))

;; Makes a canvas in the canvas-panel
(define canvas (new canvas% [parent canvas-panel]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc clear)
                       (send dc draw-bitmap GRAPH 0 0)                 
                       (send dc set-pen "red" 5 'solid)
                       (draw-lines dc)
                       ;(send dc set-pen "white" 1 'solid)
                       ;(send dc set-brush "white" 'solid)
                       ;(send dc draw-rectangle 20 110 32 22)
                       ;(send dc draw-rectangle 20 285 32 22)
                       ;(send dc draw-text "25" 20 112)
                       ;(send dc draw-text "25" 20 285)
                       )]))

;; Makes a text field in the canvas-panel for inputting the source node
(define text-field-source
  (new text-field% [parent text-field-panel]
       [label "source"]
       [init-value "A"]))

;; Makes a text field in the canvas-panel for inputting the target node
(define text-field-target
  (new text-field% [parent text-field-panel]
       [label "target"]
       [init-value "B"]))

;; Makes a button in the canvas-panel
(define draw-button
  (new button% [parent button-panel]
       [label "Dijkstra"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (define node-list '())
                   (draw-dijkstra)              
                   (send canvas on-paint))]))

;; Draws the lines in the path-list
;; dc<%> -> #<void>
(define (draw-lines dc)
  (for ([i path-list])
    (send dc draw-line (line-x1 i) (line-y1 i) (line-x2 i) (line-y2 i))))

;; Updates the path-list by using Dijkstra's algorithm with the given source node and target node
(define (draw-dijkstra)
  (define source (string->symbol (string-upcase (send text-field-source get-value))))
  (define target (string->symbol (string-upcase (send text-field-target get-value))))
  (define graph graph-pos)
  (define node-list (dijkstra source target graph))
  ; get-lines: [List-of Symbol] -> [List-of Line]
  (define (get-lines node-list)
    (cond [(boolean? node-list) '()]
          [(empty? (rest node-list)) '()]
          [else (for/last ([i LINE-LIST]
                           #:when (and (symbol=? (first node-list) (line-start-node i))
                                       (symbol=? (second node-list) (line-end-node i))))
                  (cons i (get-lines (rest node-list))))]))
  (set! path-list (get-lines node-list)))

;; Shows the frame
(define (view-go)
  (send frame show #true))