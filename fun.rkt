#lang racket

(require (planet williams/describe/describe))

#| all struct and function defs here |#

(struct node (board turn))

(define (char_to_str x) (make-string 1 x))

(define (add_newline str)
  (string-join (list (substring str 0) (char_to_str #\newline)) ""))

(define (add_space str)
  (string-join (map char_to_str (string->list str))))

(define (create_row str x)
  (add_newline (substring str x (+ x 5))))

(define (nprint node) 
  (displayln
   (string-join
    (map create_row
         (build-list 3 (lambda (x) (add_space (node-board node))))
         (list 0 6 12)) "")))

(define (sanitize i) (
                      if (and (> (string->number i) 0) (< (string->number i) 10))
                         #t #f))

(define (update node move)
  (string-join
   (list
    (substring (node-board node) 0 (- move 1))
    (node-turn node)
    (substring (node-board node) move 9)) ""))

(define (rows node)
  (for/list ([i '(0 3 6)])
    (if (equal? (substring (node-board node) i (+ i 3)) "...")
        10 0)))

(define (score node)
  (for/sum ([i (rows node)]) i))

#| program starts here |#


#|create root node|#
(displayln "Time To Tic Tac Toe\n")
(define root (node (make-string 9 #\.) "x"))
(nprint root)
(displayln (score root))

#| loop for nine moves; take input and display newly constructed node |#
(for ([i (build-list 9 values)])
  (list (display "Your move(1-9): ")
        (let ([myport (open-input-string (read-line))])  
        (nprint (struct-copy node root
                             [board (update root (read myport))]
                             [turn "o"])))))

