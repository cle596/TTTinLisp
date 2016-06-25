#lang racket

(displayln "Time To Tic Tac Toe\n")

(struct node (board turn))

(define (char_to_str x) (make-string 1 x))

(define (add_newline str)
  (string-join (list (substring str 0) (char_to_str #\newline)) ""))

(define (add_space str)
  (string-join (map char_to_str (string->list str))))

(define (create_row node x)
  (add_newline (substring (add_space (node-board node)) x (+ x 5))))

(define (nprint node) 
  (displayln
   (string-join
    (map create_row (list node node node) (list 0 6 12)) "")))

(define (sanitize i) (
                      if (and (> (string->number i) 0) (< (string->number i) 10))
                         #t #f))

(define root (node (make-string 9 #\.) "x"))
(nprint root)

(define (update node move)
  (string-join
   (list
    (substring (node-board node) 0 (- move 1))
    (node-turn node)
    (substring (node-board node) move 9)) ""))

(define child (struct-copy node root [board (update root 5)] [turn "o"]))

(nprint child)

;(sanitize (read-string 1))


