#lang racket

(displayln "Time To Tic Tac Toe\n")

(struct node (board turn))

(define (char_to_str x) (make-string 1 x))

(define (nprint node) 
  (displayln (string-replace
              (string-replace
               (string-join
                (map char_to_str
                     (string->list
                      (node-board node))) " ") ". . ." ". . . \n") "\n " "\n")))



(define (sanitize i) (
                      if (and (> (string->number i) 0) (< (string->number i) 10))
                         #t #f))

(define root (node (make-string 9 #\.) "x"))
(nprint root)

(define child (struct-copy node root
             [turn "o"]))

(displayln (node-turn root))
(nprint child)
(displayln (node-turn child))

(sanitize (read-string 1))