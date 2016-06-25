#lang racket

(require (planet williams/describe/describe))

(displayln "Time To Tic Tac Toe\n")

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

(define root (node (make-string 9 #\.) "x"))
(nprint root)

#|
(nprint (struct-copy node root
                     [board (update root (string->number (read-string 1)))]
                     [turn "o"]))




|#
(for ([i (build-list 10 values)])
  (list (display "Your move(1-9): ")
        (let ([myport (open-input-string (read-string 2))])  
        (nprint (struct-copy node root
                             [board (update root (read myport))]
                             [turn "o"]))
        )))




;(read-string 1)

;(read-char 1)
