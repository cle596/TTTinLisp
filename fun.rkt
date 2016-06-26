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

(define (flip n)
  (if (equal? (node-turn n) "x") "o" "x"))

(define (myloop n)
          (let ([new_node (struct-copy node n
                                     [board (update n (read (open-input-string (read-line))))]
                                     [turn (flip n)])])  
          (nprint new_node)
          (displayln (score new_node))
            (myloop new_node)))

#| program starts here |#

#|create root node|#
(displayln "Time To Tic Tac Toe\n")

(define root (node (make-string 9 #\.) "x"))

(nprint root)
(displayln (score root))
(myloop root)