#lang racket

#| all struct and function defs here |#

#| node struct definition contains elements for game state like board representation|#
(struct node (board turn))

#| new string with newline char added |#
(define (add_newline str)
  (string-join (list (substring str 0) (string #\newline)) ""))

#| interweave space character into string. split/join |#
(define (add_space str)
  (string-join (map string (string->list str))))

#| format row with and space and then newline |#
(define (create_row p)
  (add_newline
   (substring
    (add_space (node-board (list-ref p 0)))
    (list-ref p 1) (+ (list-ref p 1) 5))))

#| printing all three formatted rows |#
(define (nprint n) 
  (displayln
   (string-join 
    (map create_row
         (map ((curry list) n) '(0 6 12)) ) "")))

#| eval to updated board string |#
(define (update_board n move)
  (string-join
   (list
    (substring (node-board n) 0 (- move 1))
    (node-turn n)
    (substring (node-board n) move 9)) ""))

#| flip function for updating node |#
(define (flip n)
  (if (equal? (node-turn n) "x") "o" "x"))

#| eval to updated node with new board and flipped turn |#
(define (update_node n nb)
  (struct-copy node n
               [board (update_board n nb)]
               [turn (flip n)]))

#| check for three in a row and return 10 for score |#
(define (rows n)
  (for/list ([i '(0 3 6)])
    (if (equal? (substring (node-board n) i (+ i 3)) "...")
        10 0)))

#| accumulate scores into string |#
(define (score n)
  (string-join (list
                "score: " (number->string (for/sum ([i (rows n)]) i))) ""))

#| open string port to read input line on now #t |#
(define (get_input now)
  (if (equal? now #t)
       (for/list ([i '(1)])
         (display "Your move(1-9)")
         (let ([inp (read (open-input-string (read-line)))])
           (if (equal? #t (check_range inp)) inp (get_input now)))) "none"))

#| make sure input is integer between 1 and 9 |#
(define (check_range i)
  (if (and (> i 0) (< i 10))
      #t #f))
  
#| main recursive loop for game control |#
(define (myloop n)
  (let ([nn (update_node n (list-ref (get_input #t) 0))])  
    (nprint nn)
    (displayln (score nn))
    (myloop nn)))

#| program starts here |#

#|create root node|#

(displayln "Time To Tic Tac Toe\n")
(define root (node (make-string 9 #\.) "x"))
(nprint root)
(myloop root) ;recursive loop