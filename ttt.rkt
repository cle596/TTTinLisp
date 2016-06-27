#lang racket

#| all struct and function defs here |#

#| node struct definition contains elements for
game state like board representation|#
(struct node (board turn))

#| new string with newline char added |#
(define (add_newline str)
  (string-append str (string #\newline)))

#| interweave space character into string. split/join |#
(define (add_space str)
  (string-join (map string (string->list str))))

#| format row with and space and then newline |#
(define (create_row p)
  (let ([n (list-ref p 0)]
        [i (list-ref p 1)])
    (add_newline
     (substring
      (add_space (node-board n))
      i (+ i 5)))))

#| printing all three formatted rows |#
(define (nprint n)
  (displayln
   (string-join
    (map create_row
         (map ((curry list) n) '(0 6 12)) ) "")))

#| eval to updated board string |#
(define (update_board n move)
  (let ([first (substring (node-board n) 0 (- move 1))]
        [last (substring (node-board n) move 9)])
    (string-join
     (list
      first
      (node-turn n)
      last) "")))

#| flip function for updating node |#
(define (flip n)
  (if (equal? (node-turn n) "x") "o" "x"))

#| eval to updated node with new board and flipped turn |#
(define (update_node n nb)
  (struct-copy node n
               [board (update_board n nb)]
               [turn (flip n)]))

#| check for two in a row and return 5 for score
   three in a row and return 10 for score
   making sure half rows and full rows are mutually excluded|#
(define (rows n)
  (for/list ([i '(0 3 6)])
    (let ([s1 (substring (node-board n) i (+ i 2))]
          [s2 (substring (node-board n) (+ i 1) (+ i 3))]
          [str (string-append (flip n) (flip n))]
          [val (if (equal? (flip n) "x") 5 -5)])
      (cond
        [(equal? s1 str) (if (equal? s2 str)
                             (* 2 val) val)]
        [else (if (equal? s2 str) val 0)]))))


#| check for three in a col and return 10 for score |#
(define (cols n)
  (for/list ([i '(0 1 2)])
    (let ([s1 (list->string
               (map ((curry string-ref) (node-board n))
                    (list i (+ i 3))))]
          [s2 (list->string
               (map ((curry string-ref) (node-board n))
                    (list (+ i 3) (+ i 6))))]
          [x "xx"]
          [o "oo"]
          [val 5])
      (cond
        [(equal? s1 x) (if (equal? s2 x) (* 2 val) val)] 
        [(equal? s1 o) (if (equal? s2 o) (* -2 val) (* -1 val))]
        [(equal? s2 x) val]
        [(equal? s2 o) (* -1 val)]
        [else 0]))))

#| accumulate scores into string |#
(define (score n)
  (string-join (list
                "score: "
                (number->string
                 (+ (for/sum ([i (rows n)]) i)
                    (for/sum ([i (cols n)]) i) 0))) ""))

#| open string port to read input line on now #t |#
(define (get_input now n)
  (if (equal? now #t)
      (begin
        (display "Your move(1-9): ")
        (let ([inp (read (open-input-string (read-line)))])
          (if (equal? #t (sanitize inp n)) inp (get_input now n)))) "none"))

#| make sure input is integer between 1 and 9 |#
(define (sanitize i n)
  (if (and (number? i) (equal?
                        (string-ref (node-board n) (- i 1)) #\.)
           (> i 0)
           (< i 10))
      #t #f))

#| main recursive loop for game control |#
(define (myloop n)
  (let ([nn (update_node n (get_input #t n))])
    (nprint nn)
    (displayln (score nn))
    (myloop nn)))

#| program starts here |#

#| create root node |#

(displayln "Time To Tic Tac Toe\n")
(define root (node (make-string 9 #\.) "x"))
(nprint root)
(myloop root) ;recursive loop
