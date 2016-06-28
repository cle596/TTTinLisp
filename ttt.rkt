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

(define (form_str str a b c)
  (list->string (list
                 (string-ref str a)
                 (string-ref str b)
                 (string-ref str c))))

#| accumulate scores for twos and threes in eight different lines |#
(define (score n)
  (let ([s1 (substring (node-board n) 0 3)]
        [s2 (substring (node-board n) 3 6)]
        [s3 (substring (node-board n) 6 9)]
        [s4 (form_str (node-board n) 0 3 6)]
        [s5 (form_str (node-board n) 1 4 7)]
        [s6 (form_str (node-board n) 2 5 8)]
        [s7 (form_str (node-board n) 0 4 8)]
        [s8 (form_str (node-board n) 2 4 6)]
        [val 1]
        [win 10])
    (for/sum ([i (for/list ([i (list s1 s2 s3 s4 s5 s6 s7 s8)]) 
                   (cond
                     [(equal? i "xxx") win]
                     [(equal? i "xx.") val]
                     [(equal? i ".xx") val]
                     [(equal? i "x.x") val]
                     [(equal? i "ooo") (* -1 win)]
                     [(equal? i "oo.") (* -1 val)]
                     [(equal? i ".oo") (* -1 val)]
                     [(equal? i "o.o") (* -1 val)]
                     [else 0]) )]) i)))

(define (game_on? n s)
  (and (< (abs s) 8)
       (string-contains? (node-board n) ".")))

#| open string port to read input line on now #t |#
(define (get_input now n)
  (if (equal? now #t)
      (begin
        (display "Your move (1-9): ")
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
  (begin
    (displayln (expand n 0))
    (let ([nn (update_node n (get_input #t n))])
      (nprint nn)
      (let ([s (score nn)])
        (displayln (string-append "score: " (number->string s)))
        (if (game_on? nn s)
            (myloop nn)
            (displayln "game over"))))))

(define (gen n)
  (filter number? 
          (for/list ([x (in-range 9)])
            (if (equal? (string-ref (node-board n) x) #\.) (add1 x) "none"))))

#| expanding one parent into its children; recursive call |#
(define (expand n v)
  (if (equal? (game_on? n (score n)) #t)
      (letrec ([moves (gen n)]
               [childs (for/list ([m moves])
                         (struct-copy node n
                                      [board (update_board n m)]
                                      [turn (flip n)]))])
        (for/last ([c childs])
          
          (if (equal? (node-turn c) "x")
              (max (expand c v) v)
              (min (expand c v) v))
          ))
      (score n)))

#| program starts here |#

(displayln "Time To Tic Tac Toe\n")
(define root (node (make-string 9 #\.) "x"))
(nprint root)
(myloop root) ;recursive loop
