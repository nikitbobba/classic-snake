;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname final_project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "snake-lib.rkt")

(define game-start
  (make-game (make-snake 'up (list (make-posn 3 3) (make-posn 3 4)))
             (list (make-posn 1 1) (make-posn 6 8) (make-posn 10 10)
                   (make-posn 7 7)(make-posn 24 25)(make-posn 30 30)(make-posn 35 35))
             (list (make-posn 47 9)(make-posn 15 15)) 4))
  
;USED FOR CHECK EXPECTS
(define game1 (make-game (make-snake 'up (list (make-posn 2 3) (make-posn 2 4)))
                         (list (make-posn 1 1)) empty 3))
(define game2 (make-game (make-snake 'up (list (make-posn 2 3) (make-posn 2 4)))
                         (list (make-posn 3 3) (make-posn 1 1)) empty 3))
(define game3 (make-game (make-snake 'up (list (make-posn 2 3) (make-posn 2 4)))
                         (list (make-posn 1 1)) empty 10))
(define game4 (make-game (make-snake 'right (list (make-posn 2 3) (make-posn 2 4)))
                         (list (make-posn 1 1)) empty 3))
(define game5 (make-game (make-snake 'up (list (make-posn 2 3) (make-posn 2 4)))
                         (list (make-posn 10 10)(make-posn 1 1)) empty 3))
(define game6 (make-game (make-snake 'left (list (make-posn 2 3) (make-posn 2 4)))
                         (list (make-posn 1 1)) empty 3))

; add-food: game posn -> game
;adds a morsel of food at the specified board position
(define (add-food g posn)
  (make-game (game-snake g) (cons posn (game-food g)) (game-obstacles g) (game-ticks g)))

(check-expect (add-food game1 (make-posn 3 3)) game2)
(check-expect (add-food game1 (make-posn 10 10)) game5)


;change-direction: game direction -> game
;changes the direction in which the snake is travelling
(define (change-direction g dir)
  (make-game (make-snake dir (snake-segments (game-snake g)))
             (game-food g) (game-obstacles g) (game-ticks g)))

(check-expect (change-direction game1 'right) game4)
(check-expect (change-direction game1 'left) game6)


;game-score: game -> nat
;computes player score
(define (game-score g)
  (- (* 100 (length (snake-segments (game-snake g)))) (game-ticks g)))

(check-expect (game-score game1) 197)
(check-expect (game-score game3) 190)


;HELPER FUNCTION FOR GAME-OVER?
;collison?: snake-segment obstacle -> boolean
;when snake hits wall
(define (collison? seg o)
  (cond [(empty? o)      false]
        [else             (cons? (filter (lambda (x) (equal? x (first seg))) o))]))

;HELPER FUNCTION FOR GAME-OVER?
;self-destruct?: snake -> boolean
;when snake hits itself
(define (self-destruct? s)
  (cons? (filter (lambda (x) (equal? (first (snake-segments s)) x)) (rest (snake-segments s)))))

;game-over?: game -> boolean
;ends the game
(define (game-over? g)
  (or (= 51 (posn-x (first (snake-segments (game-snake g)))))
      (= 51 (posn-y (first (snake-segments (game-snake g)))))
      (= 0 (posn-x (first (snake-segments (game-snake g)))))
      (= 0 (posn-y (first (snake-segments (game-snake g)))))
      (collison? (snake-segments (game-snake g)) (game-obstacles g))
      (self-destruct? (game-snake g))))

(check-expect (game-over? game3) false)
(check-expect (game-over? game2) false)


;HELPER FUNCTION FOR ADVANCE-GAME
;drop-list: list -> list
(define (drop-list lst)
  (cond [(empty? (rest lst))     empty]
        [else         (cons (first lst) (drop-list (rest lst)))]))
(check-expect (drop-list (list 1 2 3)) (list 1 2))

      
;HELPER FUNCTION FOR ADVANCE-GAME
(define (helper-advanced g)
  (cond [(symbol=? 'up
                   (snake-heading (game-snake g))) (make-posn
                                                    (posn-x (first (snake-segments
                                                                    (game-snake g))))
                                                    (+ 1 (posn-y (first (snake-segments
                                                                         (game-snake g))))))]
        [(symbol=? 'right
                   (snake-heading (game-snake g))) (make-posn
                                                (+ 1 (posn-x
                                                      (first (snake-segments
                                                              (game-snake g)))))
                                                 (posn-y
                                                  (first (snake-segments
                                                          (game-snake g)))))]
        [(symbol=? 'down
                   (snake-heading (game-snake g))) (make-posn
                                                  (posn-x
                                                   (first (snake-segments
                                                           (game-snake g))))
                                                 (- (posn-y (first (snake-segments
                                                                    (game-snake g)))) 1))]
        [(symbol=? 'left
                   (snake-heading (game-snake g))) (make-posn 
                                                (- (posn-x
                                                    (first (snake-segments
                                                            (game-snake g)))) 1)
                                               (posn-y (first (snake-segments
                                                               (game-snake g)))))]))


;advance-game: game -> game
(define (advance-game g)
  (cond [(member (helper-advanced g)
                 (game-food g))    (make-game
                                    (make-snake (snake-heading (game-snake g))
                                                (cons
                                                 (helper-advanced g) (snake-segments (game-snake g))))
                                    (remove (helper-advanced g) (game-food g))
                                    (game-obstacles g) (+ 1 (game-ticks g)))]
        [else             (make-game
                           (make-snake (snake-heading (game-snake g))
                                       (cons (helper-advanced g)
                                             (drop-list (snake-segments (game-snake g)))))
                           (game-food g) (game-obstacles g) (+ 1 (game-ticks g)))])) 
(check-expect (advance-game game1)
              (make-game (make-snake 'up (list (make-posn 2 4) (make-posn 2 3)))
                         (list (make-posn 1 1)) '() 4))
(check-expect (advance-game game3)
              (make-game (make-snake 'up (list (make-posn 2 4) (make-posn 2 3)))
                         (list (make-posn 1 1)) '() 11))
                                              
       




  
  


  

