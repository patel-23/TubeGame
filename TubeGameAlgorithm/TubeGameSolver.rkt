;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname TubeGameSolver) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(require "TubeGameGraphics.rkt")

(define-struct game (tubesize maxcolours tubes))

;; ===================================[Constants]===================================

(define smallgame1
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define nextsmallgame (list (make-game 2 2
                                       (list (list 'red)
                                             (list 'blue 'red)
                                             (list 'blue)))
                            (make-game 2 2
                                       (list (list 'blue 'red)
                                             (list'red)
                                             (list 'blue)))))

(define mediumgame
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   (list))))

(define mediumgamestuck
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   )))

(define largergame
  (make-game 3 3
             (list (list 'blue 'red 'red)
                   (list 'yellow 'blue 'yellow)
                   (list 'red 'yellow 'blue)
                   (list))))

(define biggame
  (make-game 5 3
             (list (list 'blue 'blue 'red 'red 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list)
                   (list))))

(define hugegame
  (make-game 4 9
             (list (list 'purple 'pink 'yellow 'blue)
                   (list 'blue 'green 'purple 'white)
                   (list 'orange 'yellow 'black)
                   (list 'white 'orange 'orange 'pink)
                   (list 'pink 'red 'red 'black)
                   (list 'yellow 'green 'orange)
                   (list 'white 'purple 'red)
                   (list 'green 'red 'green 'black)
                   (list 'purple 'black )
                   (list 'pink 'white)
                   (list 'blue 'yellow 'blue))))





;; (solve gm draw-option) determines if the game gm is solveable,
;; and will also draw each possible move depending on the draw-option

;; Example:
(check-expect (time (solve smallgame1 'norm)) true)

;; solve: Game (anyof 'off 'norm 'slow 'fast) -> Bool
(define (solve gm draw-option)
  (local
    [(define setup (puzzle-setup gm draw-option))                
     (define (solve-helper to-visit visited)
       (cond
         [(empty? to-visit) false]
         [else
          (local
            [(define draw (draw-board (first to-visit) draw-option))] 
            (cond
              [(finished-game? (first to-visit)) true]
              [(member? (first to-visit) visited)
               (solve-helper (rest to-visit) visited)]
              [else
               (local [(define neighbours (next-games (first to-visit)))
                       (define new (filter (lambda (x) (not (member? x visited))) neighbours))
                       (define new-to-visit (append new (rest to-visit)))
                       (define new-visited (cons (first to-visit) visited))]
                 (solve-helper new-to-visit new-visited))]))]))]
    (solve-helper (list gm) empty)))

;; Tests:
(check-expect (time (solve mediumgame 'norm)) true)
(check-expect (time (solve mediumgamestuck 'norm)) false)
(check-expect (time (solve largergame 'norm)) true)
(check-expect (time (solve biggame 'norm)) true)
(check-expect (time (solve hugegame 'norm)) true)





;; ===================================[Helper Functions]===================================

;; (next-games gm) consumes a Game and produces a list of all possible game configurations
;; after moving one ball

;; Examples:
(check-expect (next-games mediumgamestuck) empty)
(check-expect (next-games smallgame1) nextsmallgame)

;; next-games: Game -> (listof Game)
(define (next-games gm)
  (local
    [(define tubes (game-tubes gm))
     ;; (next-tubes gm) consumes a Game and two natural numbers and produces a list of all
     ;; possible game configurations after moving one ball.
     ;; next-tubes: Game Nat Nat -> (listof Games)
     ;; Requires: tube1 = 0, tube2 = 0
     (define (next-tubes gm tube1 tube2)
       (cond
         [(= (length tubes) tube1) empty]
         [(= (length tubes) tube2) (next-tubes gm (add1 tube1) 1)]
         [(empty? (list-ref tubes tube1)) (next-tubes gm (add1 tube1) 1)]
         [(= (game-tubesize gm) (length (list-ref tubes tube2))) (next-tubes gm tube1 (add1 tube2))]
         [else
          (cons (make-game (game-tubesize gm) (game-maxcolours gm)
                           (build-list (length tubes)
                                       (lambda (index) (cond
                                                     [(= index tube1) (rest (list-ref tubes index))]
                                                     [(= index tube2)
                                                      (cons (first (list-ref tubes tube1))
                                                            (list-ref tubes tube2))]
                                                     [else (list-ref tubes index)]))))
                (next-tubes gm tube1 (add1 tube2)))]))]
    (next-tubes gm 0 0)))





;; (check-colour? size num los) consumes two natural numbers and a list of symbols. It produces true
;; if each symbol appears exactly [size] times and if there are no more than [num] different symbols
;; and false otherwise. 

;; Examples:
(check-expect (check-colour? 1 3 '(red orange yellow)) true)
(check-expect (check-colour? 23 2 '(white black white)) false)

;; check-colour?: Nat Nat (listof Sym) -> Bool
(define (check-colour? size num los)
  (local
    [;; (verify-colour size num los colours) consumes three natural numbers and a list of sumbols. It
     ;; produces true if each symbol appears exactly [size] times and if there are no more than [num]
     ;; different symbols and false otherwise.
     ;; verify-colour: Nat Nat (listof Sym) Nat -> Bool
     ;; Requires: colours = 0
     (define (verify-colour size num los colours)
       (cond
         [(empty? los) true]
         [(and (<= num colours) (empty? los)) true]
         [(<= num colours) false]
         [(not (= size (length (filter (lambda (colour) (symbol=? colour (first los))) los)))) false]
         [else (verify-colour size num
                              (filter (lambda (colour) (not (symbol=? colour (first los)))) los)
                              (add1 colours))]))]
    (verify-colour size num los 0)))





;; (remove-completed gm) consumes a Game and produces the same game with all the completed tubes
;; removed from the game. That is, all tubes containing balls of the same colour are removed.

;; Examples:
(check-expect (remove-completed (make-game 2 3
                                           (list (list 'blue 'blue)
                                                 (list 'red 'yellow)
                                                 (list 'yellow 'red)
                                                 (list))))
              (make-game 2 2
                         (list (list 'red 'yellow)
                               (list 'yellow 'red)
                               (list))))
(check-expect (remove-completed biggame) biggame)

;; remove-completed: Game -> Game
(define (remove-completed gm)
  (local
    [(define new-lot
       (filter (lambda (tube)
                 (not (and (= (length tube) (game-tubesize gm))
                           (check-colour? (game-tubesize gm) 1 tube))))
               (game-tubes gm)))]
    (cond
      [(= 0 (game-tubesize gm)) (make-game 0 0 '())]
      [(empty? (filter (lambda (tube) (not (empty? tube))) (game-tubes gm)))
       (make-game (game-tubesize gm) 0 (game-tubes gm))]
      [else (make-game (game-tubesize gm)
                       (- (game-maxcolours gm) (- (length (game-tubes gm)) (length new-lot)))
                       new-lot)])))





;; (finished-game? gm) consumes a Game and produce true if all the tubes either contain balls of
;; one colour or if they're empty and false otherwise.

;; Examples:
(check-expect (finished-game? (make-game 2 2
                                         (list (list 'blue 'blue)
                                               (list 'red 'red)
                                               (list)))) true)
(check-expect (finished-game? largergame) false)

;; finished-game?: Game -> Bool
(define (finished-game? gm)
  (empty? (filter (lambda (tube) (not (empty? tube))) (game-tubes (remove-completed gm)))))