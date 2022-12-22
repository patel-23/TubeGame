(module draw-tubes racket
  (require racket/gui/base)
  (require racket/draw)


  ;; Width of the tubes
  (define tube-width 30)

  ;; Time between boards being shown, in seconds
  (define slow-pause 0.5)
  (define fast-pause 0.025)
  (define normal-pause 0.1) 

  (define (colour-map s)
    (cond
      [(symbol=? s 'white) "white"]
      [(symbol=? s 'blue) "blue"]
      [(symbol=? s 'red) "red"]
      [(symbol=? s 'pink) "pink"]
      [(symbol=? s 'green) "green"]
      [(symbol=? s 'purple) "purple"]
      [(symbol=? s 'yellow) "yellow"]
      [(symbol=? s 'orange) "orange"]
      [(symbol=? s 'black) "black"]
      ))

  ;; Creates frame to display tubes
  (define ball-size (- tube-width 1))
  (define tube-spacing 4)
  
  (define (get-tubesize-lib puzz)
    (vector-ref (struct->vector puzz) 1))
  
  (define (get-numtubes-lib puzz)
    (length (vector-ref (struct->vector puzz) 3)))
  
  (define (get-tubes-lib puzz)
    (vector-ref (struct->vector puzz) 3))
  
  (define board-frame (new frame% [label "Solve-Tubes"]
                           [width 400]
                           [height 400]
                           [x 400]
                           [y 400]))
  (define backing-bitmap (make-screen-bitmap 300 300))
  (define board-dc (send backing-bitmap make-dc))
  (define board-canvas (new canvas%
                            [parent board-frame]
                            [paint-callback
                             (λ (self dc)
                               (send dc draw-bitmap backing-bitmap 0 0))]))
  
  (define (resize-frame new-width new-height)
    (begin (show-board false)
           (set! board-frame (new frame% [label "Solve-Tubes"]
                                  [width (quotient (* 5 new-width) 2)]
                                  [height (quotient (* 5 new-height) 2)]
                                  [x 0]
                                  [y 0]))
           (set! backing-bitmap (make-screen-bitmap (add1 (quotient (* 5 new-width) 2))
                                                    (add1 (quotient (* 5 new-height) 2))))
           (set! board-dc (send backing-bitmap make-dc))
           (set! board-canvas (new canvas%
                                   [parent board-frame]
                                   [paint-callback
                                    (λ (self dc)
                                      (send dc draw-bitmap backing-bitmap 0 0))]))

           (send board-dc set-scale 2 2)
           (show-board true)))
  
  (define (puzzle-setup puzz draw-option)
    (cond 
      [(not (symbol=? draw-option 'off))
       (resize-frame (* tube-width (get-numtubes-lib puzz)) (* tube-width (get-tubesize-lib puzz)))]))
  
 
  (define (show-board bool)
    (send board-frame show bool))

  (define (draw-board puzz draw-option)
    (cond
      [(not (symbol=? draw-option 'off))
       (local
         [(define tube-height (* tube-width (get-tubesize-lib puzz)))
          (define (draw puzz)
            (begin
              (send board-dc clear)
              (send board-dc set-pen "black" 1 'solid)
              (local
                ; Draw the tubes
                [(define (draw-tubes x y h w num)
                   (cond
                     [(zero? num) ] ; Done
                     [else (begin
                             (send board-dc set-pen "black" 1 'solid)
                             
                             (send board-dc draw-line x y x (+ y h))
                             (send board-dc draw-line x (+ y h) (+ x w) (+ y h))
                             (send board-dc draw-line (+ x w) y (+ x w) (+ y h))
                             
                             (draw-tubes (+ x w tube-spacing) y h w (sub1 num))
                             
                             
                             )
                           ]))]
                (draw-tubes 0 0 tube-height tube-width (get-numtubes-lib puzz)))
                 
              ;; Draw the balls in each tube
              (define (fill-one-tube tube x-coord y-coord board-dc)
                (cond
                  [(empty? tube)]
                  [else (begin
                          (send board-dc set-brush (colour-map (first tube)) 'solid)
                          (send board-dc set-pen "black" 1 'solid)
                          (send board-dc draw-ellipse x-coord y-coord ball-size ball-size)
                          (fill-one-tube (rest tube) x-coord (- y-coord tube-width) board-dc))]))
              
              ; Fill tubes
              (define (fill-tubes tubes x-coord)
                (cond
                  [(empty? tubes)]
                  [else (begin (fill-one-tube (reverse (first tubes)) x-coord (- tube-height tube-width) board-dc)
                               (fill-tubes (rest tubes) (+ tube-width tube-spacing x-coord)))]))
              
              
              (fill-tubes (get-tubes-lib puzz) 0)
              (send board-dc flush)
              (send board-canvas refresh-now)

              ; Add in a delay to make each step readable
              (cond 
                [(symbol=? 'slow draw-option) (sleep/yield slow-pause)]
                [(symbol=? 'fast draw-option) (sleep/yield fast-pause)]
                [else (sleep/yield normal-pause)])
              ))]
         (draw puzz))]))
  
  (provide draw-board puzzle-setup))
