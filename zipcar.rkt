;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname zipcar) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; constants
(define WORLDWIDTH 200) ; horizontal world size
(define WORLDHEIGHT 25) ; vertical world size
(define WHEELRADIUS 4) ; 2pice that is the wheel circumference
(define WHEELGAP (* 4 WHEELRADIUS)) ; empty space between wheels
(define CARLENGTH (* 2 WHEELGAP))
(define Y-OFFSET (- WORLDHEIGHT (* 2 WHEELRADIUS))) ; y-pos of car's center
(define SPEED 3) ; pixels per tick
(define CYCLES 3); stops after this many

(define CANVAS (empty-scene WORLDWIDTH WORLDHEIGHT))
(define BOUGHS (circle 8 "solid" "green"))
(define TRUNK (rectangle 4 10 "solid" "brown"))
(define TREE (overlay/xy BOUGHS 6 16 TRUNK))
(define WHEEL (circle WHEELRADIUS "solid" "black"))
(define CHASSIS (overlay/xy WHEEL WHEELGAP 0 WHEEL)) ; wheels only
(define PANELS (rectangle CARLENGTH (* 2 WHEELRADIUS) "solid" "red"))
(define CABIN (rectangle WHEELGAP WHEELRADIUS "solid" "red"))
(define FRAME (above CABIN PANELS)) ; pretty parts
(define CAR (underlay/xy FRAME WHEELRADIUS (* 2 WHEELRADIUS) CHASSIS)) ; the whole car
(define DOPPELAUTO (overlay/xy CAR WORLDWIDTH 0 CAR))
(define BACKDROP (place-image TREE 150 11 CANVAS)) ; canvas with tree


;functions
; WorldState: clock ticks elapsed

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-tick add1]
    [on-mouse hyper]
    [stop-when finished?]
    )
  )

; WorldState -> IMG
; shows the car's location at a specific time
(check-expect (render 0) (place-image DOPPELAUTO 0 Y-OFFSET BACKDROP)) ; checks
(check-expect (render (/ WORLDWIDTH  SPEED 2)) (place-image DOPPELAUTO 0 Y-OFFSET BACKDROP)) ; checks
(check-expect (render (/ WORLDWIDTH  SPEED)) (place-image DOPPELAUTO 0 Y-OFFSET BACKDROP)) ; checks
(define (render t) (place-image DOPPELAUTO (- (position t) WHEELGAP) Y-OFFSET BACKDROP))

; WorldState -> INT
; produce new position of car's front end after clock tick
; p(t) = (w (1 + sin(2 pi v t / w - pi / 2)) + l) / 2
; p(0) = (w (1 + sin(- pi / 2)) + l) / 2 = (w (1 - 1) + l) / 2 = l / 2
; p(w / 2 v) = (w (1 + sin(2 pi v w / 2 v w - pi / 2)) + l) / 2
;            = (w (1 + sin(pi - pi / 2)) + l) / 2  = (w (1 + sin(pi / 2)) + l) / 2
;            = (w (1 +1) + l) / 2 = (2 w + l) / 2 = w + l / 2
(check-expect (position 0) (/ CARLENGTH 2)) ; checks
(check-expect (position (/ WORLDWIDTH SPEED 4)) (/ (+ WORLDWIDTH CARLENGTH) 2)) ; checks
(check-expect (position (/ WORLDWIDTH SPEED 2)) (+ WORLDWIDTH (/ CARLENGTH 2))) ; checks
(check-expect (position (/ (* 3 WORLDWIDTH) SPEED 4)) (/ (+ WORLDWIDTH CARLENGTH) 2)) ; checks
(check-expect (position (/ WORLDWIDTH SPEED)) (/ CARLENGTH 2)) ; checks
(define (position t) (round (/ (+ (* WORLDWIDTH (+ 1 (sin (- (/ (* 2 pi SPEED t) WORLDWIDTH)
                                                             (/ pi 2))))) CARLENGTH) 2)))

; WorldState Number Number String -> WorldState
; move car center point to x-mouse on mouse click
(check-expect  (hyper 200/6 20 20 "enter") 200/6) ; checks
(check-expect (hyper 200/6 20 20 "move") 200/6)  ; checks
(check-expect (hyper 200/6 200 20 "button-down") (inverse (+ (modulo (+ 200 (/ WORLDWIDTH 2)) WORLDWIDTH) WHEELGAP) 0 (/ WORLDWIDTH SPEED 4)))  ; checks
(check-expect (hyper 200/6 205 20 "button-down") (inverse (+ (modulo (+ 205 (/ WORLDWIDTH 2)) WORLDWIDTH) WHEELGAP) 0 (/ WORLDWIDTH SPEED 4)))  ; checks
(define (hyper t x-mouse y-mouse me)
  (cond
    [(not (string=? me "button-down")) t]
    [(> x-mouse (/ WORLDWIDTH 2))
     (inverse (+ (modulo (+ x-mouse (/ WORLDWIDTH 2)) (+ WORLDWIDTH 1))
                 WHEELGAP) 0 (/ WORLDWIDTH SPEED 4))]
    [else
     (inverse (+ (modulo (+ x-mouse (/ WORLDWIDTH 2)) (+ WORLDWIDTH 1))
                 WHEELGAP) (/ (* 3 WORLDWIDTH) SPEED 4) (/ WORLDWIDTH SPEED 2))]
    ))

; WorldState -> BOOL
; quits after CYCLES cycles
(check-expect (finished? (/ (* WORLDWIDTH CYCLES) SPEED)) #true) ; checks
(check-expect (finished? (- (/ (* WORLDWIDTH CYCLES) SPEED) 1)) #false) ; checks
(define (finished? t) (>= (/ (* SPEED t) WORLDWIDTH) CYCLES))

; (NUMBER, WorldState, WorldState) -> WorldState
; get the value of t such that f(t) = x
; p(0) = (w (1 + sin(2 pi v 0 / w - pi / 2)) + l) / 2 = p(t) = (w (1 + sin(- pi / 2)) + l) / 2
;      = (w (1 - 1) + l) / 2 = l / 2
; p(w/4v) = (w (1 + sin(2 pi v (w / 4 v) / w - pi / 2)) + l) / 2 = (w (1 + sin(pi / 2 - pi / 2)) + l) / 2
;         = (w (1 + sin(0)) + l) / 2 = (w + l) / 2
; p(w/2v) = (w (1 + sin(2 pi v (w / 2 v) / w - pi / 2)) + l) / 2 = (w (1 + sin(pi - pi / 2)) + l) / 2
;         = (w (1 + sin(pi / 2)) + l) / 2 = (w (1 + 1) + l) / 2 = w + l / 2 
; p(3w/4v) = (w (1 + sin(2 pi v (3 w / 4 v) / w - pi / 2)) + l) / 2 = (w (1 + sin(3 pi / 2 - pi / 2)) + l) / 2
;         = (w (1 + sin(pi)) + l) / 2 = (w (1 - 0) + l) / 2 = (w + l) / 2
(check-within (inverse WHEELGAP 0 (/ WORLDWIDTH SPEED 2)) 0 1/2) ; checks
(check-within (inverse (+ WHEELGAP (/ WORLDWIDTH 2)) 0 (/ WORLDWIDTH SPEED 2)) (/ WORLDWIDTH SPEED 4) 1/2) ; checks
(check-within (inverse (+ WHEELGAP WORLDWIDTH) 0 (/ WORLDWIDTH SPEED 2)) (/ WORLDWIDTH SPEED 2) 1/2) ; checks
(check-within (inverse (+ WHEELGAP (/ WORLDWIDTH 2)) (/ WORLDWIDTH SPEED 2) (/ WORLDWIDTH SPEED)) (/ (* 3 WORLDWIDTH) SPEED 4) 1/2) ; checks
(check-within (inverse WHEELGAP (/ WORLDWIDTH SPEED 2) (/ WORLDWIDTH SPEED)) (/ WORLDWIDTH SPEED) 1/2) ; checks
(define (inverse x min-t max-t)
  (cond
    [(< (abs (- (position (/ (+ min-t max-t) 2)) x)) 1/16)
     (round (/ (+ min-t max-t) 2))]
    [(> (position (/ (+ min-t max-t) 2)) x)
     (inverse x min-t (/ (+ min-t max-t) 2))]
    [else  (inverse x (/ (+ min-t max-t) 2) max-t)]))


; actions!

(main 0)