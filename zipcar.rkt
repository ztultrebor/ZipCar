;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname zipcar) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A high-action sequence of a car vrooming to and fro.
; Click the scene to interact with it

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
(define CAR (underlay/xy FRAME WHEELRADIUS (* 2 WHEELRADIUS) CHASSIS))
(define DOPPELAUTO (overlay/xy CAR WORLDWIDTH 0 CAR))
(define BACKDROP (place-image TREE 150 11 CANVAS)) ; canvas with tree



;========================
; functions


(define (main ws)
  ; WorldState -> WorldState
  ; launches the program from some initial state
  ; the WorldState is just the ticking clock
  (big-bang ws
    [to-draw render]
    [on-tick add1]
    [on-mouse hyper]
    [stop-when finished?]
    )
  )


(define (render t) 
  ; N -> img
  ; shows the car's location at a specific time
  (place-image DOPPELAUTO (- (position t) WHEELGAP) Y-OFFSET BACKDROP))


(define (position t)
  ; N -> int
  ; produce new position of car's front end after clock tick
  (round (/ (+ (* WORLDWIDTH (+ 1 (sin (- (/ (* 2 pi SPEED t) WORLDWIDTH)
                                          (/ pi 2))))) CARLENGTH) 2)))
; p(t) = (w (1 + sin(2 pi v t / w - pi / 2)) + l) / 2
; p(0) = (w (1 + sin(- pi / 2)) + l) / 2 = (w (1 - 1) + l) / 2 = l / 2
; p(w / 2 v) = (w (1 + sin(2 pi v w / 2 v w - pi / 2)) + l) / 2
;            = (w (1 + sin(pi - pi / 2)) + l) / 2  = (w (1 + sin(pi / 2)) + l) / 2
;            = (w (1 +1) + l) / 2 = (2 w + l) / 2 = w + l / 2


(define (hyper t x-mouse y-mouse me)
  ; N N N String -> N
  ; move car center point to x-mouse on mouse click
  (cond
    [(not (string=? me "button-down")) t]
    [(> x-mouse (/ WORLDWIDTH 2))
     (inverse (+ (modulo (+ x-mouse (/ WORLDWIDTH 2)) (+ WORLDWIDTH 1))
                 WHEELGAP) 0 (/ WORLDWIDTH SPEED 4))]
    [else (inverse
           (+ (modulo (+ x-mouse (/ WORLDWIDTH 2)) (+ WORLDWIDTH 1)) WHEELGAP)
           (/ (* 3 WORLDWIDTH) SPEED 4) (/ WORLDWIDTH SPEED 2))]))


(define (finished? t) 
  ; WorldState -> BOOL
  ; quits after CYCLES cycles
  (>= (/ (* SPEED t) WORLDWIDTH) CYCLES))


(define (inverse x min-t max-t)
  ; (N, N, N) -> N
  ; get the value of t such that f(t) = x
  (cond
    [(< (abs (- (position (/ (+ min-t max-t) 2)) x)) 1/16)
     (round (/ (+ min-t max-t) 2))]
    [(> (position (/ (+ min-t max-t) 2)) x)
     (inverse x min-t (/ (+ min-t max-t) 2))]
    [else  (inverse x (/ (+ min-t max-t) 2) max-t)]))
; p(0) = (w (1 + sin(2 pi v 0 / w - pi / 2)) + l) / 2 = p(t) = (w (1 + sin(- pi / 2)) + l) / 2
;      = (w (1 - 1) + l) / 2 = l / 2
; p(w/4v) = (w (1 + sin(2 pi v (w / 4 v) / w - pi / 2)) + l) / 2 = (w (1 + sin(pi / 2 - pi / 2)) + l) / 2
;         = (w (1 + sin(0)) + l) / 2 = (w + l) / 2
; p(w/2v) = (w (1 + sin(2 pi v (w / 2 v) / w - pi / 2)) + l) / 2 = (w (1 + sin(pi - pi / 2)) + l) / 2
;         = (w (1 + sin(pi / 2)) + l) / 2 = (w (1 + 1) + l) / 2 = w + l / 2 
; p(3w/4v) = (w (1 + sin(2 pi v (3 w / 4 v) / w - pi / 2)) + l) / 2 = (w (1 + sin(3 pi / 2 - pi / 2)) + l) / 2
;         = (w (1 + sin(pi)) + l) / 2 = (w (1 - 0) + l) / 2 = (w + l) / 2



;============================
; checks

(check-expect (render 0) (place-image DOPPELAUTO 0 Y-OFFSET BACKDROP))
(check-expect (render (/ WORLDWIDTH  SPEED 2)) (place-image DOPPELAUTO 0 Y-OFFSET BACKDROP))
(check-expect (render (/ WORLDWIDTH  SPEED)) (place-image DOPPELAUTO 0 Y-OFFSET BACKDROP))
(check-expect (position 0) (/ CARLENGTH 2))
(check-expect (position (/ WORLDWIDTH SPEED 4)) (/ (+ WORLDWIDTH CARLENGTH) 2))
(check-expect (position (/ WORLDWIDTH SPEED 2)) (+ WORLDWIDTH (/ CARLENGTH 2)))
(check-expect (position (/ (* 3 WORLDWIDTH) SPEED 4)) (/ (+ WORLDWIDTH CARLENGTH) 2))
(check-expect (position (/ WORLDWIDTH SPEED)) (/ CARLENGTH 2))
(check-expect  (hyper 200/6 20 20 "enter") 200/6)
(check-expect (hyper 200/6 20 20 "move") 200/6)
(check-expect (hyper 200/6 200 20 "button-down")
              (inverse (+ (modulo (+ 200 (/ WORLDWIDTH 2)) WORLDWIDTH) WHEELGAP)
                       0 (/ WORLDWIDTH SPEED 4)))
(check-expect (hyper 200/6 205 20 "button-down")
              (inverse (+ (modulo (+ 205 (/ WORLDWIDTH 2)) WORLDWIDTH) WHEELGAP)
                       0 (/ WORLDWIDTH SPEED 4)))
(check-expect (finished? (/ (* WORLDWIDTH CYCLES) SPEED)) #true)
(check-expect (finished? (- (/ (* WORLDWIDTH CYCLES) SPEED) 1)) #false)
(check-within (inverse WHEELGAP 0 (/ WORLDWIDTH SPEED 2)) 0 1/2)
(check-within (inverse (+ WHEELGAP (/ WORLDWIDTH 2)) 0 (/ WORLDWIDTH SPEED 2))
              (/ WORLDWIDTH SPEED 4) 1/2)
(check-within (inverse (+ WHEELGAP WORLDWIDTH) 0 (/ WORLDWIDTH SPEED 2))
              (/ WORLDWIDTH SPEED 2) 1/2)
(check-within (inverse (+ WHEELGAP (/ WORLDWIDTH 2)) (/ WORLDWIDTH SPEED 2)
                       (/ WORLDWIDTH SPEED)) (/ (* 3 WORLDWIDTH) SPEED 4) 1/2)
(check-within (inverse WHEELGAP (/ WORLDWIDTH SPEED 2) (/ WORLDWIDTH SPEED))
              (/ WORLDWIDTH SPEED) 1/2)


;=======================
; actions!

(main 0)