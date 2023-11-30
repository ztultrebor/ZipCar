;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname zipcar) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; WorldState: clock ticks elapsed

; constants
(define WORLDWIDTH 200) ; horizontal world size
(define WORLDHEIGHT 25) ; vertical world size
(define WHEELRADIUS 4) ; 2pice that is the wheel circumference
(define WHEELGAP (* 4 WHEELRADIUS)) ; empty space between wheels
(define CARLENGTH (* 2 WHEELGAP))
(define Y-OFFSET (- WORLDHEIGHT (* 2 WHEELRADIUS))) ; y-pos of car's center
(define SPEED 3) ; pixels per tick

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
(define BACKDROP (place-image TREE 150 11 CANVAS)) ; canvas with tree

;functions
; WorldState -> INT
; produce new position of car's front end after clock tick
(check-within (position 0) CARLENGTH 1/1000000) ; checks
(check-within (position (/ WORLDWIDTH  SPEED 2)) WORLDWIDTH 1/1000000) ; checks
(define (position t) (/ (+ WORLDWIDTH CARLENGTH
                        (* (- WORLDWIDTH CARLENGTH)
                           (sin (- (/ (* 2 pi SPEED t) WORLDWIDTH) (/ pi 2)))))
                        2))

; WorldState -> IMG
; shows the car's location at a specific time
(check-expect (render 0) (place-image CAR WHEELGAP Y-OFFSET BACKDROP)) ; checks
(check-expect (render (/ WORLDWIDTH  SPEED 2)) (place-image CAR (- WORLDWIDTH WHEELGAP) Y-OFFSET BACKDROP)) ; checks
(define (render t) (place-image CAR (- (position t) WHEELGAP) Y-OFFSET BACKDROP))

; WorldState -> BOOL
; quits when car completely exits stage left
(check-expect (finished? (/ WORLDWIDTH SPEED)) #false) ; checks
(check-expect (finished? (/ (+ WORLDWIDTH CARLENGTH 1) SPEED)) #true) ; checks
(define (finished? t) (> (position t) (+ WORLDWIDTH CARLENGTH)))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-tick add1]
    [stop-when finished?]
    )
  )

; actions!

(main 1)