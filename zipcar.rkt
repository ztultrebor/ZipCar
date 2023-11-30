;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname zipcar.rck) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; WorldState: distance of front of car from left side canvas

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

; WorldState -> IMG
; shows the car's location at a specific time
(check-expect (render 0) BACKDROP) ; checks 
(check-expect (render (+ WHEELGAP 50)) (place-image CAR 50 Y-OFFSET BACKDROP)) ; checks
(check-expect (render (+ CARLENGTH 200)) BACKDROP) ; checks 
(define (render p) (place-image CAR (- p WHEELGAP) Y-OFFSET BACKDROP))

; WorldState -> INT
; produce the car's new position after event clock tick
(check-expect (move 0) SPEED) ; checks
(define (move p) (+ p SPEED))

; WorldState -> BOOL
; quits when car completely exits stage left
(check-expect (finished? (+ WORLDWIDTH CARLENGTH SPEED)) #false) ; checks
(check-expect (finished? (+ WORLDWIDTH CARLENGTH SPEED 1)) #true) ; checks
(define (finished? p) (> p (+ WORLDWIDTH CARLENGTH SPEED)))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-tick move]
    [stop-when finished?])
  )


; actions!

(main -20)