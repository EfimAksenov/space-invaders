;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED -10)

(define HIT-RANGE 10)

(define INVADE-RATE 0.98)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; Functions:

;; Game -> Game
;; start the world with initial state s
(define (main s)
  (big-bang s
    (on-tick nextState)
    (to-draw drawWorld)
    (on-key keyHandle)
    (stop-when loose?)))

;; Game -> Game
;; produses next state of the game.
(check-expect (nextState (make-game empty (cons M1 empty) T0))
                         (make-game empty (nextMissilesState (cons M1 empty)) (nextTankState T0)))
(check-expect (nextState (make-game (cons I1 empty) empty T0))
                         (make-game (nextInvadersState (list I1)) empty (nextTankState T0)))
(check-expect (nextState (make-game empty empty T0))
              (make-game empty empty (nextTankState T0)))

;(define (nextState game) G0)

(define (nextState game)
  (checkEnvaderMissileCollision (make-game (nextInvadersState (createInvaider (game-invaders game)))
                                           (nextMissilesState (game-missiles game))
                                           (nextTankState     (game-tank game)))))

;; Game -> Game
;; exclude from game envader and missile those collide

(define (checkEnvaderMissileCollision g)
  (make-game (removeCollisionInvaders (game-invaders g) (game-missiles g))
             (removeCollisionMissiles (game-invaders g) (game-missiles g))
             (game-tank g)))

;; LOI LOM -> LOI
;; exclude missiles those collide with envaders
(check-expect (removeCollisionInvaders (list I1 I2) (list M1 M2))
              (list I2))
              
(define (removeCollisionInvaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (invaderCollide? (first loi) lom)
             (removeCollisionInvaders (rest loi) lom)
             (cons (first loi) (removeCollisionInvaders (rest loi) lom)))]))

;; Invador LOM -> Boolean
;; true if invador collide with missiles
(check-expect (invaderCollide? I1 (list M1 M2)) true)
(check-expect (invaderCollide? I2 (list M1 M2)) false)

(define (invaderCollide? i lom)
  (cond [(empty? lom) false]
        [else
         (if (collide? i (first lom))
             true
             (invaderCollide? i (rest lom)))]))

;; LOI LOM -> LOM
;; exclude envaders those collide with missiles
(check-expect (removeCollisionMissiles (list I1 I2) (list M1 M2))
              (list M1))
              
(define (removeCollisionMissiles loi lom)
  (cond [(empty? lom) empty]
        [else
         (if (missileCollide? (first lom) loi)
             (removeCollisionMissiles loi (rest lom))
             (cons (first lom) (removeCollisionMissiles loi (rest lom))))]))

;; LOI Missile -> Boolean
;; true if missile collide with invaders
(check-expect (missileCollide? M2 (list I1 I2)) true)
(check-expect (missileCollide? M1 (list I1 I2)) false)

(define (missileCollide? m loi)
  (cond [(empty? loi) false]
        [else
         (if (collide? (first loi) m)
             true
             (missileCollide? m (rest loi)))]))

;; Invader Missile -> Boolean
;; true if they collide
(check-expect (collide? I1 M1) false)
(check-expect (collide? I1 (make-missile (invader-x I1) (+ (invader-y I1) 20))) false)
(check-expect (collide? I1 M2) true)

(define (collide? i m)
  (if (>= HIT-RANGE (sqrt (+ (expt (- (invader-x i) (missile-x m)) 2) (expt (- (invader-y i) (missile-y m)) 2))))
      true
      false))

;; LOI -> LOI
;; random chanse to create invaidor at next tick
;(check-expect (createInvaider loi))
(define (createInvaider loi)
  (if (> (random 100) (* INVADE-RATE 100))
      (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi)
      loi))

;; LOI -> LOI
;; produces next state of invaders
(check-expect (nextInvadersState (list I1))
              (cons (make-invader (+ (invader-x I1) (invader-dx I1))
                                                         (+ (invader-y I1) INVADER-Y-SPEED)
                                                         (invader-dx I1)) empty))
(check-expect (nextInvadersState (list I1 I2))
              (list (make-invader (+ (invader-x I1) (invader-dx I1))
                                                         (+ (invader-y I1) INVADER-Y-SPEED)
                                                         (invader-dx I1))
                    (make-invader (+ (invader-x I2) (invader-dx I2))
                                                         (+ (invader-y I2) INVADER-Y-SPEED)
                                                         (invader-dx I2))))

;; (define (nextInvadersState loi) empty)

(define (nextInvadersState loi)
  (cond [(empty? loi) empty]
        [else
         (cons (calculatePosition (first loi)) (nextInvadersState (rest loi)))]))

;; Invader -> Invader
;; calculate new position of the given invader
(check-expect (calculatePosition (make-invader 150   150  10)) (make-invader (+ 150    10) (+ 150 INVADER-Y-SPEED)  10))
(check-expect (calculatePosition (make-invader 150   150 -10)) (make-invader (+ 150   -10) (+ 150 INVADER-Y-SPEED) -10))
(check-expect (calculatePosition (make-invader WIDTH 150  10)) (make-invader (+ WIDTH -10) (+ 150 INVADER-Y-SPEED) -10))
(check-expect (calculatePosition (make-invader 0     150 -10)) (make-invader (+ 0      10) (+ 150 INVADER-Y-SPEED)  10))

;; (define (calculatePosition i) I1)

(define (calculatePosition i)
  (make-invader (+ (invader-x i) (calculateDx i))
                (+ (invader-y i) INVADER-Y-SPEED)
                (calculateDx i)))

;; Invader -> Number
;; calculate next dx of the given invader
(check-expect (calculateDx (make-invader 150   150  10))  10)
(check-expect (calculateDx (make-invader WIDTH 150  10)) -10)
(check-expect (calculateDx (make-invader 0     150  -10)) 10)

;;(define (calculateDx i) 0)

(define (calculateDx i)
  (if (or (> (+ (invader-x i) (invader-dx i)) WIDTH) (< (+ (invader-x i) (invader-dx i)) 0))
      (- (invader-dx i))
      (invader-dx i)))

;; LOM -> LOM
;; produces next state of missiles
(check-expect (nextMissilesState (list (make-missile 150 150)))
              (list (make-missile 150 (+ 150 MISSILE-SPEED))))
(check-expect (nextMissilesState (list (make-missile 150 150)
                                       (make-missile 100 150)))
              (list (make-missile 150 (+ 150 MISSILE-SPEED))
                    (make-missile 100 (+ 150 MISSILE-SPEED))))

;; (define (nextMissilesState lom) empty)

(define (nextMissilesState lom)
  (cond [(empty? lom) empty]
        [else
         (if (missileVisible? (first lom))
             (cons (calculateMissilePosition (first lom)) (nextMissilesState (rest lom)))
             (nextMissilesState (rest lom)))]))

;; Missile -> Missile
;; calculate missile next position
(check-expect (calculateMissilePosition (make-missile 150 150))
              (make-missile 150 (+ 150 MISSILE-SPEED)))
(check-expect (calculateMissilePosition (make-missile 150 HEIGHT))
              (make-missile 150 (+ HEIGHT MISSILE-SPEED)))

;; (define (calculateMissilePosition m) (make-missile 0 0))

(define (calculateMissilePosition m)
  (make-missile (missile-x m) (+ (missile-y m) MISSILE-SPEED)))

;; Missile -> Boolean
;; true if missile is in render area
(check-expect (missileVisible? (make-missile 150 150)) true)
(check-expect (missileVisible? (make-missile 150 (- 0 100))) false)

;; (define (missileVisible? m) false)

(define (missileVisible? m)
  (if (> (missile-y m) 0)
      true
      false))

;; Tank -> Tank
;; produces next state of tank
(check-expect (nextTankState (make-tank 150    1))   (make-tank (+ 150   TANK-SPEED)  1))
(check-expect (nextTankState (make-tank 150   -1))   (make-tank (- 150 TANK-SPEED)   -1))
(check-expect (nextTankState (make-tank WIDTH  1))   (make-tank (- WIDTH TANK-SPEED) -1))
(check-expect (nextTankState (make-tank 0     -1))   (make-tank (+ 0 TANK-SPEED)      1))

;; (define (nextTankState t) T0)

(define (nextTankState t)
  (make-tank (+ (tank-x t) (* TANK-SPEED (calculateDir t))) (calculateDir t)))

;; Tank -> Number
;; calculate next dir of the tank
(check-expect (calculateDir (make-tank 150   1))  1)
(check-expect (calculateDir (make-tank WIDTH 1)) -1)
(check-expect (calculateDir (make-tank 0    -1))  1)

;;(define (calculateDir t) 1)

(define (calculateDir t)
  (if (or (> (+ (tank-x t) (* TANK-SPEED (tank-dir t))) WIDTH) (< (+ (tank-x t) (* TANK-SPEED (tank-dir t))) 0))
      (- (tank-dir t))
      (tank-dir t)))

;; Game -> Image
;; render the current state of the world
;(check-expect (drawWorld G0))
(check-expect (drawWorld G3)
              (drawInvaders (game-invaders G3) (drawMissiles (game-missiles G3) (drawTank (game-tank G3) BACKGROUND))))

;; (define (drawWorld game) BACKGROUND)

(define (drawWorld game)
  (drawInvaders (game-invaders game) (drawMissiles (game-missiles game) (drawTank (game-tank game) BACKGROUND))))

;; Tank Image -> Image
;; Drow tank in given image
(check-expect (drawTank (make-tank (/ WIDTH 2) 1) BACKGROUND)
              (place-image TANK (/ WIDTH 2) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))

;;(define (drawTank t i) BACKGROUND)

(define (drawTank t i)
  (place-image TANK (tank-x t) (- HEIGHT (/ (image-height TANK) 2)) i))

;; LOM Image -> Image
;; draw missiles in given image
(check-expect (drawMissiles (list M1 M2) BACKGROUND)
              (place-image MISSILE (missile-x M1) (missile-y M1)
                           (place-image MISSILE (missile-x M2) (missile-y M2) BACKGROUND)))

;; (define (drawMissiles lom i) BACKGROUND)

(define (drawMissiles lom i)
  (cond [(empty? lom) i]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (drawMissiles (rest lom) i))]))

;; LOI Image -> Image
;; draw invaders in given image
(check-expect (drawInvaders (list I1 I2) BACKGROUND)
              (place-image INVADER (invader-x I1) (invader-y I1)
                           (place-image INVADER (invader-x I2) (invader-y I2) BACKGROUND)))

;; (define (drawInvaders loi i) BACKGROUND)

(define (drawInvaders loi i)
  (cond [(empty? loi) i]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) (drawInvaders (rest loi) i))]))

;; Game Key -> Game
;; left key changes tank direction to -1
;; right key changes tank direction to 1
;; space shoot new missile
(check-expect (keyHandle (make-game empty empty (make-tank 150  1)) "left")
              (make-game empty empty (make-tank 150 -1)))
(check-expect (keyHandle (make-game empty empty (make-tank 150 -1)) "left")
              (make-game empty empty (make-tank 150 -1)))
(check-expect (keyHandle (make-game empty empty (make-tank 150  1)) "right")
              (make-game empty empty (make-tank 150  1)))
(check-expect (keyHandle (make-game empty empty (make-tank 150 -1)) "right")
              (make-game empty empty (make-tank 150  1)))
(check-expect (keyHandle (make-game empty empty (make-tank 150 1)) " ")
              (make-game empty (shoot empty (make-tank 150  1)) (make-tank 150  1)))

(define (keyHandle g a-key)
  (cond
    [(key=? a-key "left")  (make-game (game-invaders g) (game-missiles g) (changeTankDir (game-tank g) -1))]
    [(key=? a-key "right") (make-game (game-invaders g) (game-missiles g) (changeTankDir (game-tank g)  1))]
    [(key=? a-key " ")     (make-game (game-invaders g) (shoot (game-missiles g) (game-tank g)) (game-tank g))]))

;; Tank Number -> Tank
;; change tank dir to n
(check-expect (changeTankDir (make-tank 150  1)  1) (make-tank 150  1))
(check-expect (changeTankDir (make-tank 150  1) -1) (make-tank 150 -1))
(check-expect (changeTankDir (make-tank 150 -1)  1) (make-tank 150  1))
(check-expect (changeTankDir (make-tank 150 -1) -1) (make-tank 150 -1))

(define (changeTankDir t n)
  (make-tank (tank-x t) n))

;; LOM Tank -> LOM
;; create new missile from the center of the tank
(check-expect (shoot (list M1) T0)
              (cons (make-missile (tank-x T0) (- HEIGHT (image-height TANK))) (list M1)))

(define (shoot lom t)
  (cons (make-missile (tank-x t) (- HEIGHT (image-height TANK))) lom))

;; Game -> Boolean
;; true if player loose
(check-expect (loose? G2) false)
(check-expect (loose? G3) true)

(define (loose? g)
  (if (invadersWin? (game-invaders g))
      true
      false))

;; LOI -> Boolean
;; true if one of invaders reach the bootom
(check-expect (invadersWin? (list (make-invader 150 150 10))) false)
(check-expect (invadersWin? (list (make-invader 150 HEIGHT 10))) true)

(define (invadersWin? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (invadersWin? (rest loi)))]))