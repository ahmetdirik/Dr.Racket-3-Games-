;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname datatypebitmiş) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

;purpose:Collecting data structure types in a list
;contract:data-types ->list
;test
;func:
(define data-types '(string number boolean))
(check-expect data-types '(string number boolean))

;purpose:Creating examples within the previously specified data types
;contract:data-examples ->list
;test:bottom
;func
(define data-examples '((string "hello" "world" "racket")
                        (number 42 99 123)
                        (boolean #true #false)))
(check-expect data-examples '((string "hello" "world" "racket")
                              (number 42 99 123)
                              (boolean #true #false)))

;; Attributes:
;; - target(symbol): The current target type that the player needs to catch.
;; - player-pos(number): The x-coordinate of the player's position.
;; - falling-items(list): A list of falling items, each represented as a list.
;; - score(number): The player's current score.
;; - scene(string): The current scene of the game (e.g., "menu", "game", "game-over").
;; - leaderboardfile(string): The file used to store leaderboard data.

(define-struct world (target player-pos falling-items score scene leaderboardfile))

;purpose:define inital version of game
;contract:inital-world -> world
;;func
(define initial-world
  (make-world 
    'string            ;; Initial target data type
    200                ;; Initial player position
    '()                ;; No falling items initially
    0                  ;; Initial score
    "menu"            ;; Start in the menu scene
    "leaderboard.txt")) ;; Leaderboard file
(check-expect initial-world 
  (make-world 
    'string            ;; Initial target data type
    200                ;; Initial player position
    '()                ;; No falling items initially
    0                  ;; Initial score
    "menu"             ;; Start in the menu scene
    "leaderboard.txt")) ;; Leaderboard file


;; Purpose: Create a new falling item with a random value, type, and position.
;; Contract: make-falling-item: -> list
;;func
(define (make-falling-item)
  (let* ([type (list-ref data-types (random (length data-types)))]
         [examples (rest (assoc type data-examples))]
         [value (list-ref examples (random (length examples)))] )
    (list value type (random 400) 0))) ;; New item: [value, type, x, y]


;; Purpose: Move falling items down and remove those that exceed the y boundary.
;; Contract: update-falling-items: items(list) -> list
;;test: in final part
;;func
(define (update-falling-items items)
  (filter (lambda (item) (< (fourth item) 400)) ;; Remove items below y=400
          (map (lambda (item) 
                 (list (first item) 
                       (second item) 
                       (third item) 
                       (+ (fourth item) 2))) ;; Increment y by 2 pixels
               items)))
;; Test
(check-expect 
 (update-falling-items 
  '((5 'number 100 398) ;; Item 1
    (10 'string 200 400) ;; Item 2, out of bounds
    (15 'boolean 300 395))) ;; Item 3
  (list
 (list
  15
  (list 'quote 'boolean)
  300
  397))) ;; Updated Item 3

;; Purpose: Generate a random target that is different from the current target.
;; Contract: random-target: current-target(symbol) -> symbol
;test:in final part
;;func
(define (random-target current-target)
  (let ([choices (remove current-target data-types)]) ; Mevcut hedefi kaldır
    (list-ref choices (random (length choices))))) ; Geriye kalanlardan birini seç


;; Purpose: Check for collisions between the player and falling items.
;; Contract: check-collision: w(world) -> list
;;test: in final part
;;func:

(define (check-collision w)
  (let ([player-x (world-player-pos w)]
        [items (world-falling-items w)]
        [target (world-target w)])
    (foldl (lambda (item result)
             (if (and (< (abs (- (third item) player-x)) 20) ;; X-axis tolerance
                      (>= (fourth item) 370) (<= (fourth item) 390)) ;; Y-axis range
                 (if (string=? (symbol->string (second item)) (symbol->string target))
                     ;; Correct type collision
                     (cons (list (+ (world-score w) 10) #true item) result)
                     ;; Incorrect type collision, game over
                     (cons (list (world-score w) #false 'game-over) result))
                 result))
           '() items)))


;; Write score to leaderboard
;; Purpose: Write the current score to the leaderboard file.
;; Contract: write-score: w(world) -> void
;; test: in final part
;; func:
(define (write-score w)
  (let ([file-content (if (file-exists? (world-leaderboardfile w))
                          (read-file (world-leaderboardfile w)) ;; Read existing content
                          "")]) ;; Empty content if file doesn't exist
    (write-file (world-leaderboardfile w)
                (string-append file-content ;; Preserve existing content
                               "Score: " (number->string (floor (world-score w))) " pts.\n"))))
;; Test: Simulate file writing and validate the output
(define test-world
  (make-world 'string 200 '() 45 "game" "test-leaderboard.txt"))

;; Simulate a test leaderboard file
(write-file "test-leaderboard.txt" "Previous Score: 100 pts.\n")

;; Write the score of test-world to the leaderboard file
(write-score test-world)

;; Read back the file content and check its correctness
(check-expect (read-file "test-leaderboard.txt")
              "Previous Score: 100 pts.Score: 45 pts.")

(define max-score 100) ;; Score required to end the game

;; Tick handler for the world
;; Purpose: Update the game state with each tick of the game loop.
;; Contract: world-tick: w(world) -> world
;; test: in final part
;func:


(define (world-tick w)
  ;; If in game-over or leaderboard scenes, do nothing
  (if (or (string=? (world-scene w) "game-over")
          (string=? (world-scene w) "leaderboard")
          (string=? (world-scene w) "menu"))
      w
      (let* ([collision-result (check-collision w)]
             [score (if (not (empty? collision-result))
                        (first (first collision-result)) ;; New score
                        (world-score w))]
             ;; Check for game-over
             [game-over (or (>= score max-score)
                           (and (not (empty? collision-result))
                                (equal? (third (first collision-result)) 'game-over)))]
             [new-target (if (not (empty? collision-result))
                           (random-target (world-target w)) ;; New target
                           (world-target w))]
             ;; Remove collided item
             [updated-items
              (if (not (empty? collision-result))
                  (remove (third (first collision-result)) (world-falling-items w))
                  (world-falling-items w))]
             ;; Create new falling items
             [maybe-new-item
              (if (< (random 60) 1) 
                  (list (make-falling-item))
                  '())]
             [all-items (append updated-items maybe-new-item)]) ;; Update list
        ;;if game-over stop
        (if game-over
            (begin
              (write-score w) ;; Write score
              (make-world (world-target w) (world-player-pos w) '() score "game-over" (world-leaderboardfile w)))
            ;;update normally
            (make-world
             new-target
             (world-player-pos w)
             (update-falling-items all-items)
             score
             "game"
             (world-leaderboardfile w))))))
;test



;; Handle key input
;; Purpose: Handle player input for key presses.
;; Contract: handle-key: w(world) key(string) -> world
;; test: in final part
;func:

(define (handle-key w key)
  (cond [(string=? key "left")
         (make-world (world-target w)
                     (max 0 (- (world-player-pos w) 10)) ;; Move left
                     (world-falling-items w)
                     (world-score w)
                     (world-scene w)
                     (world-leaderboardfile w))]
        [(string=? key "right")
         (make-world (world-target w)
                     (min 400 (+ (world-player-pos w) 10)) ;; Move right
                     (world-falling-items w)
                     (world-score w)
                     (world-scene w)
                     (world-leaderboardfile w))]
        [(string=? key "r")
         (make-world 'string 200 '() 0 "menu" (world-leaderboardfile w))]
        [else w]))
;; Başlangıç dünyası
(define test-world3
  (make-world 
   'string                 ;; Hedef türü
   200                     ;; Oyuncunun başlangıç pozisyonu
   '((5 'string 200 380))  ;; Düşen nesneler
   10                      ;; Başlangıç puanı
   "game"                  ;; Oyun sahnesi
   "leaderboard.txt"))     ;; Liderlik tablosu dosyası

;; Sol tuşa basıldığında, oyuncunun pozisyonunun 10 birim sola kayması gerekir
(define expected-world-left
  (make-world 
   'string
   190                     ;; Oyuncu sola kaydırıldı (200 - 10)
   '((5 'string 200 380))
   10
   "game"
   "leaderboard.txt"))

(check-expect (handle-key test-world3 "left") expected-world-left)
;; Handle mouse input
;; Purpose: Handle player input for mouse interactions.
;; Contract: handle-mouse: w(world) x(number) y(number) event(string) -> world
;; test: in final part
;func:
(define (handle-mouse w x y event)
  (cond
    ;; Handle menu buttons
    [(and (string=? event "button-down")
          (string=? (world-scene w) "menu"))
     (cond [(and (<= 150 x 250) (<= 100 y 140)) ;; Start button
            (make-world (world-target w) (world-player-pos w) '() 0 "game" (world-leaderboardfile w))]
           [(and (<= 150 x 250) (<= 160 y 200)) ;; Leaderboard button
            (make-world (world-target w) (world-player-pos w) '() (world-score w) "leaderboard" (world-leaderboardfile w))]
           [else w])]
    ;; Handle Leaderboard screen
    [(and (string=? event "button-down")
          (string=? (world-scene w) "leaderboard"))
     (make-world (world-target w) (world-player-pos w) '() (world-score w) "menu" (world-leaderboardfile w))]
    ;; Handle Game-Over screen
    [(and (string=? event "button-down")
          (string=? (world-scene w) "game-over")
          (<= 150 x 250) (<= 230 y 270)) ;; Return to menu button
     (make-world (world-target w) (world-player-pos w) '() (world-score w) "menu" (world-leaderboardfile w))]
    [else w]))

;; Menü ekranında fare "Start" butonuna tıklandığında oyun başlar
(define menu-world
  (make-world 
   'string 
   200 
   '() 
   0 
   "menu" 
   "leaderboard.txt"))

(define expected-game-world
  (make-world 
   'string 
   200 
   '() 
   0 
   "game" 
   "leaderboard.txt"))
(check-expect (handle-mouse menu-world 200 120 "button-down") expected-game-world)
;; Menü ekranında fare "Leaderboard" butonuna tıklandığında liderlik tablosu açılır
(define expected-leaderboard-world
  (make-world 
   'string 
   200 
   '() 
   0 
   "leaderboard" 
   "leaderboard.txt"))

;; Menüde Leaderboard butonuna tıklama (x: 200, y: 180)
(check-expect (handle-mouse menu-world 200 180 "button-down") expected-leaderboard-world)

;; Liderlik tablosunda fare tıklamasıyla menüye dönülür
(define leaderboard-world
  (make-world 
   'string 
   200 
   '() 
   0 
   "leaderboard" 
   "leaderboard.txt"))

(check-expect (handle-mouse leaderboard-world 200 200 "button-down") menu-world)

;; Oyun bitti ekranında "Return to Menu" butonuna tıklama
(define game-over-world
  (make-world 
   'string 
   200 
   '() 
   50 
   "game-over" 
   "leaderboard.txt"))

(define expected-menu-world
  (make-world 
   'string 
   200 
   '() 
   50 
   "menu" 
   "leaderboard.txt"))

;; Return to Menu butonuna tıklama (x: 200, y: 250)
(check-expect (handle-mouse game-over-world 200 250 "button-down") expected-menu-world)
(check-expect (handle-mouse menu-world 50 50 "button-down") menu-world)
;; Draw the world
;; Purpose: Draw the current state of the game on the screen.
;; Contract: draw-world: w(world) -> image
;; test: in final part
;func:
(define (draw-world w)
  (cond
    ;; Menu scene
    [(string=? (world-scene w) "menu")
     (place-image
      (text "Welcome to the Game!" 20 "blue")
      200 50
      (place-image
       (rectangle 120 55 "solid" "green") ;; Start button
       200 130
       (place-image
        (text "Start" 18 "blue")
        200 90
        (place-image
         (rectangle 120 55 "solid" "green") ;; Leaderboard button
         200 220
         (place-image
          (text "Leaderboard" 18 "blue")
          200 180
          (empty-scene 400 400))))))]
    ;; Leaderboard scene
    [(string=? (world-scene w) "leaderboard")
     (place-image
      (text "Leaderboard" 20 "red")
      200 20
      (place-image
       (text (read-file (world-leaderboardfile w)) 14 "black")
       200 200
       (empty-scene 400 400))) ]
    ;; Game-over scene
    [(string=? (world-scene w) "game-over")
     (place-image
      (text (string-append "Game Over! Your Score: " (number->string (world-score w))) 20 "red")
      200 150
      (place-image
       (rectangle 100 40 "solid" "blue") ;; Return to menu button background
       200 250
       (place-image
        (text "Return Menu" 18 "blue") ;; Button text
        200 220
        (empty-scene 400 400))))]
    ;; Game scene
    [else
     (place-image
      (text (string-append "Target: " (symbol->string (world-target w))) 14 "red")
      200 20
      (place-image
       (text (string-append "Score: " (number->string (world-score w))) 14 "green")
       50 20
       (place-image
        (circle 20 "solid" "blue") ;; Player
        (world-player-pos w) 380
        (foldl (lambda (item img)
                 (place-image (cond
                                ((symbol? (first item)) (text (symbol->string (first item)) 12 "black"))
                                ((number? (first item)) (text (number->string (first item)) 12 "black"))
                                ((boolean? (first item)) (text (boolean->string (first item)) 12 "black"))
                                (else (text (first item) 12 "black")))
                           (third item) (fourth item) img))
               (empty-scene 400 400) (world-falling-items w)))))]))

;; Beklenen resim
(define expected-menu-image
  (place-image
   (text "Welcome to the Game!" 20 "blue")
   200 50
   (place-image
    (rectangle 120 55 "solid" "green") ;; Start button
    200 130
    (place-image
     (text "Start" 18 "blue")
     200 90
     (place-image
      (rectangle 120 55 "solid" "green") ;; Leaderboard button
      200 220
      (place-image
       (text "Leaderboard" 18 "blue")
       200 180
       (empty-scene 400 400)))))))
(check-expect (draw-world menu-world) expected-menu-image)

;; Start the game
(big-bang initial-world
          (on-tick world-tick)
          (on-key handle-key)
          (on-mouse handle-mouse)
          (to-draw draw-world))


;tests

