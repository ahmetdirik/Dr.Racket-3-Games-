;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |drawing house |) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; GAME3:LOGIC OF ALGORITHM ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;
;; STRUCTURES ;;
;;;;;;;;;;;;;;;;

;structure: pos
;attribute: x (number)
;attribute: y (number)
(define-struct pos (x y))


;structure: button
;attribute:im (image)
;attribute:pos(pos)
;attribute:text(string)
(define-struct button (im pos text))


; structure: menu
; attribute: start(button): button for starting the game
; attribute: leaderBoard(button): button for showing the leaderboard
(define-struct menu (start leaderBoard))


; structure: leaderBoard
; attribute: btnReturn(button): button for returning the menu
(define-struct leaderBoard (btnReturn))

; structure: game-over-scene
; attribute: btnReturn(button): button for returning the menu
(define-struct game-over-scene (btnReturn))

;structure:game3
;attribute:menu (menu)
;attribute:currentScene(string)
;attribute:button1(button)
;attribute:button2(button)
;attribute:button3(button)
;attribute:button4(button)
;attribute:button5(button)
;attribute:current-step(number)
;attribue:time(number)
;attribute:game-over (boolean)
;attribute:leaderBoard(leaderBoard)
;attribute:leaderboardfile(string)
;attribute:game-over-scene(game-over-scene)
(define-struct game3 (menu currentScene button1 button2 button3 button4 button5 current-step time game-over leaderBoard leaderboardfile game-over-scene))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NECESSARY BUTTONS AND THE BACKGROUND ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; background                
(define background-color "lightblue")
(define background
  (overlay
   (rectangle 1260 700 "solid" background-color)
   (empty-scene 1260 700)))

; objects
(define grass (rectangle 200 50 "solid" "green"))
(define mainstructure (square 100 "solid" "brown"))
(define door (rectangle 20 40 "solid" "black"))
(define pencere1 (square 20 "solid" "blue"))
(define pencereboslugu (square 20 "solid" (make-color 0 0 0 1)))
(define windows (beside pencere1 pencereboslugu pencere1))
(define roof (triangle 100 "solid" "red"))

; buttons
(define button1 (make-button grass (make-pos 470 250) "grass"))
(define button2 (make-button mainstructure (make-pos 250 250) "mainstructure"))
(define button3 (make-button door (make-pos 650 250) "door"))
(define button4 (make-button windows (make-pos 950 250) "windows"))
(define button5 (make-button roof (make-pos 790 250) "roof"))




;;;;;;;;;;;;;;;;;;;;;;;
;; DRAWING FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;

; purpose: draw button
; contract: drawButton: b(button) -> image
; drawButton test for:
(check-expect (drawButton (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 250) "START"))
              (above (text "START" 25 "black")(rectangle 150 50 "solid" "blue")))
; drawButton function:
(define (drawButton b)
  (above (text (button-text b) 25 "black") (button-im b)))



; purpose: draw the menu scene
; contract: drawMenu: m(menu) -> image
; drawMenu test:
(check-expect (drawMenu (make-menu (make-button (rectangle 150 50 "solid" "blue")
                                                (make-pos 250 250) "START")
                                   (make-button (rectangle 150 50 "solid" "blue")
                                                (make-pos 250 400) "LEADERBOARD")))
              (place-image (above (text "START" 25 "black")(rectangle 150 50 "solid" "blue")) 250 250
                           (place-image (above (text "LEADERBOARD" 25 "black")(rectangle 150 50 "solid" "blue")) 250 400
                                        background)))
; drawMenu function:
(define (drawMenu m) (place-image (drawButton (menu-start m))
                                  (pos-x (button-pos (menu-start m)))
                                  (pos-y (button-pos (menu-start m)))
                                  (place-image (drawButton (menu-leaderBoard m))
                                               (pos-x (button-pos (menu-leaderBoard m)))
                                               (pos-y (button-pos (menu-leaderBoard m)))
                                               background)))




; purpose: draw the leaderboard scene
; contract: drawLeaderboard: scene(leaderboard) filename(string) -> image
; drawLeaderBoard test:
(check-expect (drawLeaderBoard (make-leaderBoard (make-button (rectangle 250 50 "solid" "blue")
                                                       (make-pos 250 450) "RETURN MENU")) "leaderboard3")(place-image (text "-Leaderboard-" 50 "orange") 630 80
                              (place-image (text (read-file "leaderboard3") 12 "black") 630 170
                                           (place-image (drawButton (make-button (rectangle 250 50 "solid" "blue")
                                                       (make-pos 250 450) "RETURN MENU"))
                                                        250 450                                                         
                                                        background))))            

; drawLeaderBoard function:
(define (drawLeaderBoard scene filename) (cond
           ; if file is not exists 
           ((not (file-exists? filename)) (place-image
                (text "-Leaderboard-" 50 "orange") 630 80
                (place-image (text "No record is found..." 12 "black") 650 550
                             (place-image (drawButton (leaderBoard-btnReturn scene))
                                          (pos-x (button-pos (leaderBoard-btnReturn scene)))
                                          (pos-y (button-pos (leaderBoard-btnReturn scene)))
                                          background))))
           ; otherwise 
           (else (place-image (text "-Leaderboard-" 50 "orange") 630 80
                              (place-image (text (read-file filename) 12 "black") 630 170
                                           (place-image (drawButton (leaderBoard-btnReturn scene))
                                                        (pos-x (button-pos (leaderBoard-btnReturn scene)))
                                                        (pos-y (button-pos (leaderBoard-btnReturn scene)))
                                                        background))))))




; purpose: draw the game scene and game over
; contract: drawGameScene: g(game3) -> image
; drawScene test:
(check-expect (drawScene (make-game3 (make-menu (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 250) "START")
                                              (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 400) "LEADERBOARD"))
                                     "gameScene"
                                     (make-button grass (make-pos 470 250) "grass")
                                     (make-button mainstructure (make-pos 250 250) "mainstructure")
                                     (make-button door (make-pos 650 250) "door")
                                     (make-button windows (make-pos 950 250) "windows")
                                     (make-button roof (make-pos 790 250) "roof")
                                     1
                                     3
                                     #false
                                     (make-leaderBoard (make-button (rectangle 250 50 "solid" "blue")
                                                       (make-pos 250 450) "RETURN MENU"))
                                     "leaderboard3"
                                     (make-game-over-scene (make-button (rectangle 250 50 "solid" "blue")
                                                    (make-pos 250 450) "RETURN MENU"))))

                           (place-image (overlay (text "GAME-3" 30 "white")(rectangle 150 70 "solid" "blue")) 600 70(place-image (text
                                         "To ensure a function works correctly, we must write its elements in the proper order.
                      Let's learn how to arrange them by building a house.
                We'll start from the foundation and draw the house upwards."
                                               15 "black")600 140
                                                          (place-image (text "Time: 3" 20 "black")50 20
                                                                       (place-image (above (text "grass" 25 "black") grass) 470 250
                                                                                    (place-image (above (text "mainstructure" 25 "black") mainstructure)  250 250
                                                                                                 (place-image (above (text "door" 25 "black") door) 650 250
                                                                                                              (place-image (above (text "windows" 25 "black") windows )950 250
                                                                                                                           (place-image (above (text "roof" 25 "black") roof ) 790 250
                                                                                                                                        background
                                                                                                                         )))))))))
                                     
                                     
         
;drawScene function:
(define (drawScene g)

  
    ;; Game Over scene
  (if (game3-game-over g)
      (place-image
       (text "Game Over!" 50 "red")
       650 360
       (place-image
        (text (string-append "Time: " (number->string (game3-time g))) 30 "black")
        650 400 (place-image  (drawButton (game-over-scene-btnReturn (game3-game-over-scene g)))
       (pos-x (button-pos (game-over-scene-btnReturn (game3-game-over-scene g))))(pos-y (button-pos (game-over-scene-btnReturn (game3-game-over-scene g))))background)))

       
       ;; main game scene
      
      (place-image (overlay (text "GAME-3" 30 "white")(rectangle 150 70 "solid" "blue")) 600 70
                   (place-image(text
                                         "To ensure a function works correctly, we must write its elements in the proper order.
                      Let's learn how to arrange them by building a house.
                We'll start from the foundation and draw the house upwards."
                                               15 "black")600 140 (place-image (text (string-append "Time: " (number->string (game3-time g))) 20 "black")
       50 20
       (let ((background-layer background)
             (grass-layer (place-image
                           (drawButton (game3-button1 g))
                           (pos-x (button-pos (game3-button1 g)))
                           (pos-y (button-pos (game3-button1 g)))
                           background)))
         (let ((mainstructure-layer (place-image
                                     (drawButton (game3-button2 g))
                                     (pos-x (button-pos (game3-button2 g)))
                                     (pos-y (button-pos (game3-button2 g)))
                                     grass-layer)))
           (let ((door-layer (place-image
                              (drawButton (game3-button3 g))
                              (pos-x (button-pos (game3-button3 g)))
                              (pos-y (button-pos (game3-button3 g)))
                              mainstructure-layer)))
             (let ((windows-layer (place-image
                                   (drawButton (game3-button4 g))
                                   (pos-x (button-pos (game3-button4 g)))
                                   (pos-y (button-pos (game3-button4 g)))
                                   door-layer)))
               (place-image
                (drawButton (game3-button5 g))
                (pos-x (button-pos (game3-button5 g)))
                (pos-y (button-pos (game3-button5 g)))
                windows-layer))))))))))





; drawing the whole game
; purpose: draw the scenes of the game with respect to the current scene property
; contract: drawGame: g(game3) -> image
; drawGame test:
(check-expect (drawGame (make-game3 (make-menu (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 250) "START")
                                              (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 400) "LEADERBOARD"))
                                     "gameScene"
                                     (make-button grass (make-pos 470 250) "grass")
                                     (make-button mainstructure (make-pos 250 250) "mainstructure")
                                     (make-button door (make-pos 650 250) "door")
                                     (make-button windows (make-pos 950 250) "windows")
                                     (make-button roof (make-pos 790 250) "roof")
                                     0
                                     0
                                     #false
                                     (make-leaderBoard (make-button (rectangle 250 50 "solid" "blue")
                                                       (make-pos 250 450) "RETURN MENU"))
                                     "leaderboard3"
                                     (make-game-over-scene (make-button (rectangle 250 50 "solid" "blue")
                                                    (make-pos 250 450) "RETURN MENU"))))
              (place-image (overlay (text "GAME-3" 30 "white")(rectangle 150 70 "solid" "blue")) 600 70(place-image (text
                                         "To ensure a function works correctly, we must write its elements in the proper order.
                      Let's learn how to arrange them by building a house.
                We'll start from the foundation and draw the house upwards."
                                               15 "black")600 140
                                                          (place-image (text "Time: 0" 20 "black")50 20
                                                                       (place-image (above (text "grass" 25 "black") grass) 470 250
                                                                                    (place-image (above (text "mainstructure" 25 "black") mainstructure)  250 250
                                                                                                 (place-image (above (text "door" 25 "black") door) 650 250
                                                                                                              (place-image (above (text "windows" 25 "black") windows )950 250
                                                                                                                           (place-image (above (text "roof" 25 "black") roof ) 790 250
                                                                                                                                        background
                                                                                                                         )))))))))
                                              
                        
                     
; drawGame function:
(define (drawGame g) (cond ((string=? (game3-currentScene g) "menu") (drawMenu (game3-menu g)))
                           ((string=? (game3-currentScene g) "leaderBoard") (drawLeaderBoard (game3-leaderBoard g) (game3-leaderboardfile g)))
                           ((string=? (game3-currentScene g) "gameScene") (drawScene g))
                           ))
                           



;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;

; purpose: calculate the distance between given position and x, y values for cursor position
; contract: calcDist: p(pos) x(number) y(number) -> number
; calcDist test:
(check-expect (calcDist (make-pos 0 0) 3 4) 5)
; function
(define (calcDist p x y)
  (sqrt (+ (sqr (- (pos-x p) x)) (sqr (- (pos-y p) y)))))


; purpose: check whether the cursor object (or button) is clicked. abstract functions determine the type of the object
; contract: isClicked: o(any) x(number) y(number) getPosFunc(function) getImFunc(function) -> boolean
; isClicked test:
(check-expect (isClicked (make-button (circle 50 "solid" "red") (make-pos 200 200)"button") 175 175 button-pos button-im) #true)
;function:
(define (isClicked o x y getPosFunc getImFunc)
  (let* ((pos (getPosFunc o))
         (img-width (image-width (getImFunc o)))
         (img-height (image-height (getImFunc o))))
    (and (>= x (- (pos-x pos) (/ img-width 2)))
         (<= x (+ (pos-x pos) (/ img-width 2)))
         (>= y (- (pos-y pos) (/ img-height 2)))
         (<= y (+ (pos-y pos) (/ img-height 2))))))



;;;;;;;;;;;;;;;;;;;;;;;;
;; UPDATING FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;

; purpose: write the game score to the leaderboard file (requires batch-io.rkt teachpack)
; contract: writeScore: g(game3) -> string
; writeScore test:
(check-expect (writeScore(make-game3 (make-menu (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 250) "START")
                                              (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 400) "LEADERBOARD"))
                                     "gameScene"
                                     (make-button grass (make-pos 470 250) "grass")
                                     (make-button mainstructure (make-pos 250 250) "mainstructure")
                                     (make-button door (make-pos 650 250) "door")
                                     (make-button windows (make-pos 950 250) "windows")
                                     (make-button roof (make-pos 790 250) "roof")
                                     0
                                     0
                                     #false
                                     (make-leaderBoard (make-button (rectangle 250 50 "solid" "blue")
                                                       (make-pos 250 450) "RETURN MENU"))
                                     "leaderboard3"
                                     (make-game-over-scene (make-button (rectangle 250 50 "solid" "blue")
                                                    (make-pos 250 450) "RETURN MENU"))))
                         (write-file "leaderboard3"
                                      (string-append (read-file "leaderboard3")
                                                     "Score: " " 0")))

                        
; writeScore function:
(define (writeScore g) (cond
                         ; create new file if it doesnt exists 
                         ((not (file-exists? (game3-leaderboardfile g)))
                          (write-file (game3-leaderboardfile g)
                                      (string-append "Score: " (number->string (floor (game3-time g))) " sec.\n\n")))
                         ; otherwise overwrite the file
                         (else
                          (write-file (game3-leaderboardfile g)
                                      (string-append (read-file (game3-leaderboardfile g))
                                                     "Score: " (number->string (floor (game3-time g))) " sec.\n\n")))
                         ))




; purpose: update the game with respect to the mouse interaction
; contract: updateOnMouse: g(game3) x(number) y(number) event(mouse event) -> game3
; updateOnMouse test:
(check-expect (updateOnMouse (make-game3 (make-menu (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 250) "START")
                                              (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 400) "LEADERBOARD"))
                                     "gameScene"
                                     (make-button grass (make-pos 470 250) "grass")
                                     (make-button mainstructure (make-pos 250 250) "mainstructure")
                                     (make-button door (make-pos 650 250) "door")
                                     (make-button windows (make-pos 950 250) "windows")
                                     (make-button roof (make-pos 790 250) "roof")
                                     0
                                     0
                                     #false
                                     (make-leaderBoard (make-button (rectangle 250 50 "solid" "blue")
                                                       (make-pos 250 450) "RETURN MENU"))
                                     "leaderboard3"
                                     (make-game-over-scene (make-button (rectangle 250 50 "solid" "blue")
                                                    (make-pos 250 450) "RETURN MENU"))) 470 250 "button-down")
              
              (make-game3 (make-menu (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 250) "START")
                                              (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 400) "LEADERBOARD"))
                                     "gameScene"
                                     (make-button (rectangle 200 50 "solid" "green") (make-pos 720 625) "")
                                     (make-button (square 100 "solid" "brown") (make-pos 250 250) "mainstructure")
                                     (make-button (rectangle 20 40 "solid" "black") (make-pos 650 250) "door")
                                     (make-button (beside pencere1 pencereboslugu pencere1) (make-pos 950 250) "windows")
                                     (make-button (triangle 100 "solid" "red") (make-pos 790 250) "roof")
                                     1
                                     0
                                     #false
                                     (make-leaderBoard (make-button (rectangle 250 50 "solid" "blue")
                                                       (make-pos 250 450) "RETURN MENU"))
                                     "leaderboard3"
                                     (make-game-over-scene (make-button (rectangle 250 50 "solid" "blue")
                                                    (make-pos 250 450) "RETURN MENU"))))



;updateOnMouse function:
(define (updateOnMouse g x y event)
  (cond    
    ;start button is clicked
    ((and (string=? (game3-currentScene g) "menu")
          (mouse=? event "button-down")
          (isClicked (menu-start (game3-menu g)) x y button-pos button-im))

     (make-game3
       (game3-menu g)
       "gameScene"
       (game3-button1 g)
       (game3-button2 g)
       (game3-button3 g)
       (game3-button4 g)
       (game3-button5 g)
       (game3-current-step g)
       (game3-time g)  
       #false
       (game3-leaderBoard g)
       (game3-leaderboardfile g)
       (game3-game-over-scene g)))



     ; leaderboard button is clicked
    ((and (string=? (game3-currentScene g) "menu")(mouse=? event "button-down")
                                            (isClicked (menu-leaderBoard (game3-menu g)) x y button-pos button-im))
                                      (make-game3 (game3-menu g)
                                                  "leaderBoard"
                                                 (game3-button1 g)
                                                 (game3-button2 g)
                                                 (game3-button3 g)
                                                 (game3-button4 g)
                                                 (game3-button5 g)
                                                 (game3-current-step g)
                                                 (game3-time g) ;
                                                 #false
                                                 (game3-leaderBoard g)
                                                 (game3-leaderboardfile g)
                                                 (game3-game-over-scene g)))


     ;; Game Over Return Menu button click
    ((and (game3-game-over g)
          (mouse=? event "button-down")
          (isClicked (game-over-scene-btnReturn (game3-game-over-scene g)) x y button-pos button-im))
     (make-game3
       (game3-menu g)
       "menu"
       button1
       button2
       button3
       button4
       button5
       0 ; Reset current step
       0 ; Reset time
       #false ; Reset game-over status
       (game3-leaderBoard g)
       (game3-leaderboardfile g)
       (game3-game-over-scene g)))
    

    ;; leaderBoard return menu button is clicked

           ((and (mouse=? event "button-down")          
           (isClicked (leaderBoard-btnReturn (game3-leaderBoard g)) x y button-pos button-im))
     (make-game3
       (game3-menu g)
       "menu"
       (game3-button1 g)
       (game3-button2 g)
       (game3-button3 g)
       (game3-button4 g)
       (game3-button5 g)
       0 ; Reset current step
       0 ; Reset time
       #false ; Reset game-over status
       (game3-leaderBoard g)
       (game3-leaderboardfile g)
       (game3-game-over-scene g)))                   
    
    
    ;; if the grass is in orrect order 
    ((and (mouse=? event "button-down")
          (= (game3-current-step g) 0)
          (isClicked (game3-button1 g) x y button-pos button-im))
     (make-game3
      (game3-menu g)
      "gameScene"
      (make-button grass (make-pos 720 625) "")
      (game3-button2 g)
      (game3-button3 g)
      (game3-button4 g)
      (game3-button5 g)
      1
      (game3-time g)
      #false
      (game3-leaderBoard g)
      (game3-leaderboardfile g)
      (game3-game-over-scene g)))

    ;; if the mainstructure is in orrect order 
    ((and (mouse=? event "button-down")
          (= (game3-current-step g) 1)
          (isClicked (game3-button2 g) x y button-pos button-im))
     (make-game3
      (game3-menu g)
      "gameScene"
      (game3-button1 g)
      (make-button mainstructure (make-pos 720 550) "")
      (game3-button3 g)
      (game3-button4 g)
      (game3-button5 g)
      2
      (game3-time g)
      #false
      (game3-leaderBoard g)
      (game3-leaderboardfile g)
      (game3-game-over-scene g)))

    ;; if the door is in orrect order 
    ((and (mouse=? event "button-down")
          (= (game3-current-step g) 2)
          (isClicked (game3-button3 g) x y button-pos button-im))
     (make-game3
      (game3-menu g)
      "gameScene"
      (game3-button1 g)
      (game3-button2 g)
      (make-button door (make-pos 720 580) "")
      (game3-button4 g)
      (game3-button5 g)
      3
      (game3-time g)
      #false
      (game3-leaderBoard g)
      (game3-leaderboardfile g)
      (game3-game-over-scene g)))

   
    ;; if the windows is in orrect order 
    ((and (mouse=? event "button-down")
          (= (game3-current-step g) 3)
          (isClicked (game3-button4 g) x y button-pos button-im))
     (make-game3
      (game3-menu g)
      "gameScene"
      (game3-button1 g)
      (game3-button2 g)
      (game3-button3 g)
      (make-button windows (make-pos 720 540) "")
      (game3-button5 g)
      4
      (game3-time g)
      #false
      (game3-leaderBoard g)
      (game3-leaderboardfile g)
      (game3-game-over-scene g)))

    ;; if the roof is in orrect order, game is over 
   ((and (mouse=? event "button-down")
          (= (game3-current-step g) 4)
          (isClicked (game3-button5 g) x y button-pos button-im))
     (begin
       (writeScore g) ; Skoru kaydet
       (make-game3
         (game3-menu g)
         "gameScene"
         (game3-button1 g)
         (game3-button2 g)
         (game3-button3 g)
         (game3-button4 g)
         (make-button roof (make-pos 720 460) "")
         5
         (game3-time g)
         #true
         (game3-leaderBoard g)
         (game3-leaderboardfile g)
         (game3-game-over-scene g))))
    
      ((game3-game-over g) g)
    ;; if in wrong order, game. 
    (else g)))





; purpose: update the game with tick. checks whether player achieved the goal and count seconds until the game is finished.
; contract: world-tick: g(game) -> game
;world-tick test:
(check-expect (world-tick (make-game3 (make-menu (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 250) "START")
                                              (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 400) "LEADERBOARD"))
                                     "gameScene"
                                     (make-button grass (make-pos 470 250) "grass")
                                     (make-button mainstructure (make-pos 250 250) "mainstructure")
                                     (make-button door (make-pos 650 250) "door")
                                     (make-button windows (make-pos 950 250) "windows")
                                     (make-button roof (make-pos 790 250) "roof")
                                     0
                                     0
                                     #false
                                     (make-leaderBoard (make-button (rectangle 250 50 "solid" "blue")
                                                       (make-pos 250 450) "RETURN MENU"))
                                     "leaderboard3"
                                     (make-game-over-scene (make-button (rectangle 250 50 "solid" "blue")
                                                    (make-pos 250 450) "RETURN MENU"))))
                          (make-game3 (make-menu (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 250) "START")
                                              (make-button (rectangle 150 50 "solid" "blue") (make-pos 250 400) "LEADERBOARD"))
                                     "gameScene"
                                     (make-button grass (make-pos 470 250) "grass")
                                     (make-button mainstructure (make-pos 250 250) "mainstructure")
                                     (make-button door (make-pos 650 250) "door")
                                     (make-button windows (make-pos 950 250) "windows")
                                     (make-button roof (make-pos 790 250) "roof")
                                     0
                                     1
                                     #false
                                     (make-leaderBoard (make-button (rectangle 250 50 "solid" "blue")
                                                       (make-pos 250 450) "RETURN MENU"))
                                     "leaderboard3"
                                     (make-game-over-scene (make-button (rectangle 250 50 "solid" "blue")
                                                    (make-pos 250 450) "RETURN MENU")))) 

; updating the time with world-tick 
(define (world-tick g)
  (cond
    ((game3-game-over g) g)
    ((string=? (game3-currentScene g) "menu") g)
    ((string=? (game3-currentScene g) "leaderBoard") g)
    ((boolean=? #true (game3-game-over g))(writeScore g))
      (else (make-game3
       (game3-menu g)
       "gameScene"
       (game3-button1 g)
       (game3-button2 g)
       (game3-button3 g)
       (game3-button4 g)
       (game3-button5 g)
       (game3-current-step g)
       (+ (game3-time g) 1) ;; Zamanı artır
       #false
       (game3-leaderBoard g)
       (game3-leaderboardfile g)
       (game3-game-over-scene g)))
                                ))


;;;;;;;;;;
;; GAME ;;
;;;;;;;;;;




; initial game state
(define GAME (make-game3 (make-menu (make-button (rectangle 200 50 "solid" "blue")
                                                (make-pos 650 200) "START")
                                   (make-button (rectangle 200 50 "solid" "blue")
                                                (make-pos 650 400) "LEADERBOARD"))
                        "menu"
                        button1 button2 button3 button4 button5
                        0 0
                        #false
                        (make-leaderBoard (make-button (rectangle 250 50 "solid" "blue")
                                                       (make-pos 250 450) "RETURN MENU"))
                        "leaderboard3"
                        (make-game-over-scene (make-button (rectangle 250 50 "solid" "blue")
                                                    (make-pos 250 450) "RETURN MENU"))))
; big-bang
(big-bang GAME
          (to-draw drawGame)
          (on-tick world-tick 1) ;; each second is one tick
          (on-mouse updateOnMouse))
