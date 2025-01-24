;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname matematikbitmiş) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
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

;;; fixed values
(define WIDTH 1260)
(define HEIGHT 700)
(define PLAYER-SIZE 20)
(define TARGET-SIZE 30)
(define INITIAL-SCORE 0)
(define LEADERBOARD-FILE "leaderboard2.txt")

;;world State
;structure: world
;attribute: player-x(pos) 
;attribute: player-y(pos)
;attribute: targets(list of targets)
;attribute: current-answer (The target that player should currently go to)
;attribute: sequence(saves the player's progress)
;attribute: score(number)
;attribute: remaining-questions(list of questions that needs to be solve)
;attribute: game-over?(Checks whether the game is over or not)
;attribute: show-leaderboard?(Controls whether the leaderboard will be shown or not)
;attribute: show-menu?(Checks whether the menu will be shown or not)
;attribute: Menu(allows visualization and operation of the menu)
;attribute: currentScene( The current scene of the game (e.g., "menu", "game", "game-over")
(define-struct world (player-x player-y targets current-answer sequence score remaining-questions game-over? show-leaderboard? show-menu? Menu currentScene))

; background                
(define background-color "lightblue")
(define background
  (overlay
   (rectangle 1260 700 "solid" background-color)
   (empty-scene 1260 700)))

(define (drawButton b)
  (above (text (button-text b) 25 "black") (button-im b)))

;draw buttons
;purpose: draw buttons
;contract: drawButton: b(button) -> image
;test:
(check-expect (drawButton (make-button (rectangle 100 70 "solid" "red") (make-pos 200 150) "LEADERBOARD"))
              (above (text "LEADERBOARD" 25 "black")(rectangle 100 70 "solid" "red")))

; purpose: draw the menu scene
; contract: drawMenu: m(menu) -> image
;function:
(define (drawMenu m) (place-image (drawButton (menu-start m))
                                  (pos-x (button-pos (menu-start m)))
                                  (pos-y (button-pos (menu-start m)))
                                  (place-image (drawButton (menu-leaderBoard m))
                                               (pos-x (button-pos (menu-leaderBoard m)))
                                               (pos-y (button-pos (menu-leaderBoard m)))
                                               background)))

;purpose: generate random questions
;contract: genera-question -> loq(list of questions)
(define (generate-question)
  (let* ([operators '(+ - * /)] 
         [random-op (list-ref operators (random (length operators)))] 
         [num1 (random 1 10)] 
         [num2 (random 1 10)]) 
    (list "(" random-op num1 num2 ")" )))

;purpose:generate target with random positions
;contract: generate-targets question(loq)-> targets(list)
(define (generate-targets question)
  
  (map (lambda (item)
         (list item (random 50 (- WIDTH 50)) (random 50 (- HEIGHT 50))))
       question))


;purpose: checking if the player touched the target
;contract: check-collision player-x(pos) player-y(pos)-> boolean
(define (check-collision player-x player-y target)
  (and (list? target) 
       (= (length target) 3) 
       (< (distance player-x player-y (second target) (third target)) (+ PLAYER-SIZE (/ TARGET-SIZE 2)))))

;purpose: check the distance between two positiom
;contract: distance x1(pos) y1(pos) x2(pos) y2(pos)->number  
(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))

;purpose: checks if the player has collided with any target in the list of targets.
;contract:find-hit-target player-x(pos) player-y(pos) targets(lot)-> target(list or #f)
(define (find-hit-target player-x player-y targets)
  (cond
    [(null? targets) #f] 
    [(and (list? (first targets)) (check-collision player-x player-y (first targets))) (first targets)] 
    [else (find-hit-target player-x player-y (rest targets))])) 
;purpose: writes the current score to a leaderboard file.
;contract:write-leaderboard: score(number) -> void(performs a side effect of writing to a file)
(define (write-leaderboard score)
  (let ([current-content (if (file-exists? LEADERBOARD-FILE)
                              (read-file LEADERBOARD-FILE)
                              "")])
    (write-file LEADERBOARD-FILE
                (string-append current-content "Score: " (number->string score) "\n"))))


;; Contract:max-score : number
;; 
;; Purpose: Represents the maximum allowable score in a game or evaluation system. It sets a limit to ensure scores do not exceed this value.

(define max-score 5) ;max score limit

;purpose: renews the world with every tick
; contract: world-tick: w(world) -> world
(define (world-tick w)
  (if (or (world-game-over? w)
          (not(string=? (world-currentScene w) "gameScene")))
      w 
      (let* ([player-x (world-player-x w)]
             [player-y (world-player-y w)]
             [targets (world-targets w)]
             [current-answer (world-current-answer w)]
             [sequence (world-sequence w)]
             [score (world-score w)]
             [remaining-questions (world-remaining-questions w)]
             [hit-target (find-hit-target player-x player-y targets)])
        (cond
         
          [(and (list? hit-target)
                (equal? (first hit-target) (list-ref current-answer (length sequence))))
           (let ([next-sequence (append sequence (list (first hit-target)))]
                 [new-score (+ score 1)])
             (if (equal? next-sequence current-answer)
                 (if (>= new-score max-score) 
                     (begin
                       (write-leaderboard new-score)
                       (make-world player-x player-y '() '() '() new-score '() #t #f #f (world-Menu w) "gameOver"))
                     (if (null? remaining-questions) 
                         (let ([new-questions (list (generate-question) (generate-question))])
                           (make-world player-x player-y
                                       (generate-targets (car new-questions))
                                       (car new-questions)
                                       '()
                                       new-score
                                       (cdr new-questions)
                                       #f #f #f
                                       (world-Menu w) "gameScene"))
                         (make-world player-x player-y
                                     (generate-targets (car remaining-questions))
                                     (car remaining-questions)
                                     '()
                                     new-score
                                     (cdr remaining-questions)
                                     #f #f #f
                                     (world-Menu w) "gameScene")))
                 (make-world player-x player-y
                             (remove hit-target targets)
                             current-answer
                             next-sequence
                             score
                             remaining-questions
                             #f #f #f
                             (world-Menu w) "gameScene")))]

          
          ((and (list? hit-target)
                (not (equal? (first hit-target) (list-ref current-answer (length sequence)))))
           (begin
             (write-leaderboard score)
             (make-world player-x player-y
                         targets
                         current-answer
                         sequence
                         score
                         remaining-questions
                         #t #f #f (world-Menu w) "gameOver")))

         
          [else w]))))




;purpose:To handle the scenario when the player hits the correct target and update the game state accordingly.
;contract: handle-correct-hit: w(world) player-x(number) player-y(number) targets(list) current-answer(list) sequence(list) score(number) remaining-questions(list) hit-target(list) -> world
(define (handle-correct-hit w player-x player-y targets current-answer sequence score remaining-questions hit-target)
  (let ([next-sequence (append sequence (list (first hit-target)))])
    (if (equal? next-sequence current-answer)
        (if (>= (+ score 1) max-score)
            (begin
              (write-leaderboard (+ score 1))
              (make-world player-x player-y '() '() '() (+ score 1) '() #t #f #f (world-Menu w) "gameOver"))
           (if (null? remaining-questions)
                (let ([new-questions (list (generate-question) (generate-question))])
                  (begin (make-world player-x player-y 
                              (generate-targets (car new-questions))
                              (car new-questions) 
                              '() 
                              (+ score 1) 
                              (cdr new-questions) 
                              #f #f #f 
                              (world-Menu w) "gameScene")))
                (make-world player-x player-y 
                            (generate-targets (car remaining-questions))
                            (car remaining-questions) 
                            '() 
                            (+ score 1) 
                            (cdr remaining-questions) 
                            #f #f #f 
                            (world-Menu w) "gameScene")))
        (make-world player-x player-y targets current-answer next-sequence score remaining-questions #f #f #f (world-Menu w) "gameScene"))))





;purpose: to handle keyboard input and update the game world accordingly.
;contract: handle-key: w(world) key(string) -> world
(define (handle-key w key)
  (let* ([player-x (world-player-x w)]
         [question (generate-question)])
  (if (world-game-over? w)
      (if (equal? key "r")
          
          (make-world (/ WIDTH 2) (/ HEIGHT 2) (generate-targets question) question '() 0 (list (generate-question) (generate-question)) #f #f #f (world-Menu w)
       "gameScene")
          w)
      (let ([player-x (world-player-x w)]
            [player-y (world-player-y w)])
        (cond
          [(equal? key "left") (make-world (max 0 (- player-x 10)) player-y (world-targets w) (world-current-answer w) (world-sequence w) (world-score w) (world-remaining-questions w) #f #f #f (world-Menu w)
       "gameScene")]
          [(equal? key "right") (make-world (min WIDTH (+ player-x 10)) player-y (world-targets w) (world-current-answer w) (world-sequence w) (world-score w) (world-remaining-questions w) #f #f #f (world-Menu w)
       "gameScene")]
          [(equal? key "up") (make-world player-x (max 0 (- player-y 10)) (world-targets w) (world-current-answer w) (world-sequence w) (world-score w) (world-remaining-questions w) #f #f #f (world-Menu w)
       "gameScene")]
          [(equal? key "down") (make-world player-x (min HEIGHT (+ player-y 10)) (world-targets w) (world-current-answer w) (world-sequence w) (world-score w) (world-remaining-questions w) #f #f #f (world-Menu w)
       "gameScene")]
          [else w])))))

;purpose: to handle mouse input and update the game world based on the player's click events.
;contract: handle-mouse: w(world) x(number) y(number) event(string) -> world
(define (handle-mouse w x y event)
  (let* ([player-x (world-player-x w)]
         [question (generate-question)])
    (cond
      
      [(and (equal? event "button-down")
            (string=? (world-currentScene w) "menu")
            (<= x (+ (pos-x (button-pos (menu-start (world-Menu w)))) 100)) 
            (>= x (- (pos-x (button-pos (menu-start (world-Menu w)))) 100))
            (<= y (+ (pos-y (button-pos (menu-start (world-Menu w)))) 25))  
            (>= y (- (pos-y (button-pos (menu-start (world-Menu w)))) 25)))
      
       (make-world (/ WIDTH 2) (/ HEIGHT 2) 
                   (generate-targets question)  
                   question
                   '() 
                   0 
                   (list (generate-question) (generate-question)) 
                   #false #false #false 
                   (world-Menu w) 
                   "gameScene")]

      ;; LEADERBOARD butonuna tıklanırsa
      [(and (equal? event "button-down")
            (string=? (world-currentScene w) "menu")
            (<= x (+ (pos-x (button-pos (menu-leaderBoard (world-Menu w)))) 100))
            (>= x (- (pos-x (button-pos (menu-leaderBoard (world-Menu w)))) 100))
            (<= y (+ (pos-y (button-pos (menu-leaderBoard (world-Menu w)))) 25)) 
            (>= y (- (pos-y (button-pos (menu-leaderBoard (world-Menu w)))) 25)))
       
       (make-world (world-player-x w) (world-player-y w) 
                   (world-targets w) 
                   (world-current-answer w) 
                   (world-sequence w) 
                   (world-score w) 
                   (world-remaining-questions w) 
                   #false #true #false 
                   (world-Menu w) 
                   "leaderBoard")]
      
      [(and (equal? event "button-down")
      (string=? (world-currentScene w) "leaderBoard")
      (<= x 750) ; Return button width (650 + 100)
      (>= x 550) ; Return button width (650 - 100)
      (<= y 625) ; Return button height (600 + 25)
      (>= y 575)) ; Return button height (600 - 25)
 
 (make-world (world-player-x w) (world-player-y w) 
             (world-targets w) 
             (world-current-answer w) 
             (world-sequence w) 
             (world-score w) 
             (world-remaining-questions w) 
             #false #false #true 
             (world-Menu w) 
             "menu")]


      
      [(and (equal? event "button-down")
            (string=? (world-currentScene w) "gameOver")
            (<= x 350) 
            (>= x 150)
            (<= y 390) 
            (>= y 350))
       
       (make-world (world-player-x w) (world-player-y w) 
                   (world-targets w) 
                   (world-current-answer w)  
                   (world-sequence w) 
                   (world-score w) 
                   (world-remaining-questions w) 
                   #false #false #true 
                   (world-Menu w) 
                   "menu")] 

      
      [else w])))



;purpose: to render the game world visually based on its current state and scene
;contract: render-world: w(world) -> image
(define (render-world w)
  (let* ([player (circle PLAYER-SIZE "solid" "red")]
         [scene (empty-scene WIDTH HEIGHT)]
         [question-text 
          (text (string-append "Soru: "
                               (apply string-append 
                                      (map (lambda (item) (format "~a " item)) (world-current-answer w))))
                18 "black")]
         [score-text (text (string-append "Score: " (number->string (world-score w))) 18 "green")]
         [game-over-text (if (world-game-over? w)
                             (text "GAME OVER! Press R to Restart." 24 "red")
                             empty-image)]
         [return-menu-button (if (world-game-over? w)
                                 (place-image (text "Return Menu" 16 "white")
                                              250 370
                                              (place-image (rectangle 200 40 "solid" "blue")
                                                           250 370
                                                           scene))
                                 scene)])
    (cond
      [(world-show-leaderboard? w)
 (place-image
  (text (read-file LEADERBOARD-FILE) 16 "black")
  (/ WIDTH 2) (/ HEIGHT 2)
  (place-image (text "Return Menu" 16 "white")
               650 600
               (place-image (rectangle 200 50 "solid" "blue")
                            650 600
                            background)))]

      ((world-show-menu? w)
       (drawMenu (world-Menu w))) 
      (else (foldl (lambda (target img)
                     (let ([target-circle (circle TARGET-SIZE "solid" "blue")]
                           [target-text (text (format "~a" (first target)) 14 "white")])
                       (place-image target-text (second target) (third target)
                                    (place-image target-circle (second target) (third target) img))))
                   (place-image game-over-text (/ WIDTH 2) (/ HEIGHT 2)
                                (place-image question-text 150 20
                                             (place-image score-text 50 20
                                                          (place-image player (world-player-x w) (world-player-y w) return-menu-button))))
                   (world-targets w))))))

;helper values
(define scene (empty-scene WIDTH HEIGHT))
[define player (circle PLAYER-SIZE "solid" "red")]

;purpose: to decide and render the appropriate scene of the game based on the current state
;contract: drawGame: w(world) -> image
(define (drawGame w) 
  (cond 
    
    [(string=? (world-currentScene w) "menu") 
     (drawMenu (world-Menu w))]             

    [(string=? (world-currentScene w) "leaderBoard") 
     (place-image
      (text (read-file LEADERBOARD-FILE) 16 "black")
      (/ WIDTH 2) (/ HEIGHT 2) background)]

    [(string=? (world-currentScene w) "gameScene") 
     (render-world w)]

    [else background]))

;initial world
(define initial-world
  (let ([question (generate-question)])
    (make-world (/ WIDTH 2) (/ HEIGHT 2) (generate-targets question) question '() INITIAL-SCORE (list (generate-question) (generate-question)) #f #f #t (make-menu (make-button (rectangle 200 50 "solid" "blue")
                                                (make-pos 650 200) "START")
                                   (make-button (rectangle 200 50 "solid" "blue")
                                                (make-pos 650 400) "LEADERBOARD"))
                        "menu")))

;big bang
(big-bang initial-world
          (on-tick world-tick)
          (on-key handle-key)
          (on-mouse handle-mouse)
          (to-draw render-world))




