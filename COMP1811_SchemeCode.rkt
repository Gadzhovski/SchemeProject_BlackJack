;;Seven and Half Computer-Aided Solitaire

;; Radoslav Gadzhovski ID: 001200583
;; Riwaj Silwal ID: 001195216






;; Spades, clubs, Hearts, Diamonds

;; Coding cards

(require racket/list)  ;; shuffle


;; F1 - The cards

;; Invariant representation

;; a pair of letter

(define (card? value)             ;; Returns #t if the the card has both "numeral/face" and "suite" . 
  (and
   (pair? value)
   (list? (member (car value) '( 1 2 3 4 5 6 7 #\J #\Q #\K)))
   (list? (member (cdr value) '(#\H #\S #\C #\D)))
   )
  )



(define (suite card)          ;; Returns the second part of the card which is its "suite".
  (cdr card))   



(define (numeral card)        ;; Returns the first part of the card which is its "rank/numeral".
  (car card))  



(define (face? card)
  (not (integer? (numeral card))))  ;; Check if the has "face" (Jack, Queen or King) returns #t.



(define (value card)          ;; Returns the "value" of a given card.
  (cond
    [(face? card) 0.5]
    [else (numeral card)]))



(define (card->string value)        ;; Given a valid card, it returns a human readable string for it.
  (cond
   [(face? value) (string (numeral value) (suite value))]
   [else
    (string-append
    (number->string (numeral value))
    (string (suite value)))]))




;; 3 .- The deck.

(define (deck? values)         ;; Provided a list of values, returns #t if it is a valid deck.
  (cond
  [(empty? values) #t]
  [else
   (and
    (card? (first values))
    (deck? (rest values)))]))


(define (valueOf round)      ;; Returns the total value of the cards in the rouund.
  (cond
    [(empty? round) 0]
    [else
     (+ (value (first round)) (valueOf (rest round)))]))


;; hard

(define (do-suite value)       ;; Entire list of all 40 valid cards( 7 numerical and 3 faces)*4).
  (map
   (lambda (num) (cons num value))
   '( 1 2 3 4 5 6 7 #\J #\Q #\K )))
          

;; how to generate a deck.

(define deck (append (do-suite #\H) (do-suite #\S) (do-suite #\C) (do-suite #\D)))   ;; Generating deck


(define (deck->strings deck)     ;;  Passed a valid deck, it returns a human representation of it.
  (cond
    [(empty? deck) empty]
    [else
     (cons
      (card->string (first deck))
      (deck->strings (rest deck)))]))



;;(define (playS deck hand strategy)
;;F3 - Probabilities.


(define (probability comp number deck)     ;; Passed (<, >, =), a number, and a list of cards, works out the number of cards in list having values less,                                     
  (cond                                 ;;  greater or equal than the provided number respectively.
    [(empty? deck) 0] 
    [(comp (value (first deck)) number)
     (+ 1 (probability comp number (rest deck)))]
    [else (probability comp number (rest deck))]))



(define cheat #f)         ;; Hides the next card from the deck



;; F4.- Game.

;; DO NO CHANGE THE  FUNCTIONS BELOW THIS LINE

;; -----------------------------------------------------

;; -----------------------------------------------------

;; -----------------------------------------------------

;; -----------------------------------------------------

;; -----------------------------------------------------

(define (show-statistics deck hand)

  (let

      ([toCheck (- 7.5 (valueOf hand))])

    (display

     (format

      "P(>7.5):~a/~a\nP(<7.5):~a/~a\nP(=7.5):~a/~a\nHAND:~a~nVALUE:~a\nDECK:~a...\n"

      (probability > toCheck deck)

      (length deck)

      (probability < toCheck deck)

      (length deck)

      (probability = toCheck deck)

      (length deck)                     

      (deck->strings hand)

      (valueOf hand)

      (if cheat (deck->strings (take deck

                                     (max 0 

                                          (min 4 (length deck) )))) "****")

      )

     )))

  

;; Human interaction.

(define (play deck hand)

  (begin      

    (show-statistics deck hand)

    ;; Control

    (cond

      [(= (valueOf hand) 7.5) (display "WIN")]

      [(> (valueOf hand) 7.5) (display "LOST")]

      [(empty? deck) (display "NO CARDS LEFT") ]

      [(let

           ([ command (read)])

         (cond

           [(equal? command 'accept)

            (play (rest deck) (cons (first deck) hand))]

           [(equal? command 'pass)

            (play (drop deck 1) hand)]

           [(equal? command 'end) (void)]

           [else (play deck hand)]))])))