;; -------------------------------------------------------------------
;;    Joseph Vendryes
;;    May 2021
;;    Mastermind v2 Command Line
;; -------------------------------------------------------------------

#lang racket

(require "mastermind.rkt")

(struct mark (r w))
;; A Mark is a (mark (anyof Nat false) (anyof Nat false))

;; None -> Action
;; initiate a game
(define (start)
  (display
   (string-append "Introducing the Code-Crackin' Mind!\n"
                  "If you have not already done so, feel free to"
                  " compose a sequence of natural numbers for it"
                  " to guess.\n"
                  "...\n"
                  "Please provide a single upper bound greater than"
                  " or equal to each of the numbers in your code: "))
  (define pool-size-input
    (request "a natural number" (compose natural? string->number)))
  (display "Please enter the length of your code: ")
  (define code-length-input
    (request "a positive integer"
             (compose (and/c integer? positive? exact?)
                      string->number)))
  (cond [(not code-length-input) ; reached eof
         (display "Insufficient input: game cancelled.\n")
         (restart)]
        [else
         (define pool-size (add1 (string->number pool-size-input)))
         (define code-length (string->number code-length-input))
         (display
          (string-append
           "The Code-Crackin' Mind will now guess at your code. After"
           " each guess, please enter two numbers on the same line:"
           " first, how many numbers are both correctly identified"
           " and correctly placed, and then how many of the remaining"
           " numbers are correctly identified but misplaced.\n"))
         (continue (mastermind-first-play code-length pool-size)
                   code-length pool-size)]))

;; Board Nat -> Action
;; continue the game to its completion
(define (continue game code-length pool-size)
  (cond [(mastermind-impossible? game)
         (handle-bad-marking)]
        [else
         (define max-element (sub1 pool-size))
         (map (compose display
                       (string-lengthener
                        #\space
                        (add1 (string-length
                               (number->string max-element))))
                       number->string)
              (mastermind-current-guess game))
         (display ": ")
         (define mark (parse-mark (read-line)))
         (define r (mark-r mark))
         (define w (mark-w mark))
         (cond [(not (and (number? r)
                          (number? w)))
                (display
                 (string-append
                  "The Code-Crackin' Mind doesn't understand what you"
                  " wrote: game cancelled.\n"))
                (restart)]
               [(> (+ r w) code-length)
                (handle-bad-marking)]
               [(= r code-length) ; all correct
                (display "Thank you for playing!\n")
                (restart)]
               [else
                (continue (mastermind-play game r w)
                          code-length pool-size)])]))

;; None -> Action
;; inform the "master" player of a marking mistake, and offer
;;    to play again
(define (handle-bad-marking)
  (display
   (string-append
    "The Code-Crackin' Mind has determined that your clues so far"
    " do not accurately reflect a sequence of natural numbers with"
    " the length and upper bound you specified earlier:"
    " game cancelled.\n\n"
    "Examples of correct marking:\n\n"
    "Example 1:\n"
    "Your code: 7 0 3 3\n"
    "Guess:     4 7 3 7  you respond: 1 1\n"
    "since one 3 is correct and one of the 7s is misplaced."
    " The other 7 is incorrect (doesn't matter which).\n\n"
    "Example 2:\n"
    "Your code: 1 2 3\n"
    "Guess:     1 1 1  you respond: 1 0\n"
    "since one 1 is correct and the other two 1s are incorrect.\n\n"
    "Example 3:\n"
    "Your code: 1\n"
    "Guess:     2  you respond: 0 0\n"
    "since the 2 is incorrect.\n\n"))
  (restart))

;; None -> Action
;; offer to start a new game
(define (restart)
  (display
   "\nWould you like to play again? Y(es) / N(o): ")
  (define response
    (request "either Yes or Y or No or N (any case)"
             (or/c yes? no?)))
  (cond [(or (not (string? response)) ; e.g. reader reached eof
             (no? response))
         (display "Session ended.\n")]
        [else (start)]))

;; Str (Str -> Bool) -> (anyof Nat false)
;; read in an acceptable line of text, prompting as many times as
;;    needed using the provided description, or produce false at eof
(define (request description acceptable?)
  (define input (read-line))
  (cond [(not (string? input)) #f]
        [(acceptable? input) input]
        [else (display (string-append "(not accepted) Please enter "
                                      description ": "))
              (request description acceptable?)]))

;; Str -> Mark
;; interpret the string as a mark, whose r is the first word in the
;;    string and whose w is the rest of the string, converted to
;;    natural numbers if appropriate or else to false
(define (parse-mark str)
  (define pieces (string-split str))
  (if (= 2 (length pieces))
      (mark (string->number (first pieces))
            (string->number (second pieces)))
      (mark #f #f)))

;; Str -> Bool
;; check if the string is "yes" or "y" in capital or lowercase letters
(define (yes? str)
  (or (string-ci=? str "yes")
      (string-ci=? str "y")))

;; Str -> Bool
;; check if the string is "no" or "n" in capital or lowercase letters
(define (no? str)
  (or (string-ci=? str "no")
      (string-ci=? str "n")))

;; Char Nat -> (Str -> Str)
;; produce a function that adds copies of the specified character onto
;;    the end of a string until it is at least the specified length
(define (string-lengthener c len)
  (lambda (str)
    (string-append str
                   (make-string (max (- len (string-length str))
                                     0)
                                c))))

(start)
