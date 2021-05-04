;; -------------------------------------------------------------------
;;    Joseph Vendryes
;;    May 2021
;;    Mastermind v2 GUI
;; -------------------------------------------------------------------

#lang racket

(require "mastermind.rkt")
(require htdp/gui)

;; (mind code-length pool-size) guesses at the user's secret code,
;;    which is of length code-length and contains colours less than
;;    pool-size.

;; mind: Nat Nat -> true
;; Requires:
;;    code-length > 0
;;    pool-size > 0
(define (mind code-length pool-size)
  (define counter 1)

  (define mark-message "Enter mark")

  (define board (mastermind-first-play code-length pool-size))

  (define choices (map number->string
                       (build-list (add1 code-length)
                                   (lambda (n) n))))

  (define r-choice (make-choice choices))
  (define w-choice (make-choice choices))

  (define progress (make-message (progress-message counter)))
  (define guess-message
    (make-message
     (stringify-attempt (mastermind-board-guess board))))

  (define window
    (create-window
     (list
      (list progress)
      (list guess-message)
      (list r-choice w-choice)
      (list (make-button
             mark-message
             (lambda (ignore)
               (set! board (mastermind-play board
                                            (choice-index r-choice)
                                            (choice-index w-choice)))
               (set! counter (add1 counter))
               (cond [(mastermind-game-over? board)
                      (define task3
                        (display (summary-message
                                  (mastermind-board-history board))))
                      (hide-window window)]
                     [(mastermind-impossible? board)
                      (define task4 (hide-window window))
                      (error "No solution found")]
                     [else
                      (and (draw-message guess-message
                               (stringify-attempt
                                (mastermind-board-guess board)))
                           (draw-message
                            progress
                            (progress-message counter)))])))))))

  true)


;; ---------------
;; (progress-message n) produces a message indicating this is the n-th
;;    attempt.
;; Examples:
;(check-expect (progress-message 1) "Attempt 1:")
;(check-expect (progress-message 10) "Attempt 10:")

;; progress-message: Nat -> Str
(define (progress-message n)
  (string-append "Attempt "
                 (number->string n)
                 ":"))


;; ---------------
;; (summary-message history) produces a summary message from history.
;; Examples:
;(check-expect (summary-message '(((0 1 2 3))))
;              "Used 1 try\n")
;(check-expect (summary-message '(((1 2 3) (0)) ((0 1 2 3))))
;              "Used 2 tries\n")

;; summary-message: History -> Str
(define (summary-message history)
  (string-append "Used "
                 (number->string (length history))
                 (if (= 1 (length history))
                     " try"
                     " tries")
                 "\n"))



;; ---------------
;; (stringify-attempt attempt) converts attempt to a readable string.
;; Examples:
;(check-expect (stringify-attempt '((0 1 2 3))) " 0 0 0 0")
;(check-expect (stringify-attempt '((1 2 3) (0))) " 0 1 1 1")
;(check-expect
; (stringify-attempt '((8) (2) (7) (6) (5) (1) (4) (10) (0) (9) (3)))
; " 2 5 9 0 4 6 7 8 10 1 3")

;; stringify-attempt: Attempt -> Str
(define (stringify-attempt attempt-arg)
  (define attempt (map (lambda (mouth) (filter number? mouth))
                       attempt-arg))
  (define (attempt->list attempt)
    (define labelled
      (let loop ([attempt attempt])
        (cond [(empty? attempt) empty]
              [else
               (cons (map (lambda (index)
                            (list index
                                  (length
                                   (rest attempt))))
                          (first attempt))
                     (loop (rest attempt)))])))

    (define sorted
      (quicksort (foldr append empty labelled)
                 (lambda (pair1 pair2)
                   (< (first pair1) (first pair2)))))

    (map second sorted))

  (define (stringify-list lst)
    (foldr string-append ""
           (map (lambda (colour)
                  (string-append " " (number->string colour)))
                lst)))

  (stringify-list (attempt->list attempt)))


;; ---------------
;; (quicksort lst precedes?) produces a list containing the elements
;;    of lst sorted into the order imparted by precedes?
(define (quicksort lst precedes?)
  (cond [(empty? lst) empty]
        [else
         (define pivot (first lst))
         (define before (filter (lambda (lst-item)
                                  (precedes? lst-item pivot))
                                (rest lst)))
         (define after (filter (lambda (lst-item)
                                 (not (precedes? lst-item pivot)))
                               (rest lst)))
         (append (quicksort before precedes?)
                 (list pivot)
                 (quicksort after precedes?))]))
