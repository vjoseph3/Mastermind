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
    (make-message (stringify-list (mastermind-current-guess board))))

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
               (cond [(= (choice-index r-choice) code-length) ; done
                      (define task3
                        (display (summary-message
                                  (mastermind-guess-history board))))
                      (hide-window window)]
                     [(mastermind-impossible? board)
                      (define task4 (hide-window window))
                      (error "No solution found")]
                     [else
                      (and (draw-message
                            guess-message
                            (stringify-list
                             (mastermind-current-guess board)))
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
;; (stringify-list lst) produces a string of the numbers in lst.

;; stringify-list: (listof Num) -> Str
(define (stringify-list lst)
  (cond [(empty? lst) ""]
        [(empty? (rest lst)) (number->string (first lst))]
        [else
         (foldr string-append ""
                (cons (number->string (first lst))
                      (map (lambda (item)
                             (string-append " "
                                            (number->string item)))
                           (rest lst))))]))
