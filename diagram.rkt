#lang racket

(require "constants.rkt")

(define al #\a)
(define st 'empty)

(define test-dfa1 (dfa
   '(#\a #\b #\c)
   '(0 1 2 3)
   '((0 2 empty) (2 3 empty) (3 1 #\a) (2 1 #\b) (0 1 #\c))
   0
   '(1)
   ))

(define test-dfa2 (dfa
   '(#\a #\b)
   '(0 1 2 3 4 5)
   '((0 2 empty) (0 3 empty) (4 1 empty) (5 1 empty) (2 4 #\a) (3 5 #\b))
   0
   '(1)
   ))

(define (dfa-graph test-dfa)
  (fprintf (current-output-port)
           "~a\n"
           '\#states)
  (for ([state (dfa-states test-dfa)])
    (write 's)
    (println state))

  (fprintf (current-output-port)
           "~a\n"
           '\#initial)
  (write 's)
  (println (dfa-start test-dfa))

  (fprintf (current-output-port)
           "~a\n"
           '\#accepting)
  (for ([end (dfa-end test-dfa)])
    (write 's)
    (println end))

  (fprintf (current-output-port)
           "~a\n"
           '\#alphabet)
  (for ([a (dfa-alphabet test-dfa)])
    (fprintf (current-output-port) "~a\n" a))

  (fprintf (current-output-port)
           "~a\n"
           '\#transitions)
  (for ([t (dfa-transitions test-dfa)])
    ;(write (first t))
    (write 's)
    (fprintf (current-output-port) "~a" (first t))
    (write ':)
    ;(write (third t))
    ;(fprintf (current-output-port) "~a" (third t))
    (if (eq? (third t) 'empty)
        (fprintf (current-output-port) "~a" '\$)
        (fprintf (current-output-port) "~a" (third t)))
    (write '>)
    ;(writeln (second t))
    (write 's)
    (fprintf (current-output-port) "~a\n" (second t))
    )
  )

(provide dfa-graph)