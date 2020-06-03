#lang racket
(require "diagram.rkt" "constants.rkt")

(define current-state 1)
(define nfa-automata (dfa '() '(0 1) '() 0 '(1) ))

(define (create-new-states n)
  (define value-list (for/list ([k (in-range n)])
                       (set! current-state (+ current-state 1))
                       current-state))
  (set-dfa-states! nfa-automata (append (dfa-states nfa-automata) value-list))
  (apply values value-list))

(define (construct ast from to)
  (when (not (empty? ast))
    (construct-enfa ast from to)))

(define (construct-enfa ast from to)
  (define f (first ast))
  (define r (second ast))
  (cond
    [(eq? 'regex f) (construct r from to)]
    [(eq? 'union f) (union ast from to)]
    [(eq? 'simple f) (construct r from to)]
    [(eq? 'concat f) (concat ast from to)]
    [(eq? 'basic f) (basic r from to)]
    [(eq? 'elem f) (construct r from to)]
    [(eq? 'group f) (construct (third ast) from to)]
    [(eq? 'char f) (char r from to)]
    [(eq? 'dot f) (dot r from to)]))

(define (union ast from to)
  (construct (second ast) from to)
  (construct (fourth ast) from to))

(define (concat ast from to)
  (define-values (a) (create-new-states 1))
  (construct (second ast) from a)
  (construct (third ast) a to))

(define (basic ast from to)
  (if (eq? 'elem (first ast))
      (construct ast from to)
      (check-plus ast from to)))

(define (check-plus ast from to)
  (if (eq? 'plus (first ast))
      (plus ast from to)
      (quantifier ast from to)))

(define (plus ast from to)
  (set-dfa-transitions! nfa-automata (append (dfa-transitions nfa-automata)
                                             (list (list to from 'empty))))
  (construct (second ast) from to))

(define (quantifier ast from to)
  (define-values (a b) (create-new-states 2))
  (define t (cond
              [(eq? 'star (first ast)) (list (list to from 'empty) (list from to 'empty))]
              [(eq? 'q (first ast)) (list (list from to 'empty))]))
  (set-dfa-transitions! nfa-automata (append (dfa-transitions nfa-automata) t))
  (construct (second ast) from to))

(define (char ast from to)
  (set-dfa-transitions! nfa-automata (append (dfa-transitions nfa-automata) (list (list from to (first (string->list ast)))))))

(define (dot ast from to)
  (for ([a (dfa-alphabet nfa-automata)])
    (set-dfa-transitions! nfa-automata (append (dfa-transitions nfa-automata) (list (list from to a))))))

(define (reduce-transitions transitions)
  (filter (lambda (s) (not (eq? (third s) 'empty))) transitions))

(define (vectorize trans l)
  (for/fold ([v (make-vector (+ l 1) '())])
            ([t trans])
    (vector-set! v (first t) (cons t (vector-ref v (first t))))
    v))

(define (get-nfa regex-ast a)
  (set-dfa-alphabet! nfa-automata a)
  (construct regex-ast 0 1)
  (set-dfa-transitions! nfa-automata (vectorize (dfa-transitions nfa-automata) (length (dfa-states nfa-automata))))
  nfa-automata)

(provide get-nfa vectorize)