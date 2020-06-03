#lang racket
(require "diagram.rkt" "constants.rkt" "nfa.rkt")

(define (main-loop processed nfa transitions new-states table)
  (define-values (t s) (loop-all new-states nfa table))
  (define total (set-union processed new-states))
  (define left (set-subtract s total))
  (if (set-empty? left)
      (values (append t transitions) total)
      (main-loop total nfa (append t transitions) left table)))

(define (loop-all states nfa table)
  (for/fold ([l-t '()] [l-s (set)])
            ([st states])
    (define-values (t s) (transits st nfa table))
    (values (append t l-t) (set-union s l-s))))   

(define (transits state nfa table)
  (for/fold ([l-t '()] [l-s (set)])
            ([sym (dfa-alphabet nfa)])
    (define-values (t s) (transit state sym nfa table))
    (if (set-empty? s)
        (values l-t l-s)
        (values (cons t l-t) (set-union (set s) l-s)))))

(define (transit state sym nfa table)
  (define next-state
    (eclose-union (transit-set state sym nfa) nfa table))
  (values
   (list state next-state sym) next-state))

(define (transit-set state sym nfa)
  (for*/set ([s state]
             [t (vector-ref (dfa-transitions nfa) s)]
                                       #:when (eq? (third t) sym))
    (second t)))

(define (eclose-union state nfa table)
  (for/fold ([acc (set)])([s state])
    (set-union (hash-ref table s) acc)))

(define (eclose-all-states nfa)
  (for/hash ([s (dfa-states nfa)])
    (values s (eclose s '() nfa))))

(define (eclose s visited nfa)
  (for/fold ([acc (set s)])
            ([t (vector-ref (dfa-transitions nfa) s)]
             #:when (and (eq? 'empty (third t))
                         (not (member (second t) visited))))
    (set-union (eclose (second t) (cons (second t) visited) nfa)   
               (set-add acc (second t)))))

(define (reduce-transitions transitions)
  (filter (lambda (s) (not (eq? (third s) 'empty))) transitions))

(define (replace from to)
  (lambda (v)
    (if (equal? v from) to v)))

(define (reduce auto)
  (define new-state 0)
  (define states-list (set->list (dfa-states auto)))
  (define-values (trans st b e)
    (for/fold
     ([tr (dfa-transitions auto)][se states-list][start (dfa-start auto)][end (dfa-end auto)])
     ([s (dfa-states auto)])
      (set! new-state (+ new-state 1))
      (values
       (for/list ([t tr])
         (map (replace s new-state) t))
       (map (replace s new-state) se)
       ((replace s new-state) start)
       (map (replace s new-state) end))))
  (dfa (dfa-alphabet auto) st (vectorize trans (length st)) b e))

(define (get-dfa ast a)
  (define nfa (get-nfa ast a))
  (define table (eclose-all-states nfa))
  (define start (hash-ref table (dfa-start nfa)))
  (define-values (t s) (main-loop (set) nfa '() (set start) table))
  (define end (for/list ([state s] #:when (set-member? state (first (dfa-end nfa)))) state))
  (define done (reduce (dfa a s t start end)))
  done)

(provide get-dfa)