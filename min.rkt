#lang racket
(require "diagram.rkt" "dfa.rkt" "constants.rkt")

(define min-automata (dfa '() '() '() '() '()))
(define dfa-automata (dfa '() '() '() '() '()))
(define new-fill #f)

;; construct-table
(define (construct-table a)
  (define states (dfa-states a))
  (define f (fill-first states states (make-hash) a))
  (define inv-to (invert-vector (dfa-transitions a) second))
  (define f2 (fill-it (set) (list->set (dfa-end a)) a f inv-to))
  (fill-everything states states f2 a))

(define (invert-vector vec kind)
  (for/fold ([v (make-vector (vector-length vec) '())])
   ([trans vec])
    (for ([t trans])
      (vector-set! v (kind t) (cons t (vector-ref v (kind t)))))
    v))

(define (fill-first x y ha auto)
  (for*/fold ([h ha])
             ([state-a y]
              [state-b x])
    (define state-a-end (member state-a (dfa-end auto)))
    (define state-b-end (member state-b (dfa-end auto)))
    (if (and (or state-a-end state-b-end)
             (not (and state-a-end state-b-end)))
        ; distinguishable
        (firstfill state-a state-b #f h)
        ; not distinguishable (blank)
        (firstfill state-a state-b #t h))))

(define (firstfill x y value h)
  (define key (if (< x y)(cons x y)(cons y x)))
  (hash-set! h key value)
  h)

(define (fill-it total new-states auto tab inv-to)
  (define-values (new new-tab)
    (for/fold ([t (set)]
               [new-tab tab])
              ([s new-states])
      (define adjacent (get-adjacent s tab inv-to))
      (define-values (n tabb)
        (for/fold ([ad (set)][ta tab])([a adjacent])
          (values (set-add ad (car a))
                  (set-disting (car a) (cdr a) s auto ta))))
      (values (set-union t n)
              tabb)))
  (define new-total (set-union total new-states))
  (define left (set-subtract new new-total))
  (if (set-empty? left)
      new-tab
      (fill-it new-total left auto new-tab inv-to)))

(define (get-adjacent s tab inv)
  (for/list ([t (vector-ref inv s)])
    (cons (first t) (third t))))

(define (set-disting state sym end auto tab)
  (for*/fold
   ([ta tab])
   ([t (dfa-transitions auto)]
    [l t]
    #:when (eq? sym (third l)))
    (if (not (notdistinguishable2? (second l) end ta))
        (fill2 (first l) state #f ta)
        ta)))

(define (notdistinguishable2? x y tab)
  (define key (if (< x y)(cons x y)(cons y x)))
  (or (eq? x y) (hash-ref tab key)))

(define (fill2 x y value tab)
  (define key (if (< x y)(cons x y)(cons y x)))
  (set! new-fill #t)
  (hash-set! tab key value)
  tab)

;; fill table
(define nr 0)

(define (fill-everything x y tab auto)
  (set! new-fill #f)
  (fill-rest x y tab auto)
  (if new-fill
    (fill-everything x y tab auto)
    tab))

(define (fill-rest x y tab auto)
  (define state-b (first x))
  (define new-tab
    (for*/fold ([ta tab])
               ([state-a y])
      (if (notdistinguishable? state-a state-b ta) 
        (inner-fill state-a state-b ta auto)
        ta)))
  (if (not (empty? (rest y)))
      (fill-rest (rest x) (rest y) new-tab auto)
      new-tab))

(define (inner-fill state-a state-b tab auto)
  (define d (for/last ([a (dfa-alphabet auto)] 
                             #:final (distinguishable? state-a state-b a tab))
                    break-value))
        (if d (fill state-a state-b #f tab) tab))

(define (fill x y value tab)
  (set! nr (+ nr 1))
  (define key (if (< x y)(cons x y)(cons y x)))
  (set! new-fill #t)
  (hash-set! tab key value)
  tab)

(define break-value #f)
(define (distinguishable? a b sym tab)
  (set! break-value (not (notdistinguishable? (next-state a sym)(next-state b sym) tab)))
  break-value)

(define (notdistinguishable? x y tab)
  (define key (if (< x y)(cons x y)(cons y x)))
  (or (eq? x y) (hash-ref tab key)))

;; construct-min-states
(define (construct-min-states table)
  (define already-member? #f)
  (for ([s (dfa-states dfa-automata)])
    (set! already-member? #f)
    (for ([m (dfa-states min-automata)])
      (when (member s m)
        (set! already-member? #t)))
    (when (not already-member?)
      (set-dfa-states! min-automata (append (dfa-states min-automata)
                                            (list (give-new-state s table)))))))

(define (give-new-state s table)
  (define new-state (list s))
  (for ([(k v) (in-hash table)]
        #:when v)
    (when (eq? (car k) s)
      (set! new-state (cons (cdr k) new-state)))
    (when (eq? (cdr k) s)
      (set! new-state (cons (car k) new-state))))
  new-state)

;; construct-min-transitions
(define (construct-min-transitions)
  (for* ([s (dfa-states min-automata)]
         [a (dfa-alphabet dfa-automata)])
    (set-dfa-transitions! min-automata (append (dfa-transitions min-automata) (list (list s (create-dest s a) a))))))

(define (create-dest state sym)
  (for/first ([s (dfa-states min-automata)]
              #:when (member (next-state (first state) sym) s))
    s))

(define (next-state state sym)
  (for/first ([t (vector-ref (dfa-transitions dfa-automata) state)]
              #:when (eq? (third t) sym))
    (second t)))

(define (set-start-state)
  (for ([s (dfa-states min-automata)]
        #:when (member (dfa-start dfa-automata) s))
    (set-dfa-start! min-automata s)))

(define (set-end-states)
  (for* ([s (dfa-states min-automata)]
         [e (dfa-end dfa-automata)]
         #:when (member e s))
    (set-dfa-end! min-automata (cons s (dfa-end min-automata))))
  (set-dfa-end! min-automata (remove-duplicates (dfa-end min-automata))))

(define (reduce auto)
  (define new-state 0)
  (for ([s (dfa-states auto)])
    (set! new-state (+ new-state 1))
    (set-dfa-transitions! auto (for/list ([t (dfa-transitions auto)])
                                 (map (replace s new-state) t)))
    (set-dfa-states! auto (map (replace s new-state) (dfa-states auto)))
    (set-dfa-start! auto ((replace s new-state) (dfa-start auto)))
    (set-dfa-end! auto (map (replace s new-state) (dfa-end auto))))
  auto)

(define (replace from to)
  (lambda (v)
    (if (and (list? v)
             (equal? (list->set v) (list->set from))) to v)))

(define (get-min regex-ast a)
  (set! dfa-automata (get-dfa regex-ast a))
  (set-dfa-alphabet! min-automata a)
  (define tab (construct-table dfa-automata))
  (construct-min-states tab)
  (construct-min-transitions)
  (set-start-state)
  (set-end-states)
  (reduce min-automata)
  min-automata)

(provide get-min)