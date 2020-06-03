#lang br

(require "parser.rkt" "tokenizer.rkt" brag/support "min.rkt" "constants.rkt")

(define in (open-input-file "./testcases/testcase8.txt"))
(define alphabet (read-line in))
(define regex (string-append ".*(" (read-line in) ")"))
(define ast (parse-to-datum (apply-tokenizer-maker make-tokenizer regex)))
(define min-automata (get-min ast alphabet))
(define current-state (dfa-start min-automata))
(define matches '())

(define (next-line-it)
  (let ((line (read-line in)))
    (unless (eof-object? line)
      (set! current-state (dfa-start min-automata))
      (simulate ast (string->list alphabet) (string->list line))
      (next-line-it))))

(define (simulate regex-ast a line)
    (for ([l line])
      (set! current-state (next-state current-state l))
      (when (member current-state (dfa-end min-automata))
        (set! matches (append matches (list (list->string line)))))))

(define (next-state state sym)
  (for/first ([t (dfa-transitions min-automata)]
              #:when (and (eq? (first t) state)
                          (eq? (third t) sym)))
    (second t)))
(next-line-it)
(for ([m (remove-duplicates matches)])
      (println m))
(close-input-port in)