#lang racket
(provide (all-defined-out))

(struct dfa (alphabet states transitions start end) #:mutable #:transparent)