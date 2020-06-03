#lang br/quicklang
(require brag/support)

(define (make-tokenizer port)
  (define (next-token)
    (define auto-lexer
      (lexer
       [(char-set ".|*+?()") lexeme]
       [any-char (token 'CHAR-TOK lexeme)]))
    (auto-lexer port))  
  next-token)
(provide make-tokenizer)