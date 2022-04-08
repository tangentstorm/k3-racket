#lang racket
(provide k-code k-expr)
(define (k-expr . args) `(dr:K-EXPR ,@args))
(define (k-code . args) `(dr:K-CODE ,@args))
