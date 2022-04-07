#lang racket
;
; the goal here is to provide a default expander
; for a k parse tree that basically does nothing.
;
; for each kind of node, there is a corresponding
; macro and function definition.
; the macro just calls the fuction, which
; re-creates the original s-expression,
; so by default, expansion is a no-op.
;
; !!! (it's a n-op other than losing syntax information, which i don't know how to fix yet)
;
; to implement a "visitor", you just require this
; module, override the definitions you want to
; change (override the macro if you want to adjust the expander,
; or the function if you want to override the evaluation)
;
; then follow macro-style-example.rkt for how to use it.
;
(provide
 k-code k-endnote k-comment k-command k-expr k-case k-flow k-assign k-ident k-call k-fsig k-func k-return)

(define (K tag body) (list tag body))

; !! HELP. I don't know how to make a macro that makes macros.
(define-syntax-rule (k-code args ...)    (K-CODE (list args ...)))
(define-syntax-rule (k-endnote args ...) (K-ENDNOTE (list args ...)))
(define-syntax-rule (k-comment args ...) (K-COMMENT (list args ...)))
(define-syntax-rule (k-command args ...) (K-COMMAND (list args ...)))
(define-syntax-rule (k-expr args ...)    (K-EXPR (list args ...)))
(define-syntax-rule (k-case args ...)    (K-CASE (list args ...)))
(define-syntax-rule (k-flow args ...)    (K-FLOW (list args ...)))
(define-syntax-rule (k-assign args ...)  (K-ASSIGN (list args ...)))
(define-syntax-rule (k-ident args ...)   (K-IDENT (list args ...)))
(define-syntax-rule (k-call args ...)    (K-CALL (list args ...)))
(define-syntax-rule (k-fsig args ...)    (K-FSIG (list args ...)))
(define-syntax-rule (k-func args ...)    (K-FUNC (list args ...)))
(define-syntax-rule (k-return args ...)  (K-RETURN (list args ...)))

; !! ALSO HELP. I don't know how to make a macro that upcases the name.
(define (K-CODE args)      (cons 'k-code args))
(define (K-ENDNOTE args)   (cons 'k-endnote args))
(define (K-COMMENT args)   (cons 'k-comment args))
(define (K-COMMAND args)   (cons 'k-command args))
(define (K-EXPR args)      (cons 'k-expr args))
(define (K-CASE args)      (cons 'k-case args))
(define (K-FLOW args)      (cons 'k-flow args))
(define (K-ASSIGN args)    (cons 'k-assign args))
(define (K-IDENT args)     (cons 'k-ident args))
(define (K-CALL args)      (cons 'k-call args))
(define (K-FSIG args)      (cons 'k-fsig args))
(define (K-FUNC args)      (cons 'k-func args))
(define (K-RETURN args)    (cons 'k-return args))
