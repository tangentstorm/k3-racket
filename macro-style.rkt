#lang racket
;
; the goal here is to provide a default expander
; for a k parse tree that basically does nothing.
;
; for each kind of node, there is a corresponding
; function definition, which simply re-creates the
; original s-expression.
;
; so by default, expansion is a no-op. other than losing syntax information
; !!! (i would prefer to keep syntax info but don't know how to fix this yet)
; (using define-syntax would do that, but you can't (??) have syntax that expands
; to its own input or else you'll get an infinite loop.)
;
; to implement a "visitor", you just require this
; module, override the definitions you want to
; change (create a macro if you want to adjust the expander,
; or replace the function if you want to override the evaluation)
;
; then follow macro-style-example.rkt for how to use it.
;
(require (for-syntax racket/syntax))

(define-syntax-rule (default-handler name)
  (define (name . args) (cons 'name args)))

(define-syntax (provide-default-handlers stx)
  (syntax-case stx ()
    [(_ names ...)
     (syntax/loc stx
       (begin
         (provide names ...)
         (default-handler names) ...))]))

(provide-default-handlers
  k-code k-endnote k-comment k-command k-expr k-case k-flow k-assign k-ident k-call k-fsig k-func k-return)
