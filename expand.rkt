#lang br/quicklang
;
; k3 expander
; this supplies the functions and macros that expand
; the parsed source k3 code into a racket expression.
;
; there should be one definition here for every rule
; in parser.rkt that does not have a '@' in front of the name.
; (the '@' indicates that it gets inlined)

(provide (rename-out [k3-module-begin #%module-begin])
         (matching-identifiers-out #rx"^k-" (all-defined-out)))

(define-macro (k3-module-begin (k-code K3 ...))
  #'(#%module-begin K3 ...))

(define (k-comment ARG) (void))
(define (k-endnote ARG) (void))
(define (k-command ARG) (printf "!! k interpreter command: ~a ~n" ARG))

(define-macro (k-expr ARG ...)
  #'(displayln '(k-expr ARG ...)))
