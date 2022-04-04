#lang br/quicklang
;
; k3 expander
; this supplies the functions and macros that expand
; the parsed source k3 code into a racket expression.
(provide (rename-out [k3-module-begin #%module-begin])
         k-line k-endnote)

(define-macro (k3-module-begin (k-code K3 ...))
  #'(#%module-begin K3 ...))

#;(define-macro (k-comment ARG)
  #'(display ARG))

(define-macro (k-line ARG ...)
  #'(display #'(ARG ...)))

(define-macro (k-endnote ARG)
  #'(display ARG))
