#lang br/quicklang
;
; main entry point to the k3 package.
;
(require brag/support
         "lexer.rkt" "parser.rkt")

(module+ reader
  (provide read read-syntax get-info))

(define (read-syntax path port)
  (strip-bindings
   #`(module k3-module k3/expand
       #,(parse path (make-k3-lexer port path)))))

(define (read port)
  (syntax->datum (read-syntax "" port)))

(define (get-info port src-mod src-line src-col src-pos)
  (define (handle-query key default)
    (case key
      [(color-lexer)
       (dynamic-require 'k3/color 'k3-color)]
      [else default]))
  handle-query)