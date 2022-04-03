#lang br/quicklang
;
; main entry point to the k3 package.
;
(require brag/support
         "lexer.rkt" "parser.rkt")

(module+ reader
  (provide read-syntax get-info))

(define (make-tokenizer input [path #f])
  (port-count-lines! input)
  (lexer-file-path path)
  (λ () (k3-lexer input)))

(define (read-syntax path port)
  (strip-bindings
   #`(module k3-module k3/expand
       #,(parse path (make-tokenizer port path)))))

(define (get-info port src-mod src-line src-col src-pos)
  (define (handle-query key default)
    (case key
      [(color-lexer)
       (dynamic-require 'k3/color 'k3-color)]
      [else default]))
  handle-query)