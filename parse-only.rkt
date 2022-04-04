#lang br/quicklang
; #lang k3/parse-only
; provides a reader and expander that
; simply shows the parse tree
(require "parser.rkt" "lexer.rkt")
(provide (rename-out [parser-only-mb #%module-begin]))
(module+ reader (provide read-syntax))

(define (read-syntax path port)
  (strip-bindings
   #`(module k3-parser-mod k3/parse-only
       #,(parse path (make-k3-lexer port path)))))

(define-macro (parser-only-mb PARSE-TREE)
  #'(#%module-begin
     'PARSE-TREE))

