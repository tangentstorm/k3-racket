#lang br
;
; colorizer for k3 in dr-racket.
; based on the one for basic from 'beautiful racket'
;
(require "lexer.rkt" brag/support)
(provide k3-color)

(define (k3-color port)
  (define (handle-lexer-error excn)
    (define excn-srclocs (exn:fail:read-srclocs excn))
    (srcloc-token (token 'ERROR) (car excn-srclocs)))

  (define (safe-fallback)
    ; Return safe values when lexer fails (e.g., when expanding collapsed s-expressions)
    ; Match the EOF case pattern exactly
    (values 'eof 'eof #f #f #f))

  ; Handle the case where Framework passes non-character data during s-expression expansion
  (with-handlers ([exn:fail:read? (lambda (e) (safe-fallback))]
                  [exn:fail? (lambda (e) (safe-fallback))])
    (define srcloc-tok
      ((make-k3-lexer port)))
    (match srcloc-tok
      [(? eof-object?) (values srcloc-tok 'eof #f #f #f)]
      [else
       (match-define
         (srcloc-token
          (token-struct type val _ _ _ _ _)
          (srcloc _ _ _ posn span)) srcloc-tok)
       ; (printf ">> ~a ~a ~a ~n" posn type val)
       (define start posn)
       (define end (+ start span))
       (match-define (list cat paren)
         (match type
           ['STRING '(string #f)]
           ['SYMBOL '(constant #f)]
           ['COMMENT '(comment #f)]
           ['ENDNOTE '(comment #f)]
           ['COMMAND '(other #f)]
           ['BUILTIN '(symbol #f)]
           ['ERROR '(error #f)]
           ['PRIM '(symbol #f)]
           ['NUMCOLON '(symbol #f)]
           ['PRIMCOLON '(symbol #f)]
           ['ADVERB '(keyword #f)]
           ['NUMBER '(constant #f)]
           ['GNAME '(hash-colon-keyword #f)]
           [else (match val
                   [(? number?) '(constant #f)]
                   [";" '(parenthesis #f)]
                   ["(" '(parenthesis |(|)]
                   [")" '(parenthesis |)|)]
                   ["{" '(parenthesis |{|)]
                   ["}" '(parenthesis |}|)]
                   ["[" '(keyword |[|)]
                   ["]" '(keyword |]|)]
                   [":" '(keyword #f)]
                   [":[" '(keyword |[|)]
                   ["if[" '(keyword |[|)]
                   ["do[" '(keyword |[|)]
                   ["while[" '(keyword |[|)]
                   [else '(no-color #f)])]))
       (values val cat paren start end)])))
