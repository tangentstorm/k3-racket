#lang br
;
; k3 lexer
;
(require brag/support)
(provide make-k3-lexer)

(define-lex-abbrevs
  [digit (char-set "0123456789")]
  [alpha alphabetic]
  [ident (:: alpha (:* (:or alpha digit "_")))]
  [lname (:: ident (:* (:: "." ident)))]
  [gname (:+ (:: "." ident))]
  [string (from/to "\"" "\"")])


(define (unget-char! port)
  (file-position port (- (file-position port) 1)))

; if this is inside make-k3-lexer, it seems to be
; completely ignored by the colorizer
; (which presumably reconstructs the lexer for each line?)
(define at-slash-token? #f)

(define (make-k3-lexer input [path #f])
  (port-count-lines! input)
  (lexer-file-path path)
  (define (next-token)
 
    ; -- slash lexer --
    ; the "/" character starts a comment in k, but only
    ; when preceded by whitespace or at the start of the file.
    ; otherwise it's an adverb, so we need to distinguish that.
    ; (same with "\" for commands)
    (define slash-lexer
      (lexer-srcloc
       [(from/stop-before "/" "\n")
        (token 'COMMENT lexeme)]
       [(:: "\\" (:? whitespace) "\n" (:* (:~ "\0")))
        (token 'ENDNOTE lexeme)]
       [(from/stop-before "\\" "\n")
        (token 'COMMAND (substring lexeme 1 (string-length lexeme)))]))

    (define (check-for-slash)
      (define (is-slash c) (or (eq? #\\ c) (eq? #\/ c)))
      (let [(next (peek-char input))]
        (when (is-slash next)
          (set! at-slash-token? #t))))

    ; -- all other tokens --
    (define k3-lexer
      (lexer-srcloc
       ["\n" (begin (check-for-slash) (token 'NL lexeme))]
       [whitespace (begin (check-for-slash) (token lexeme #:skip? #t))]
       [(:or "'" "':" "/" "/:" "\\" "\\:") (token 'ADVERB lexeme)]
       [(:or "{" "}" "(" ")" "[" "]" ";" ":") (token lexeme lexeme)]
       [(:or (:: digit ":") "::"
             (:: (char-set "+-*%!&|<>=~,^#_$?@.") (:? ":")))
        (token 'PRIM lexeme)]
       [(:: (:? "-") (:+ digit) (:? (:: "." (:* digit)))) (token 'NUMBER lexeme)]
       [string (begin (token 'STRING lexeme))]
       [lname (token 'LNAME lexeme)]
       [gname (token 'GNAME lexeme)]
       [(:: "`" (:? (:or lname gname string))) (token 'SYMBOL lexeme)]))
    
    (cond [at-slash-token?
           (begin
             (set! at-slash-token? #f)
             (slash-lexer input))]
          [else (k3-lexer input)]))
  next-token)
