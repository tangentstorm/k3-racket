#lang br
;
; k3 lexer
;
(require brag/support)
(provide make-k3-lexer)

(define-lex-abbrevs
  [digit (char-set "0123456789")]
  [alpha alphabetic]
  [ident (:: (:? "_") alpha (:* (:or alpha digit "_")))]
  [lname (:: ident (:* (:: "." ident)))]
  [gname (:+ (:: "." ident))]
  [prim (:: (char-set "_+-*%!&|<>=~,^#$?@."))]
  ; string escapes in k3 are officially "the same as in the c language"
  ; with the following specific examples given: \b backspace \n newline
  ; \t tab \" doublequote \\ backslash  \o \oo \ooo octal numbers.
  ; \r for carriage may be undocumented. Any other character can
  ; also be escaped, in which case the backslash is ignored.
  ; so this lexer preserves the backslashes and just includes the entire
  ; ascii character set (:/ "\0" "~")
  [string  (:: "\"" (:* (:or (:: "\\" (:/ "\0" "~"))
                             (:~ (char-set "\\\"")))) "\"")])


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
       [(:or "{" "}" "(" ")" ":[" "[" "]" ";" ":") (token lexeme lexeme)]
       [(:or "if[" "while[" "do[") (token lexeme lexeme)]
       [(:or "'" "':" "/" "/:" "\\" "\\:") (token 'ADVERB lexeme)]
       [(:: (:or prim ":") ":")
        ; have to peek one character to deal with things like: a,:[case-expr]
        (begin
          (if (and (equal? (substring lexeme 1) ":")
                   (equal? (peek-char input) #\[))
              ; (unget-char! input) was not needed. seems setting the span is enough.
              (token 'PRIM (substring lexeme 0 (sub1 (string-length lexeme))))
              (token 'PRIMCOLON lexeme)))]
       [prim (token 'PRIM lexeme)]
       ; numeric primitives are differente because they're never part of augmented assignment:
       [(:: digit ":" (:? ":")) (token 'NUMCOLON lexeme)]
       [(:: (:? "-") (:+ digit) (:? (:: "." (:* digit)))) (token 'NUMBER lexeme)]
       [string (begin (token 'STRING lexeme))]
       [lname (token 'LNAME lexeme)]
       [gname (token 'GNAME lexeme)]
       ;       [(:: "_" ident) (token 'BUILTIN lexeme)]
       ;       [(:: "_" (:? ":")) (token 'PRIM lexeme)]
       [(:: "`" (:? (:or lname gname string))) (token 'SYMBOL lexeme)]))
    
    (cond [at-slash-token?
           (begin
             (set! at-slash-token? #f)
             (slash-lexer input))]
          [else (k3-lexer input)]))
  next-token)
