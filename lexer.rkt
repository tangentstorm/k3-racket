#lang br
;
; k3 lexer
;
(require brag/support)
(provide k3-lexer)

(define-lex-abbrevs
  [digit (char-set "0123456789")]
  [alpha alphabetic]
  [ident (:: alpha (:* (:or alpha digit "_")))]
  [lname (:: ident (:* (:: "." ident)))]
  [gname (:+ (:: "." ident))]
  [string (from/to "\"" "\"")])


(define k3-lexer
  (lexer-srcloc
   ["\n" (token 'NL lexeme)]
   [(:: "\n\\" (:? whitespace) "\n" (:* (:~ "\0")))
    (token 'APPENDIX lexeme)]
   [(from/stop-before (:: (:or " " "\t" "\n") "/") "\n")
    (token 'COMMENT
           (substring lexeme 1 (string-length lexeme)))]
   [whitespace (token lexeme #:skip? #t)]
   [(:or "'" "':" "/" "/:" "\\" "\\:") (token 'ADVERB lexeme)]
   [(:or "{" "}" "(" ")" "[" "]" ";" ":") (token lexeme lexeme)]
   [(:or (:: digit ":") "::"
         (:: (char-set "+-*%!&|<>=~,^#_$?@.") (:? ":")))
    (token 'PRIM lexeme)]
   [(:: (:? "-") (:+ digit) (:? (:: "." (:* digit)))) (token 'NUMBER lexeme)]
   [string (token 'STRING lexeme)]
   [lname (token 'LNAME lexeme)]
   [gname (token 'GNAME lexeme)]
   [(:: "`" (:? (:or lname gname string))) (token 'SYMBOL lexeme)]))
