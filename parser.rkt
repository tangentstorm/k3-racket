#lang brag
;
; grammar for the k3 language
; (uses tokens defined in lexer.rkt)
;
k-code : k-line* k-endnote?
k-endnote : ENDNOTE
@k-line : k-comment-line | k-command-line | k-stmt-line | /NL
@k-command-line: k-command /NL
@k-comment-line: k-comment /NL
k-comment : COMMENT
k-command : COMMAND
@k-stmt-line : k-expr k-comment? /NL
k-expr : (LNAME|PRIM|ADVERB|k-func|NUMBER|STRING|SYMBOL|":"|";")+
@k-block: k-line* k-expr?
k-func : "{" k-block "}"
