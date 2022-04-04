#lang brag
;
; grammar for the k3 language
; (uses tokens defined in lexer.rkt)
;
k-code : k-line* k-endnote?
k-endnote : APPENDIX
@k-line : k-comment-line | k-stmt-line | k-meta-line | /NL
k-meta-line : "\\" .* /NL
@k-comment-line : k-comment /NL
k-stmt-line : k-expr k-comment? /NL
k-comment : COMMENT
@k-expr : (LNAME|PRIM|ADVERB|k-func|NUMBER|STRING|SYMBOL|":"|";")+
@k-block: k-line* k-expr?
k-func : "{" k-block "}"
