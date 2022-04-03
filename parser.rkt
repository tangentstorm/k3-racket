#lang brag
;
; grammar for the k3 language
; (uses tokens defined in lexer.rkt)
;
k-code : /NL? k-item* APPENDIX?
@k-item : k-line | k-meta
k-meta : "\\" .* /NL+
k-line : k-expr? k-comment? /NL+
k-comment : COMMENT
@k-expr : (LNAME|PRIM|ADVERB|k-func|NUMBER|STRING|SYMBOL|":"|";")+
@k-block: k-item* k-expr?
k-func : "{" k-block "}"
