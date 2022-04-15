#lang brag
;
; grammar for the k3 language
; (uses tokens defined in lexer.rkt)
;
k-code : k-line* k-stmts? k-endnote?
k-endnote : ENDNOTE
@k-line : k-comment-line | k-command-line | k-stmt-line | /NL
@k-command-line: k-command /NL
@k-comment-line: k-comment /NL
k-comment : COMMENT
k-command : COMMAND
@k-stmt-line : k-stmts? k-comment? /NL
@k-stmts : k-expr? (/";" k-expr?)*
k-expr  : ":"|k-return|k-chain
@k-chain : (k-case|k-flow|k-assign|PRIM|PRIMCOLON|NUMCOLON|BUILTIN|ADVERB|k-func|k-call|k-nest|NUMBER|STRING|SYMBOL|k-lvalue)+ PRIMCOLON?
k-case : /":[" k-block /"]"
k-flow : ("if["|"while["|"do[") k-block /"]"
@k-nest : /"(" k-block /")"
@k-block: k-line? (/NL* k-line?)* k-stmts?
k-assign : k-lvalue (k-call)* (":"|PRIMCOLON) k-expr
@k-lvalue : k-ident k-call?
k-ident : LNAME|GNAME
k-call : /"[" k-block /"]"
k-fsig : /"[" LNAME? (/";" LNAME)* /"]"
k-func : /"{" k-fsig? k-block /"}"
k-return : /":" k-expr
