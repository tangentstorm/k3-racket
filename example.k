#lang k3/parse-only
/ test file that exercises k3 parser
/(two comments in a row.)

sort: {x@<x} / end of line comment
/ next line (and this one) have trailing \
rsum: +/\

\d interpreter command
csl:1_"",/",",/:$:   / train with monad at end
floor_floats:{@[x; &2=4::'x; _:]} / regression test
p:(123;`abc)
/ handling a,:[<case-stmt>] requires 1 char peek
`0:"abc",:[1;"d";"z"]
p,::[ 1 ; `t
   0 ; if[ 0 ; `f ]
]
.global: 3
try:@[dyadic;`a;2;:]
dyadic: {[a;b] a+b}; dyadic[32;3 4]
nil_adic: {[] .global + 2}
multi: { a:.global[_n]; 2 }

whitespace : {  [  abc  ]
                result:dyadic[32 ;3 4]
   :  result}

f:{/should ignore this paren: (
    y[x]}

`0: 5: rsum ints: !100
`symbols`are`"fun!"
`0: sort "hello world"
`0: ,$ 10 10 $ ints
`0: (2 * 3) + 4

*:'("hello";"world") / test adverb after monad
slash: "\\" /" comment
quote: "\"" /" comment

\
This text at the bottom should be ignored.
