#lang k3/parse-only
/ test file that exercises k3 parser
/(two comments in a row.)

sort: {x@<x} / end of line comment
/ next line (and this one) have trailing \
rsum: +/\

\d interpreter command

`0: 5: rsum ints: !100
`symbols`are`"fun!"
`0: sort "hello world"
`0: ,$ 10 10 $ ints

\
This text at the bottom should be ignored.
