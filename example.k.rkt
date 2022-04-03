#lang k3
/ test file that exercises k3 parser

sort: {x@<x}
rsum: +/\
`0: 5: rsum ints: !100
`symbols`are`"fun!"
`0: sort "hello world"
`0: ,$ 10 10 $ ints

/ TODO: allow the \ below to work without two newlines

\
This text at the bottom should be ignored.