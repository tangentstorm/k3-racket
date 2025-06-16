#lang info
(define collection "k3")
(define deps '("base" "brag-lib" "beautiful-racket-lib" "gui-easy-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/k3.scrbl" ())))
(define pkg-desc "k3 language support for racket")
(define pkg-authors '(tangentstorm))
(define version "0.1")
(define license '(MIT))
