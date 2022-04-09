#lang racket
; count variables.
; this relies on the racket expander to walk the tree
;
(require k3/macro-style)
; !TODO: these are provided by k3/macro sytle and re-exported here:
; this should be simpler to make happen.
(provide k-code k-endnote k-comment k-command k-expr k-case k-flow k-assign k-ident k-call k-fsig k-func k-return)
  
(define idents (mutable-set))
(define assigned (mutable-set))
(define params (mutable-set))

(define (k-ident id) (set-add! idents id) id)
(define (k-assign id . args) (set-add! assigned id))
(define (k-fsig . args) (map (λ(x)(set-add! params x)) args))

; top-level evaluator
(define (k-code . args)
  (printf "found ~a parameters, ~a names, ~a assigned names.~n"
          (set-count params)
          (set-count idents)
          (set-count assigned)))

(eval-with 'k3/count-vars (read-k3-file "example.k"))
