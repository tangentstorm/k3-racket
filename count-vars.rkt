#lang racket
; count variables.
; this relies on the racket expander to walk the tree
;
(require k3/macro-style)
; !TODO: these are provided by k3/macro sytle and re-exported here:
; this should be simpler to make happen.
(provide k-code k-endnote k-comment k-command k-case k-expr k-flow k-assign k-ident k-call k-fsig k-func k-return)

(define idents (mutable-set))
(define assigned (mutable-set))
(define params (mutable-set))
(define lambdas (mutable-set))

(define (unwrap-single sym args)
  (if (= 1 (length args)) (first args) `(,sym ,@args)))

(define (k-ident id) (set-add! idents id) id)
(define (k-expr . args) (unwrap-single 'k-expr args))
(define (k-func . args) 'LAMBDA)
(define (k-fsig . args) (map (λ(x)(set-add! params x)) args))

(define (k-assign id op . val)
  (set! val (unwrap-single 'args val))
  ;(printf "set! ~a ~a ~a\n" id op val)
  (set-add! assigned id)
  (when (eq? val 'LAMBDA)
    (when (set-member? lambdas id)
      (printf "lambda ~a shadows another definition!\n" id))
    (set-add! lambdas id)))

; top-level evaluator
(define (k-code . args)
  (printf "found ~a parameters, ~a names, ~a assigned names ~a lambdas.~n"
          (set-count params)
          (set-count idents)
          (set-count assigned)
          (set-count lambdas)))

;(eval-with 'k3/count-vars (read-k3-file "example.k"))
(provide reset-counts! lambdas idents assigned params)
(define (reset-counts!)
  (set! idents (mutable-set))
  (set! assigned (mutable-set))
  (set! params (mutable-set))
  (set! lambdas (mutable-set)))
