#lang racket
;
; this isn't really a linter yet. it does walk the entire parse tree
; and look for interesting 
;
(require syntax/stx rnrs/io/ports-6 (submod k3 reader))


; we want stx rather than list because we want to
; know on which lines the assignments took place
(define (walk visit stx-node)
  (define (step stx-node)
    (define node (syntax-e stx-node))
    (cond
      [(list? node)
       (define head (syntax-e (first node)))
       (when (symbol? head)
         (visit head (rest node)))
       (map step node)]
      [(string? node) node]
      [(symbol? node) node]))
  (step stx-node))
 
(define idents (mutable-set))
(define assigned (mutable-set))
(define params (mutable-set))

(define (visit head tail)
  (match head
    ['k-fsig   (map (λ (x) (set-add! params (syntax-e x))) tail)] 
    ['k-ident  (set-add! idents (syntax-e (car tail)))]
    ['k-assign (set-add! assigned (syntax-e (car tail)))]
    [else 'ok]))


(define (run-linter path)
  (define port (open-file-input-port path))
  (define ast (fourth (stx->list (read-syntax path port))))
  (void (walk visit ast))
  (printf "found ~a parameters, ~a names, ~a assigned names.~n"
          (set-count params)
          (set-count idents)
          (set-count assigned)))

;(run-linter "mlw-interact.k")
;(run-linter "example.k.rkt")
(run-linter "hello.k")