#lang racket/gui
(require drracket/tool)

(provide tool@)
(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define (phase1) (message-box "k3 IDE" "phase1"))
    (define (phase2) (message-box "k3 IDE" "phase2"))
    (message-box "k3 IDE" "unit invoked")))
