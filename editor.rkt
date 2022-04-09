#lang racket/gui
(require framework)
(require k3/color)

(define (find-color c) (send the-color-database find-color c))
;(editor:set-default-font-color (find-color "Red") (find-color "Yellow"))

(define styles (new style-list%)) ; includes "Basic"
(define Syntax (send styles new-named-style "Syntax" (send styles basic-style)))
(let [(delta (new style-delta%))]
  (send delta set-delta-face "Consolas")
  ;(send delta set-delta-background "#111122")
  (send delta set-delta 'change-size 12)
  (send Syntax set-delta delta))


(define (add-style name color)
  (let ([new-style (send styles new-named-style name Syntax)]
        [delta (new style-delta%)])
    (send new-style set-base-style Syntax)
    (send delta set-delta-foreground (find-color color))
    (send new-style set-delta delta)
    new-style))

(add-style "string" "Olive")
(add-style "symbol" "Dark Goldenrod")
(add-style "constant" "Steel Blue")
(add-style "comment" "Dark Magenta")
(add-style "keyword" "Dark Green")
(add-style "hash-colon-keyword" "Peru")
(add-style "parenthesis" "Dark Slate Gray")
(add-style "other" "Magenta")
(add-style "Standard" "Black")

(define k-path "example.k")
(define win (new frame% [label k-path] [width 1024] [height 768]))
(define ed (new editor-canvas% (parent win)))
(define txt (new color:text%))
(send ed set-editor txt)
(send txt load-file k-path)
(send txt set-style-list styles)

(define (token-sym->style sym)
  (symbol->string sym))
(define pairs '((|(| |)|) (|[| |]|) (|{| |}|)))
(send txt start-colorer token-sym->style k3-color pairs)


(send win show #t)

