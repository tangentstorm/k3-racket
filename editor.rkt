#lang racket/gui
(require framework racket/gui/easy)
(require k3/color k3)
(require syntax/stx)

(define k-path "example.k")

; --  syntax tree support  ----------------------------------------

(define (pos-in-syntax? pos stx)
  (let* [(s-pos (syntax-position stx))
         (s-end (+ s-pos (syntax-span stx)))]
    (and (<= s-pos pos)
         (< pos s-end))))

; given a position, find a token and all its parents in the syntax tree.
(define (find-token stx pos)
  (define (find-token-rev stx pos)
    (if (stx-list? stx)
        (let [(f (findf (curry pos-in-syntax? pos)
                        (rest (syntax-e stx))))]
          (cons stx (find-token-rev f pos)))
        (list stx)))
  (reverse (find-token-rev stx pos)))


; -- syntax colors  -----------------------------------------

(define (find-color c) (send the-color-database find-color c))
;(editor:set-default-font-color (find-color "Red") (find-color "Yellow"))

(define styles (new style-list%)) ; includes "Basic"
(define Syntax (send styles new-named-style "Syntax" (send styles basic-style)))
(let [(delta (new style-delta%))]
  (send delta set-delta-face "Consolas")
  (send delta set-delta 'change-size 12)
  (send Syntax set-delta delta))

(define (add-style name color)
  (let ([new-style (send styles new-named-style name Syntax)]
        [delta (new style-delta%)])
    (send new-style set-base-style Syntax)
    (send delta set-delta-foreground (find-color color))
    (send new-style set-delta delta)
    new-style))

(void
 (add-style "string" "Olive")
 (add-style "symbol" "Dark Goldenrod")
 (add-style "constant" "Steel Blue")
 (add-style "comment" "Dark Magenta")
 (add-style "keyword" "Dark Green")
 (add-style "hash-colon-keyword" "Peru")
 (add-style "parenthesis" "Dark Slate Gray")
 (add-style "other" "Magenta")
 (add-style "Standard" "Black"))

(define k3-text% ; plain gui framework
  (class color:text%
    (super-new)

    (define/override (on-local-event e)
      (super on-local-event e)
      (define ex (send e get-x))
      (define ey (send e get-y))
      (define-values (edx edy) (send this dc-location-to-editor-location ex ey))
      (define pos (send this find-position edx edy))
      (define ch (send this get-character pos))
      (define snip (send this find-snip pos 'after))
      (when (eq? 'left-down (send e get-event-type))
        (printf "click @ pos:~a ch:~a snip:~a\n" pos ch snip)
        (for [(node (find-token ast pos))] (displayln node))))))

(define k3-view% ; for easy-gui
  (class* object% (view<%>)
    (init-field path)
    (super-new)

    (define/public (dependencies) '())

    (define/public (create parent)
      (define ed (new editor-canvas% (parent parent)))
      (define txt (new k3-text%))
      (send ed set-editor txt)
      (define (token-sym->style sym) (symbol->string sym))
      (define pairs '((|(| |)|) (|[| |]|) (|{| |}|)))
      (send txt start-colorer token-sym->style k3-color pairs)
      (void (send txt load-file k-path))
      (send txt set-style-list styles))

    (define/public (update v what val)
      (void))
    (define/public (destroy v)
      (void))))

(define (k3-view path)
  (new k3-view% [path path]))


; -- main program ------------------------------------------------

(define win
  (window
   #:title k-path #:size '(1024 768)
   (k3-view k-path) ))

;(define ast (read-k3 k-path (open-input-string (send txt get-text 0 'eof))))
(define ast (read-k3 k-path (open-input-file k-path)))

(render win)
