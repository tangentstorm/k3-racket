#lang racket/gui
(require framework racket/gui/easy)
(require k3/color k3)
(require syntax/stx)

; Read file path from command line arguments, default to "example.k"
(define k-path
  (let ([args (current-command-line-arguments)])
    (if (> (vector-length args) 0)
        (vector-ref args 0)
        "example.k")))

; Shared function to load a file and reset scroll position
(define (load-file-with-scroll-reset editor file-path)
  (send editor clear)
  (send editor load-file file-path)
  ; Reset scroll position to top-left after loading file
  (send editor set-position 0)
  (send editor scroll-to-position 0)
  ; Force horizontal scroll to leftmost position
  (send editor move-position 'home #f 'simple))

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

; Platform-aware font selection
(define (get-monospace-font)
  (case (system-type 'os)
    [(windows) "Consolas"]
    [(macosx) "Menlo"]
    [(unix) "DejaVu Sans Mono"]
    [else "monospace"]))

(define styles (new style-list%)) ; includes "Basic"
(define Syntax (send styles new-named-style "Syntax" (send styles basic-style)))
(let [(delta (new style-delta%))]
  (send delta set-delta-face (get-monospace-font))
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

; Create a special style for tab-snips with light blue background
(define tab-style (send styles new-named-style "tab-style" Syntax))
(let [(delta (new style-delta%))]
  (send delta set-delta-background (find-color "Light Blue"))
  (send tab-style set-delta delta))

; Custom tab-snip class that draws with light blue background and fixed width
(define custom-tab-snip%
  (class tab-snip%
    (super-new)

    ; Override get-extent to make tab always 4 spaces wide
    (define/override (get-extent dc x y w h descent space lspace rspace)
      (define char-width 8) ; approximate character width for 12pt font
      (define fixed-tab-width (* 4 char-width)) ; exactly 4 spaces
      (when w (set-box! w fixed-tab-width))
      (when h (set-box! h 16)) ; line height
      (when descent (set-box! descent 0))
      (when space (set-box! space 0))
      (when lspace (set-box! lspace 0))
      (when rspace (set-box! rspace 0)))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      ; Draw light blue background for exactly 4 spaces
      (define old-brush (send dc get-brush))
      (define old-pen (send dc get-pen))
      (define char-width 8)
      (define fixed-tab-width (* 4 char-width)) ; exactly 4 spaces
      (define line-height 16)

      ; Fill with light blue background
      (send dc set-brush (new brush% [color (find-color "Light Blue")]))
      (send dc set-pen (new pen% [color (find-color "Light Blue")]))
      (send dc draw-rectangle x y fixed-tab-width line-height)

      ; Draw a lighter border (1 pixel)
      (send dc set-brush (new brush% [color (find-color "Light Blue")] [style 'transparent]))
      (send dc set-pen (new pen% [color (find-color "Light Cyan")] [width 1]))
      (send dc draw-rectangle x y fixed-tab-width line-height)

      (send dc set-brush old-brush)
      (send dc set-pen old-pen)
      ; Don't call super draw - we want our fixed-width behavior only
      )))

(define k3-text% ; enhanced editor with line numbers
  (class (text:line-numbers-mixin racket:text%)
    (super-new)

    (define current-file-path #f)

    (define/public (set-current-file-path! path)
      (set! current-file-path path))

    (define/public (get-current-file-path)
      current-file-path)

    ; Override to create custom tab-snips with light blue background
    (define/override (on-new-tab-snip)
      (new custom-tab-snip%))

    (define/public (save-current-file)
      (when current-file-path
        (save-file-preserving-line-endings current-file-path)
        (printf "Saved: ~a\n" current-file-path)))

    (define (save-file-preserving-line-endings file-path)
      ; Detect original line endings from the file
      (define original-line-ending
        (if (file-exists? file-path)
            (let ([content (file->bytes file-path)])
              (cond
                [(regexp-match #rx#"\r\n" content) "\r\n"] ; Windows CRLF
                [(regexp-match #rx#"\r" content) "\r"]     ; Classic Mac CR
                [else "\n"]))                              ; Unix LF (default)
            "\n")) ; Default to Unix if file doesn't exist

      ; Get the current text content
      (define text-content (send this get-text 0 'eof))

      ; Convert line endings to match original format
      (define converted-content
        (cond
          [(equal? original-line-ending "\r\n")
           (regexp-replace* #rx"\r?\n" text-content "\r\n")]
          [(equal? original-line-ending "\r")
           (regexp-replace* #rx"\r?\n" text-content "\r")]
          [else
           (regexp-replace* #rx"\r\n?" text-content "\n")]))

      ; Write the file with preserved line endings
      (call-with-output-file file-path
        (lambda (out)
          (display converted-content out))
        #:exists 'replace))

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

; Definitions panel component
(define definitions-panel%
  (class* object% (view<%>)
    (init-field [editor #f])
    (super-new)

    (define definitions-list #f)
    (define current-file-path #f)
    (define current-definitions '()) ; Store definitions with line numbers
    (define sort-alphabetically? #f) ; Sort by name when true, by line number when false

    (define/public (dependencies) '())

    (define/public (set-editor! ed)
      (set! editor ed))

    (define (extract-k3-definitions text)
      (define lines (string-split text "\n"))
      (define definitions '())
      (define brace-depth 0)
      (for ([line lines] [line-num (in-naturals 1)])
        (define trimmed (string-trim line))
        ; Update brace depth by counting braces in the line
        (define open-braces (length (regexp-match* #rx"\\{" line)))
        (define close-braces (length (regexp-match* #rx"\\}" line)))
        ; Check if this line contains a definition and we're at top level (brace-depth = 0)
        (when (and (= brace-depth 0)  ; only at top level
                   (not (string-prefix? trimmed "/"))  ; not a comment
                   (not (string-prefix? trimmed "\\")) ; not a command
                   (regexp-match #rx"^([a-zA-Z_][a-zA-Z0-9_]*):(.*)$" trimmed))
          (define match (regexp-match #rx"^([a-zA-Z_][a-zA-Z0-9_]*):(.*)$" trimmed))
          (when match
            (define name (cadr match))
            (set! definitions (cons (list name line-num) definitions))))
        ; Update brace depth after processing the line
        (set! brace-depth (+ brace-depth open-braces (- close-braces))))
      (reverse definitions))

    (define (extract-racket-definitions text)
      (define definitions '())
      (define lines (string-split text "\n"))
      (for ([line lines] [line-num (in-naturals 1)])
        (define trimmed (string-trim line))
        ; Only match definitions that start at the beginning of the line (top-level)
        (when (or (regexp-match #rx"^\\(define\\s+([a-zA-Z_][a-zA-Z0-9_-]*)" trimmed)
                  (regexp-match #rx"^\\(define/public\\s+\\(([a-zA-Z_][a-zA-Z0-9_-]*)" trimmed)
                  (regexp-match #rx"^\\(define/override\\s+\\(([a-zA-Z_][a-zA-Z0-9_-]*)" trimmed)
                  (regexp-match #rx"^\\(define\\s+\\(([a-zA-Z_][a-zA-Z0-9_-]*)" trimmed))
          (define match (or (regexp-match #rx"^\\(define\\s+([a-zA-Z_][a-zA-Z0-9_-]*)" trimmed)
                           (regexp-match #rx"^\\(define/public\\s+\\(([a-zA-Z_][a-zA-Z0-9_-]*)" trimmed)
                           (regexp-match #rx"^\\(define/override\\s+\\(([a-zA-Z_][a-zA-Z0-9_-]*)" trimmed)
                           (regexp-match #rx"^\\(define\\s+\\(([a-zA-Z_][a-zA-Z0-9_-]*)" trimmed)))
          (when match
            (define name (cadr match))
            (set! definitions (cons (list name line-num) definitions)))))
      (reverse definitions))

    (define (sort-definitions defs)
      (if sort-alphabetically?
          (sort defs (lambda (a b) (string<? (car a) (car b))))
          defs)) ; already sorted by line number from extraction

    (define/public (update-definitions file-path)
      (set! current-file-path file-path)
      (when (and definitions-list (file-exists? file-path))
        (define text (file->string file-path))
        (define defs (if (string-suffix? file-path ".k")
                        (extract-k3-definitions text)
                        (extract-racket-definitions text)))
        (define sorted-defs (sort-definitions defs))
        (set! current-definitions sorted-defs)
        (send definitions-list set (map (lambda (def) (format "~a (line ~a)" (car def) (cadr def))) sorted-defs))))

    (define/public (refresh-definitions)
      (when current-file-path
        (update-definitions current-file-path)))

    (define/public (create parent)
      (define panel (new vertical-panel% [parent parent] [min-width 200]))

      ; Header with label and sort checkbox
      (define header-panel (new horizontal-panel% [parent panel] [stretchable-height #f]))
      (define label (new message% [parent header-panel] [label "Definitions:"]))
      (define sort-checkbox (new check-box%
                                 [parent header-panel]
                                 [label "az"]
                                 [value #f]
                                 [callback (lambda (cb event)
                                            (set! sort-alphabetically? (send cb get-value))
                                            (refresh-definitions))]))

      (set! definitions-list (new list-box%
                                  [parent panel]
                                  [label #f]
                                  [choices '()]
                                  [callback (lambda (lb event)
                                             (when (eq? 'list-box-dclick (send event get-event-type))
                                               (define selection (send lb get-selection))
                                               (when (and selection editor (< selection (length current-definitions)))
                                                 (define def (list-ref current-definitions selection))
                                                 (define line-num (cadr def))
                                                 ; Jump to the line in the editor
                                                 (define line-start (send editor paragraph-start-position (- line-num 1)))
                                                 ; Move cursor to the beginning of the line
                                                 (send editor set-position line-start)
                                                 ; Force scroll to put the line at the top by scrolling to end first, then to target
                                                 (send editor scroll-to-position (send editor last-position))
                                                 (send editor scroll-to-position line-start))))]))
      panel)

    (define/public (update v what val) (void))
    (define/public (destroy v) (void))))

; File browser component
(define file-browser%
  (class* object% (view<%>)
    (init-field [root-path "."])
    (super-new)

    (define current-editor #f)
    (define current-path (simplify-path (path->complete-path root-path)))
    (define file-list #f)
    (define path-panel #f)
    (define definitions-panel #f)

    (define/public (dependencies) '())

    (define/public (set-editor! editor)
      (set! current-editor editor))

    (define/public (set-definitions-panel! panel)
      (set! definitions-panel panel))

    (define (get-items path)
      (define items '())
      ; Add parent directory if not at root
      (unless (equal? path (simplify-path (build-path path "..")))
        (set! items (cons ".." items)))
      ; Add directories
      (define dirs (filter (lambda (p)
                            (directory-exists? (build-path path p)))
                          (directory-list path)))
      (set! items (append items (map (lambda (d) (string-append "[" (path->string d) "]")) dirs)))
      ; Add files
      (define files (filter (lambda (p)
                             (let ([full-path (build-path path p)])
                               (and (file-exists? full-path)
                                    (or (string-suffix? (path->string p) ".k")
                                        (string-suffix? (path->string p) ".rkt")
                                        (string-suffix? (path->string p) ".txt")))))
                           (directory-list path)))
      (set! items (append items (map path->string files)))
      items)

    (define (load-file-in-editor file-path)
      (when current-editor
        (load-file-with-scroll-reset current-editor (path->string file-path))
        ; Set the current file path in the editor
        (send current-editor set-current-file-path! (path->string file-path))
        ; Update definitions panel when file is loaded
        (when definitions-panel
          (send definitions-panel update-definitions (path->string file-path)))))

    (define (navigate-to new-path)
      (set! current-path (simplify-path (path->complete-path new-path)))
      (refresh-view))

    (define (refresh-view)
      (when file-list
        (send file-list set (get-items current-path)))
      (when path-panel
        (update-path-display)))

    (define (update-path-display)
      (send path-panel change-children (lambda (children) '()))
      (define path-parts (explode-path current-path))
      (define (create-path-button part accumulated-path)
        (new button%
             [parent path-panel]
             [label (if (path? part) (path->string part) (format "~a" part))]
             [callback (lambda (button event)
                        (navigate-to accumulated-path))]))

      (let loop ([parts path-parts] [acc-path #f])
        (unless (null? parts)
          (define part (car parts))
          (define new-acc (if acc-path (build-path acc-path part) part))
          (create-path-button part new-acc)
          (unless (null? (cdr parts))
            (new message% [parent path-panel] [label "/"]))
          (loop (cdr parts) new-acc))))

    (define/public (create parent)
      (define panel (new vertical-panel% [parent parent] [min-width 150]))

      ; File/directory list (no path breadcrumb here)
      (set! file-list (new list-box%
                           [parent panel]
                           [label #f]
                           [choices (get-items current-path)]
                           [callback (lambda (lb event)
                                      (when (eq? 'list-box-dclick (send event get-event-type))
                                        (define selection (send lb get-selection))
                                        (when selection
                                          (define item-name (send lb get-string selection))
                                          (cond
                                            [(equal? item-name "..")
                                             (navigate-to (build-path current-path ".."))]
                                            [(and (string-prefix? item-name "[") (string-suffix? item-name "]"))
                                             ; Directory - remove brackets and navigate
                                             (define dir-name (substring item-name 1 (- (string-length item-name) 1)))
                                             (navigate-to (build-path current-path dir-name))]
                                            [else
                                             ; File - load in editor
                                             (define file-path (build-path current-path item-name))
                                             (load-file-in-editor file-path)]))))]))
      panel)

    (define/public (create-path-panel parent)
      ; Create path breadcrumb panel
      (set! path-panel (new horizontal-panel% [parent parent] [stretchable-height #f]))
      (update-path-display)
      path-panel)

    (define/public (update v what val) (void))
    (define/public (destroy v) (void))))

(define k3-view% ; for easy-gui
  (class* object% (view<%>)
    (init-field path)
    (super-new)

    (define txt #f)
    (define file-browser #f)
    (define definitions-panel #f)

    (define/public (dependencies) '())

    (define/public (get-editor)
      txt)

    (define/public (create parent)
      (define main-container (new vertical-panel% [parent parent]))

      ; Create file browser first to get access to path panel creation
      (set! file-browser (new file-browser% [root-path "."]))

      ; Create path breadcrumb at the top
      (send file-browser create-path-panel main-container)

      ; Create horizontal panel for browser, editor, and definitions
      (define main-panel (new horizontal-panel% [parent main-container]))

      ; Create narrow file browser (left side)
      (define browser-panel (send file-browser create main-panel))
      (send browser-panel min-width 150)
      (send browser-panel stretchable-width #f)

      ; Create editor (center) - takes remaining space
      (define ed (new editor-canvas% [parent main-panel]))
      (set! txt (new k3-text%))
      (send ed set-editor txt)
      (send txt set-style-list styles)

      ; Ensure editor starts at top-left position
      (send ed scroll-to 0 0 0 0 #t)

      ; Create definitions panel (right side)
      (set! definitions-panel (new definitions-panel% [editor txt]))
      (define defs-panel (send definitions-panel create main-panel))
      (send defs-panel min-width 200)
      (send defs-panel stretchable-width #f)

      ; Connect file browser to editor and definitions panel
      (send file-browser set-editor! txt)
      (send file-browser set-definitions-panel! definitions-panel)

      (define (token-sym->style sym) (symbol->string sym))
      (define pairs '((|(| |)|) (|[| |]|) (|{| |}|)))
      (send txt start-colorer token-sym->style k3-color pairs)

      ; Load initial file with proper scroll reset
      (load-file-with-scroll-reset txt k-path)

      ; Update definitions for initial file
      (send definitions-panel update-definitions k-path))

    (define/public (update v what val)
      (void))
    (define/public (destroy v)
      (void))))

(define (k3-view path)
  (new k3-view% [path path]))


; -- main program ------------------------------------------------

; Create the main frame with menu bar
(define frame (new frame%
                   [label k-path]
                   [width 1024]
                   [height 768]))

; Create menu bar
(define menu-bar (new menu-bar% [parent frame]))
(define file-menu (new menu% [label "File"] [parent menu-bar]))

; Global reference to the editor (will be set when k3-view is created)
(define main-editor #f)

; Save menu item with Ctrl+S/Cmd+S hotkey
(define save-item (new menu-item%
                       [label "Save"]
                       [parent file-menu]
                       [shortcut #\s]
                       [callback (lambda (item event)
                                  (when main-editor
                                    (send main-editor save-current-file)))]))

; Create the main panel for the k3-view
(define main-panel (new panel% [parent frame]))

; Create k3-view instance
(define k3-view-instance (k3-view k-path))

; Create the view in the main panel
(send k3-view-instance create main-panel)

; Get reference to the editor from the k3-view and set it for the menu
(set! main-editor (send k3-view-instance get-editor))

; Set the initial file path in the editor
(send main-editor set-current-file-path! k-path)

(define ast (read-k3 k-path (open-input-file k-path)))

; Show the frame
(send frame show #t)
