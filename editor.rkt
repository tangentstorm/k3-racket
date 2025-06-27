#lang racket/gui
(require framework racket/gui/easy)
(require k3/color k3)
(require syntax/stx)
(require racket/system)

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
    (define modified? #f)

    (define/public (set-current-file-path! path)
      (set! current-file-path path))

    (define/public (get-current-file-path)
      current-file-path)

    (define/public (is-file-modified?)
      modified?)

    (define/public (set-file-modified! mod?)
      (set! modified? mod?))

    ; Override to create custom tab-snips with light blue background
    (define/override (on-new-tab-snip)
      (new custom-tab-snip%))

    ; Method to mark as modified (called externally)
    (define/public (mark-modified!)
      (set! modified? #t))

    (define/public (save-current-file)
      (when current-file-path
        (save-file-preserving-line-endings current-file-path)
        (set! modified? #f)
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
        (when (and ast (syntax? ast))
          (for [(node (find-token ast pos))] (displayln node)))))))

; Shared function to load a file and reset scroll position
(define (load-file-with-scroll-reset editor file-path)
  (send editor clear)
  (send editor load-file file-path)
  ; Reset scroll position to top-left after loading file
  (send editor set-position 0)
  (send editor scroll-to-position 0)
  ; Force horizontal scroll to leftmost position
  (send editor move-position 'home #f 'simple)
  ; Mark as unmodified after loading
  (send editor set-file-modified! #f))

; Tab container class using vertical-panel% with custom tab bar
(define tab-container%
  (class vertical-panel%
    (init-field [parent #f] [definitions-panel #f])
    (super-new [parent parent])

    (define tab-data '()) ; List of (hash 'file-path 'editor 'canvas 'title)
    (define next-untitled-id 1)
    (define tab-bar #f)
    (define editor-panel #f)
    (define active-tab-index 0)

    (define/public (get-tab-count)
      (length tab-data))

    (define/public (get-active-tab-index)
      active-tab-index)

    (define/public (get-active-editor)
      (define index (get-active-tab-index))
      (if (and (>= index 0) (< index (length tab-data)))
          (hash-ref (list-ref tab-data index) 'editor)
          #f))

    (define/public (get-active-file-path)
      (define editor (get-active-editor))
      (if editor
          (send editor get-current-file-path)
          #f))

    (define/public (get-tab-file-paths)
      (map (lambda (tab) (hash-ref tab 'file-path)) tab-data))

    (define (on-tab-switch)
      (define index (get-active-tab-index))
      (when (and (>= index 0) (< index (length tab-data)))
        (define tab (list-ref tab-data index))
        (define editor (hash-ref tab 'editor))
        (define file-path (hash-ref tab 'file-path))

        ; Update global reference
        (set! main-editor editor)

        ; Update definitions panel
        (when definitions-panel
          (send definitions-panel set-active-tab-editor! editor file-path))

        ; Save tab state
        (save-tab-state)))

    (define (create-tab-title file-path)
      (cond
        [file-path (path->string (file-name-from-path file-path))]
        [else (format "Untitled-~a" next-untitled-id)]))

    (define (update-tab-title index)
      (when (and (>= index 0) (< index (length tab-data)))
        (define tab (list-ref tab-data index))
        (define editor (hash-ref tab 'editor))
        (define base-title (hash-ref tab 'title))
        (define modified? (send editor is-file-modified?))
        (define display-title (if modified? (string-append base-title "*") base-title))

        ; Update the tab label using set-item-label
        (send this set-item-label index display-title)))

    (define/public (add-tab file-path)
      ; Check if file is already open
      (define existing-index (find-tab-with-file file-path))
      (cond
        [existing-index (switch-to-tab existing-index)]
        [else
         ; Create new editor and canvas - initially hidden
         (define canvas (new editor-canvas% [parent editor-panel]))
         (define editor (new k3-text%))
         (send canvas set-editor editor)
         (send editor set-style-list styles)
         (send canvas show #f) ; Hide initially

         ; Load file if specified
         (when file-path
           (load-file-with-scroll-reset editor file-path)
           (send editor set-current-file-path! file-path))

         ; Create tab data
         (define title (create-tab-title file-path))
         (when (not file-path)
           (set! next-untitled-id (+ next-untitled-id 1)))

         (define tab-info (hash 'file-path file-path
                                'editor editor
                                'canvas canvas
                                'title title))

         ; Add to tab data
         (set! tab-data (append tab-data (list tab-info)))

         ; Create tab button
         (create-tab-button (- (length tab-data) 1) title)

         ; Set up syntax coloring
         (define token-sym->style (lambda (sym) (symbol->string sym)))
         (define pairs '((|(| |)|) (|[| |]|) (|{| |}|)))
         (send editor start-colorer token-sym->style k3-color pairs)

         ; Switch to new tab
         (switch-to-tab (- (length tab-data) 1))

         ; Save tab state
         (save-tab-state)]))

    (define (find-tab-with-file file-path)
      (if file-path
          (let loop ([tabs tab-data] [index 0])
            (cond
              [(null? tabs) #f]
              [(equal? (hash-ref (car tabs) 'file-path) file-path) index]
              [else (loop (cdr tabs) (+ index 1))]))
          #f))

    (define (find-tab-with-editor editor)
      (let loop ([tabs tab-data] [index 0])
        (cond
          [(null? tabs) #f]
          [(eq? (hash-ref (car tabs) 'editor) editor) index]
          [else (loop (cdr tabs) (+ index 1))])))

    (define/public (close-tab index)
      (when (and (>= index 0) (< index (length tab-data)))
        (define tab (list-ref tab-data index))
        (define editor (hash-ref tab 'editor))

        ; Check for unsaved changes
        (if (send editor is-file-modified?)
            (let ([result (message-box "Unsaved Changes"
                                       (format "Save changes to ~a?"
                                               (hash-ref tab 'title))
                                       #f
                                       '(yes-no-cancel))])
              (case result
                [(yes) (send editor save-current-file)
                       (do-close-tab index)]
                [(no) (do-close-tab index)]
                [(cancel) (void)]))
            (do-close-tab index))))

    ; Initialize the tab container UI
    (define/public (init-ui)
      ; Create tab bar at the top
      (set! tab-bar (new horizontal-panel% [parent this] [stretchable-height #f]))
      ; Create editor panel that takes up remaining space
      (set! editor-panel (new panel% [parent this])))

    ; Create a tab button
    (define (create-tab-button index title)
      (new button%
           [parent tab-bar]
           [label title]
           [callback (lambda (btn event)
                      (switch-to-tab index))]))

    ; Switch to a specific tab
    (define (switch-to-tab index)
      (when (and (>= index 0) (< index (length tab-data)))
        ; Hide all canvases
        (for ([tab tab-data])
          (send (hash-ref tab 'canvas) show #f))

        ; Show the selected canvas
        (define selected-tab (list-ref tab-data index))
        (send (hash-ref selected-tab 'canvas) show #t)

        ; Update active index
        (set! active-tab-index index)

        ; Trigger tab switch callback
        (on-tab-switch)))

    (define (do-close-tab index)
      ; Get the tab to remove before modifying the list
      (define tab-to-remove (list-ref tab-data index))
      (define canvas-to-remove (hash-ref tab-to-remove 'canvas))

      ; Remove from tab data
      (set! tab-data (append (take tab-data index)
                             (drop tab-data (+ index 1))))

      ; Remove the canvas
      (send editor-panel delete-child canvas-to-remove)

      ; Rebuild tab bar
      (rebuild-tab-bar)

      ; Adjust selection if necessary
      (when (> (length tab-data) 0)
        (define new-selection (min index (- (length tab-data) 1)))
        (switch-to-tab new-selection))

      ; Save tab state
      (save-tab-state))

    ; Rebuild the tab bar
    (define (rebuild-tab-bar)
      (send tab-bar change-children (lambda (children) '()))
      (for ([tab tab-data] [index (in-naturals)])
        (define title (hash-ref tab 'title))
        (create-tab-button index title)))

    (define/public (close-current-tab)
      (define index (get-active-tab-index))
      (when (>= index 0)
        (close-tab index)))

    (define/public (save-current-tab)
      (define editor (get-active-editor))
      (when editor
        (send editor save-current-file)
        (update-tab-title (get-active-tab-index))))

    (define/public (restore-tabs file-paths active-index)
      ; Clear existing tabs first
      (set! tab-data '())

      ; Add tabs for each file path
      (for ([file-path file-paths])
        (add-tab (if (string=? file-path "") #f file-path)))

      ; Set active tab
      (when (and (> (length tab-data) 0)
                 (>= active-index 0)
                 (< active-index (length tab-data)))
        (switch-to-tab active-index)))))

; Definitions panel component
(define definitions-panel%
  (class* object% (view<%>)
    (init-field [editor #f])
    (super-new)

    (define definitions-list #f)
    (define current-file-path #f)
    (define current-definitions '()) ; Store definitions with line numbers
    (define sort-alphabetically? (hash-ref scp-config 'sort-definitions-alphabetically #f)) ; Sort by name when true, by line number when false

    (define/public (dependencies) '())

    (define/public (set-editor! ed)
      (set! editor ed))

    (define/public (set-active-tab-editor! ed file-path)
      (set! editor ed)
      (set! current-file-path file-path)
      (refresh-definitions))

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
      (refresh-definitions))

    (define/public (refresh-definitions)
      (when (and definitions-list current-file-path (file-exists? current-file-path))
        (define text (file->string current-file-path))
        (define defs (if (string-suffix? current-file-path ".k")
                        (extract-k3-definitions text)
                        (extract-racket-definitions text)))
        (define sorted-defs (sort-definitions defs))
        (set! current-definitions sorted-defs)
        (send definitions-list set (map (lambda (def) (format "~a (line ~a)" (car def) (cadr def))) sorted-defs))))

    (define/public (create parent)
      (define panel (new vertical-panel% [parent parent] [min-width 200]))

      ; Header with label and sort checkbox
      (define header-panel (new horizontal-panel% [parent panel] [stretchable-height #f]))
      (define label (new message% [parent header-panel] [label "Definitions:"]))
      (define sort-checkbox (new check-box%
                                 [parent header-panel]
                                 [label "az"]
                                 [value sort-alphabetically?]
                                 [callback (lambda (cb event)
                                            (set! sort-alphabetically? (send cb get-value))
                                            (hash-set! scp-config 'sort-definitions-alphabetically sort-alphabetically?)
                                            (save-scp-config)
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

    (define current-path (simplify-path (path->complete-path root-path)))
    (define file-list #f)
    (define path-panel #f)
    (define tab-container #f)

    (define/public (dependencies) '())

    (define/public (set-tab-container! container)
      (set! tab-container container))

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

    (define (load-file-in-tab file-path)
      (when tab-container
        (send tab-container add-tab (path->string file-path))))

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
                                             ; File - load in tab
                                             (define file-path (build-path current-path item-name))
                                             (load-file-in-tab file-path)]))))]))
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

    (define tab-container #f)
    (define file-browser #f)
    (define definitions-panel #f)
    (define browser-panel #f)
    (define defs-panel #f)
    (define main-panel #f)

    ; Panel visibility state - load from config
    (define show-file-browser? (hash-ref scp-config 'show-file-browser #t))
    (define show-definitions? (hash-ref scp-config 'show-definitions #t))

    (define/public (dependencies) '())

    (define/public (get-tab-container)
      tab-container)

    ; Toggle file browser panel
    (define/public (toggle-file-browser)
      (set! show-file-browser? (not show-file-browser?))
      (hash-set! scp-config 'show-file-browser show-file-browser?)
      (save-scp-config)
      (if show-file-browser?
          (show-browser-panel)
          (hide-browser-panel)))

    ; Toggle definitions panel
    (define/public (toggle-definitions)
      (set! show-definitions? (not show-definitions?))
      (hash-set! scp-config 'show-definitions show-definitions?)
      (save-scp-config)
      (if show-definitions?
          (show-definitions-panel)
          (hide-definitions-panel)))

    ; Show browser panel
    (define (show-browser-panel)
      (when (and main-panel (not browser-panel))
        (set! browser-panel (send file-browser create main-panel))
        (send browser-panel min-width 150)
        (send browser-panel stretchable-width #f)
        ; Move browser panel to the beginning
        (send main-panel change-children
              (lambda (children)
                (cons browser-panel (filter (lambda (c) (not (eq? c browser-panel))) children))))))

    ; Hide browser panel
    (define (hide-browser-panel)
      (when browser-panel
        (send main-panel delete-child browser-panel)
        (set! browser-panel #f)))

    ; Show definitions panel
    (define (show-definitions-panel)
      (when (and main-panel (not defs-panel))
        (set! defs-panel (send definitions-panel create main-panel))
        (send defs-panel min-width 200)
        (send defs-panel stretchable-width #f)))

    ; Hide definitions panel
    (define (hide-definitions-panel)
      (when defs-panel
        (send main-panel delete-child defs-panel)
        (set! defs-panel #f)))

    (define/public (create parent)
      (define main-container (new vertical-panel% [parent parent]))

      ; Create file browser first to get access to path panel creation
      (set! file-browser (new file-browser% [root-path k-directory]))

      ; Create path breadcrumb at the top
      (send file-browser create-path-panel main-container)

      ; Create horizontal panel for browser, editor, and definitions
      (set! main-panel (new horizontal-panel% [parent main-container]))

      ; Create definitions panel instance
      (set! definitions-panel (new definitions-panel% [editor #f]))

      ; Create tab container (center) - takes remaining space
      (set! tab-container (new tab-container% [parent main-panel] [definitions-panel definitions-panel]))

      ; Initialize the tab container UI
      (send tab-container init-ui)

      ; Create initial panels based on visibility state
      (when show-file-browser?
        (set! browser-panel (send file-browser create main-panel))
        (send browser-panel min-width 150)
        (send browser-panel stretchable-width #f))

      (when show-definitions?
        (set! defs-panel (send definitions-panel create main-panel))
        (send defs-panel min-width 200)
        (send defs-panel stretchable-width #f))

      ; Ensure proper panel ordering - file browser on left, definitions on right
      (when (and show-file-browser? show-definitions?)
        (send main-panel change-children
              (lambda (children)
                (list browser-panel tab-container defs-panel))))

      ; Connect file browser to tab container
      (send file-browser set-tab-container! tab-container)

      ; Restore tabs from config or create initial tab
      (define-values (saved-tabs saved-active-index) (load-tab-state))
      (send tab-container restore-tabs saved-tabs saved-active-index))

    (define/public (update v what val)
      (void))
    (define/public (destroy v)
      (void))))

(define (k3-view path)
  (new k3-view% [path path]))


; -- main program ------------------------------------------------

; Read file path from command line arguments, or use default "example.k"
(define k-path
  (let ([args (current-command-line-arguments)])
    (if (> (vector-length args) 0)
        (vector-ref args 0)
        "example.k")))

; Get the directory of the specified file for the file browser
(define k-directory
  (let ([file-path (path->complete-path k-path)])
    (path->string (path-only file-path))))

; Create the main frame with menu bar
(define frame (new frame%
                   [label k-path]
                   [width 1024]
                   [height 768]))

; Create menu bar
(define menu-bar (new menu-bar% [parent frame]))
(define file-menu (new menu% [label "File"] [parent menu-bar]))
(define view-menu (new menu% [label "View"] [parent menu-bar]))
(define transfer-menu (new menu% [label "Transfer"] [parent menu-bar]))

; Global reference to the editor (will be set when k3-view is created)
(define main-editor #f)
; Global reference to the k3-view instance for panel toggling
(define main-k3-view #f)
; Global reference to AST for syntax tree support
(define ast #f)

; SCP Configuration storage and persistence
(define scp-config (make-hash))
(define config-file-path (build-path (find-system-path 'home-dir) ".kracket"))

; Load configuration from file
(define (load-scp-config)
  (when (file-exists? config-file-path)
    (with-handlers ([exn:fail? (lambda (e)
                                 (printf "Warning: Could not load config file: ~a\n" (exn-message e)))])
      (define config-data (call-with-input-file config-file-path read))
      (when (hash? config-data)
        (hash-set! scp-config 'remote-host (hash-ref config-data 'remote-host ""))
        (hash-set! scp-config 'remote-path (hash-ref config-data 'remote-path ""))
        (hash-set! scp-config 'ssh-key-path (hash-ref config-data 'ssh-key-path ""))
        (hash-set! scp-config 'show-file-browser (hash-ref config-data 'show-file-browser #t))
        (hash-set! scp-config 'show-definitions (hash-ref config-data 'show-definitions #t))
        (hash-set! scp-config 'sort-definitions-alphabetically (hash-ref config-data 'sort-definitions-alphabetically #f))
        (hash-set! scp-config 'open-tabs (hash-ref config-data 'open-tabs (list k-path)))
        (hash-set! scp-config 'active-tab-index (hash-ref config-data 'active-tab-index 0))))))

; Save configuration to file
(define (save-scp-config)
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "Warning: Could not save config file: ~a\n" (exn-message e)))])
    (call-with-output-file config-file-path
      (lambda (out)
        (write scp-config out))
      #:exists 'replace)))

; Tab state management functions
(define (save-tab-state)
  (when main-k3-view
    (define tab-container (send main-k3-view get-tab-container))
    (when tab-container
      (define file-paths (send tab-container get-tab-file-paths))
      (define active-index (send tab-container get-active-tab-index))
      ; Convert #f file paths to empty strings for serialization
      (define serializable-paths (map (lambda (path) (if path path "")) file-paths))
      (hash-set! scp-config 'open-tabs serializable-paths)
      (hash-set! scp-config 'active-tab-index active-index)
      (save-scp-config))))

(define (load-tab-state)
  (define saved-tabs (hash-ref scp-config 'open-tabs (list k-path)))
  (define saved-active-index (hash-ref scp-config 'active-tab-index 0))
  ; Ensure we have at least one tab
  (define final-tabs (if (null? saved-tabs) (list k-path) saved-tabs))
  (values final-tabs saved-active-index))

; Initialize default values and load saved config
(hash-set! scp-config 'remote-host "")
(hash-set! scp-config 'remote-path "")
(hash-set! scp-config 'ssh-key-path "")
(hash-set! scp-config 'show-file-browser #t)
(hash-set! scp-config 'show-definitions #t)
(hash-set! scp-config 'sort-definitions-alphabetically #f)
(hash-set! scp-config 'open-tabs (list k-path))
(hash-set! scp-config 'active-tab-index 0)
(load-scp-config)

; Function to show SCP configuration dialog
(define (show-scp-config-dialog)
  (define config-dialog (new dialog%
                             [label "SCP Transfer Configuration"]
                             [width 400]
                             [height 200]))

  (define main-panel (new vertical-panel% [parent config-dialog]))

  ; Remote host field
  (define host-panel (new horizontal-panel% [parent main-panel] [stretchable-height #f]))
  (new message% [parent host-panel] [label "Remote Host:"] [min-width 100])
  (define host-field (new text-field%
                          [parent host-panel]
                          [label #f]
                          [init-value (hash-ref scp-config 'remote-host "")]
                          [min-width 250]))

  ; Remote path field
  (define path-panel (new horizontal-panel% [parent main-panel] [stretchable-height #f]))
  (new message% [parent path-panel] [label "Remote Path:"] [min-width 100])
  (define path-field (new text-field%
                          [parent path-panel]
                          [label #f]
                          [init-value (hash-ref scp-config 'remote-path "")]
                          [min-width 250]))

  ; SSH key path field
  (define key-panel (new horizontal-panel% [parent main-panel] [stretchable-height #f]))
  (new message% [parent key-panel] [label "SSH Key Path:"] [min-width 100])
  (define key-field (new text-field%
                         [parent key-panel]
                         [label #f]
                         [init-value (hash-ref scp-config 'ssh-key-path "")]
                         [min-width 250]))

  ; Button panel
  (define button-panel (new horizontal-panel% [parent main-panel] [stretchable-height #f]))
  (new button% [parent button-panel] [label "OK"]
       [callback (lambda (button event)
                   (hash-set! scp-config 'remote-host (send host-field get-value))
                   (hash-set! scp-config 'remote-path (send path-field get-value))
                   (hash-set! scp-config 'ssh-key-path (send key-field get-value))
                   (save-scp-config)
                   (send config-dialog show #f))])
  (new button% [parent button-panel] [label "Cancel"]
       [callback (lambda (button event)
                   (send config-dialog show #f))])

  (send config-dialog show #t))

; Function to transfer file via SCP
(define (transfer-current-file-via-scp)
  (when main-editor
    (define current-file (send main-editor get-current-file-path))
    (if current-file
        (let ([remote-host (hash-ref scp-config 'remote-host "")]
              [remote-path (hash-ref scp-config 'remote-path "")]
              [ssh-key-path (hash-ref scp-config 'ssh-key-path "")])
          (cond
            [(string=? remote-host "")
             (message-box "SCP Transfer" "Please configure remote host first." frame '(ok))]
            [else
             ; Save file before transfer
             (send main-editor save-current-file)
             ; Perform SCP transfer
             (transfer-file-scp current-file remote-host remote-path ssh-key-path)]))
        (message-box "SCP Transfer" "No file is currently open." frame '(ok)))))

; Function to perform the actual SCP transfer
(define (transfer-file-scp local-file remote-host remote-path ssh-key-path)
  ; Construct SCP command
  (define scp-args
    (if (and ssh-key-path (not (string=? ssh-key-path "")))
        (list "-i" ssh-key-path local-file
              (string-append remote-host ":" remote-path))
        (list local-file
              (string-append remote-host ":" remote-path))))

  ; Debug output
  (printf "SCP Command: scp ~a\n" (string-join scp-args " "))
  (printf "Local file: ~a\n" local-file)
  (printf "Remote host: ~a\n" remote-host)
  (printf "Remote path: ~a\n" remote-path)

  ; Show initial status message
  (printf "Starting transfer...\n")

  ; Create a simple progress frame (non-modal)
  (define progress-frame (new frame%
                              [label "SCP Transfer"]
                              [width 300]
                              [height 100]))
  (define progress-panel (new vertical-panel% [parent progress-frame]))
  (define status-msg (new message%
                          [parent progress-panel]
                          [label "Transferring file..."]))
  (define progress-gauge (new gauge%
                              [parent progress-panel]
                              [label #f]
                              [range 100]))

  ; Show progress frame
  (send progress-frame show #t)

  ; Execute SCP in a separate thread to avoid blocking GUI
  (thread
   (lambda ()
     (with-handlers ([exn:fail? (lambda (e)
                                  (printf "SCP Error: ~a\n" (exn-message e))
                                  (queue-callback
                                   (lambda ()
                                     (send progress-frame show #f)
                                     (message-box "SCP Transfer Error"
                                                  (format "Transfer failed with error: ~a"
                                                          (exn-message e))
                                                  frame '(ok stop)))))])
       ; Start progress animation
       (define progress-thread
         (thread
          (lambda ()
            (let loop ([progress 0])
              (when (send progress-frame is-shown?)
                (queue-callback
                 (lambda ()
                   (when (send progress-frame is-shown?)
                     (send progress-gauge set-value (modulo progress 100)))))
                (sleep 0.1)
                (loop (+ progress 5)))))))

       (printf "Executing SCP command...\n")
       (define-values (process stdout stdin stderr)
         (apply subprocess #f #f #f "/usr/bin/scp" scp-args))

       (printf "Process started, waiting for completion...\n")

       ; Read any error output
       (define error-output (port->string stderr))
       (define std-output (port->string stdout))

       ; Wait for process to complete
       (subprocess-wait process)
       (define exit-code (subprocess-status process))

       ; Stop progress animation
       (kill-thread progress-thread)

       ; Close ports
       (close-output-port stdin)
       (close-input-port stdout)
       (close-input-port stderr)

       ; Debug output
       (printf "Exit code: ~a\n" exit-code)
       (printf "Stdout: ~a\n" std-output)
       (printf "Stderr: ~a\n" error-output)

       ; Update UI on main thread
       (queue-callback
        (lambda ()
          (send progress-frame show #f)
          (if (= exit-code 0)
              (message-box "SCP Transfer"
                           (format "File successfully transferred to ~a"
                                   (string-append remote-host ":" remote-path))
                           frame '(ok))
              (message-box "SCP Transfer Error"
                           (format "Transfer failed with exit code: ~a\n\nError output:\n~a\n\nStandard output:\n~a"
                                   exit-code error-output std-output)
                           frame '(ok stop)))))))))

; Save menu item with Ctrl+S/Cmd+S hotkey
(define save-item (new menu-item%
                       [label "Save"]
                       [parent file-menu]
                       [shortcut #\s]
                       [callback (lambda (item event)
                                  (when main-k3-view
                                    (define tab-container (send main-k3-view get-tab-container))
                                    (when tab-container
                                      (send tab-container save-current-tab))))]))

; New tab menu item with Ctrl+T/Cmd+T hotkey
(define new-tab-item (new menu-item%
                          [label "New Tab"]
                          [parent file-menu]
                          [shortcut #\t]
                          [callback (lambda (item event)
                                     (when main-k3-view
                                       (define tab-container (send main-k3-view get-tab-container))
                                       (when tab-container
                                         (send tab-container add-tab #f))))]))

; Close tab menu item with Ctrl+W/Cmd+W hotkey
(define close-tab-item (new menu-item%
                            [label "Close Tab"]
                            [parent file-menu]
                            [shortcut #\w]
                            [callback (lambda (item event)
                                       (when main-k3-view
                                         (define tab-container (send main-k3-view get-tab-container))
                                         (when tab-container
                                           (send tab-container close-current-tab))))]))

; SCP Configuration menu item
(define scp-config-item (new menu-item%
                             [label "Configure SCP..."]
                             [parent transfer-menu]
                             [callback (lambda (item event)
                                         (show-scp-config-dialog))]))

; Transfer current file menu item with Ctrl+Shift+T hotkey
(define transfer-item (new menu-item%
                           [label "Transfer Current File"]
                           [parent transfer-menu]
                           [shortcut #\T]
                           [callback (lambda (item event)
                                       (transfer-current-file-via-scp))]))

; View menu items for toggling panels
(define toggle-file-browser-item (new menu-item%
                                      [label "Toggle File Browser"]
                                      [parent view-menu]
                                      [shortcut #\1]
                                      [callback (lambda (item event)
                                                 (when main-k3-view
                                                   (send main-k3-view toggle-file-browser)))]))

(define toggle-definitions-item (new menu-item%
                                     [label "Toggle Definitions Panel"]
                                     [parent view-menu]
                                     [shortcut #\2]
                                     [callback (lambda (item event)
                                                (when main-k3-view
                                                  (send main-k3-view toggle-definitions)))]))

; Create the main panel for the k3-view
(define main-panel (new panel% [parent frame]))

; Create k3-view instance
(define k3-view-instance (k3-view k-path))

; Create the view in the main panel
(send k3-view-instance create main-panel)

; Set global references
(set! main-k3-view k3-view-instance)
(define tab-container (send k3-view-instance get-tab-container))
(set! main-editor (send tab-container get-active-editor))

; Try to read AST for syntax tree support
(with-handlers ([exn:fail? (lambda (e)
                             (printf "Warning: Could not read AST: ~a\n" (exn-message e))
                             (set! ast #f))])
  (define current-file (send tab-container get-active-file-path))
  (when (and current-file (file-exists? current-file))
    (set! ast (read-k3 current-file (open-input-file current-file)))))

; Show the frame
(send frame show #t)
