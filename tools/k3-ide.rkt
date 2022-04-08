#lang racket
;
; support for editing k3 files in drracket.
;
(require drracket/tool k3
         #;(prefix-in k3: (submod k3/parse-only reader)))


(provide tool@)

(define-unit tool@
  (import drracket:tool^)
  (export drracket:tool-exports^)

  (define (phase1) (void))
  (define (phase2)

    ; we eventually need an instance of a class that implements drracket:language:language<%>
    ; it's a big interface, so if you don't want to implement everything by hand, you need
    ; this big scary thing here to construct a default class that you can then extend.
    ; (also, the get-default-mixin at the top can only run inside phase2)
    (define super%
      ((drracket:language:get-default-mixin)
       (drracket:language:module-based-language->language-mixin
        (drracket:language:simple-module-based-language->module-based-language-mixin
         drracket:language:simple-module-based-language%))))

    (define k3-language%
      (class super%
        (super-new
         ; arguments to drracket:language:simple-module-based-language%
         [module 'k3]
         [reader read-k3]
         [language-position '("Array Languages" "K3")]
         [language-id "k3"]
         [one-line-summary "k3 from kx systems"])

        ; in order to "execute" k3 code in the repl, we have to
        ; load all the necessary definitions into the user's namespace.
        ; this was cobbled together from examples of other language plugins.
        (define/override (on-execute settings run-in-user-thread)
          (super on-execute settings run-in-user-thread)
          (dynamic-require 'k3/tools/dr-eval #f)
          (let ([dr-ns (current-namespace)]
                [path ((current-module-name-resolver) 'k3/tools/dr-eval #f #f)])
            (run-in-user-thread
             (lambda ()
               (with-handlers
                 ([void (lambda (ex) (printf "error: ~a\n" (exn-message ex)))])
                 (namespace-attach-module dr-ns path)
                 (namespace-require path) )))))

        ; !!several plugins had a reader that looks like this. it seems like without
        ; something like this, the reader goes into an infinite loop, but
        ; this particular implementation didn't change anything, so i'm
        ; falling back to just [reader read-k3] in super-new above.
        #;(define/override (get-reader)
            (lambda (name port)
              (let ([v (read-k3 name port)])
                (if (eof-object? v)
                    v
                    (namespace-syntax-introduce v))))) ))

    (drracket:language-configuration:add-language
     (new k3-language%))))
