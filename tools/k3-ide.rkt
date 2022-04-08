#lang racket
;
; support for editing k3 files in drracket.
;
(require drracket/tool
         k3 k3/parse-only
         (prefix-in k3: (submod k3/parse-only reader)))

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
         [language-position '("Array Languages" "K3")]
         [language-id "k3"]
         [one-line-summary "k3 from kx systems"]
         ; reader from k3 collection via (require k3)
         [reader k3:read-syntax])))

    (drracket:language-configuration:add-language
     (new k3-language%))))
