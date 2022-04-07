#lang br
;
; demo of using "macro-style" transformers on the parsed k code:
;
(require k3 k3/macro-style rnrs/io/ports-6)

(define path "hello.k")
(define port (open-file-input-port path))
(define raw-k3 (read-k3 path port))
  
(define ns (make-base-namespace))
(namespace-attach-module (current-namespace) 'k3/macro-style ns)
(namespace-require 'k3/macro-style ns)

(parameterize [(current-namespace ns)]
  (printf "~nsyntax->datum:~n")
  (displayln (syntax->datum raw-k3))
  (printf "~nexpansion:~n")
  (displayln (syntax->datum (expand raw-k3)))
  (printf "~nevaluation (should be same as syntax->datum!)~n")
  (displayln (eval raw-k3))
  (printf "~nevaluate again to be sure.~n")
  (displayln (eval (eval raw-k3))))
