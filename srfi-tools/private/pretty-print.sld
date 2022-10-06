(define-library (srfi-tools private pretty-print)
  (export pretty-print)
  (import (scheme base))

  (cond-expand
   (chibi
    (import (chibi show)
            (chibi show pretty)))
   (gauche
    (import (only (gauche base) pprint)))
   (else
    (import (scheme write))))

  (cond-expand
   (chibi
    (begin
      (define (pretty-print obj)
        (show #t (pretty obj)))))
   (gauche
    (begin
      (define (pretty-print obj)
        (pprint obj))))
   (else
    (begin
      (define (pretty-print obj)
        (write obj)
        (newline))))))
