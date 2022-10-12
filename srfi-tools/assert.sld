(define-library (srfi-tools assert)
  (export assert)
  (import (scheme base))
  (begin
    (define (assert test . arguments)
      (unless test (apply error arguments)))))