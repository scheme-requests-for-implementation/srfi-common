(define-library (srfi-tools private error)
  (export display-error
          user-error
          assert
          usage)
  (import (scheme base)
          (scheme process-context)
          (scheme write)
          (srfi-tools private string)
          (srfi-tools private port))
  (begin

    (define (display-error err)
      (parameterize ((current-output-port (current-error-port)))
        (display (error-object-message err))
        (newline)))

    (define user-error error)

    (define (assert test . arguments)
      (unless test (apply error arguments)))

    (define (usage arg . arguments)
      (display arg)
      (for-each (lambda (arg) (write-string " ") (display arg))
                arguments)
      (newline)
      (exit #f))))
