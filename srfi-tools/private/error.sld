(define-library (srfi-tools private error)
  (export user-error
          usage)
  (import (scheme base)
          (scheme process-context)
          (srfi-tools private string)
          (srfi-tools private port))
  (begin

    (define user-error
      (lambda args
        (parameterize ((current-output-port (current-error-port)))
          (write-line (string-join (map displayed args) " ")))
        (exit #f)))

    (define usage user-error)))
