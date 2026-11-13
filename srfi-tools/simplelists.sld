(define-library (srfi-tools simplelists)
  (export simplelists-api-token
          simplelists-api-base-url
          simplelists-create-list
          simplelists-get-list
          simplelists-update-list-form)
  (import (scheme base)
          (scheme file)
          (scheme process-context)
          (scheme read)
          (scheme write)

          (srfi-tools data)
          (srfi-tools path)

          (srfi-tools private command)
          (srfi-tools private error)
          (srfi-tools private file)
          (srfi-tools private format)
          (srfi-tools private list)
          (srfi-tools private os)
          (srfi-tools private port)
          (srfi-tools private pretty-print)
          (srfi-tools private string))
  (cond-expand
   ((library (srfi 180)) (import (srfi 180)))
   (chibi (import (chibi json)))
   (else
    (begin (error "Please install SRFI 180 or a compatible JSON library."))))
  (cond-expand
   ((library (srfi 166)) (import (only (srfi 166) show tabular each)))
   ((library (srfi 159)) (import (only (srfi 159) show tabular each)))
   (else (begin (error "Please install SRFI 159 or SRFI 166."))))
  (include "simplelists.scm"))