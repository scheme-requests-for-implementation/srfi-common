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

  ;; Import JSON library based on implementation
  ;; Both (srfi 180) and (chibi json) provide compatible APIs:
  ;;   - json-read: port-or-string -> scheme-object
  ;;   - json-write: scheme-object port -> unspecified
  (cond-expand
   ;; SRFI 180 is the portable standard (Chicken, Gauche, and others support it)
   ((library (srfi 180))
    (import (srfi 180)))
   ;; Chibi Scheme has its own compatible JSON library
   (chibi
    (import (chibi json)))
   ;; Fallback error if no JSON library available
   (else
    (begin
      (error "No JSON library available. Please install SRFI 180 or a compatible JSON library."))))
  (include "simplelists.scm"))