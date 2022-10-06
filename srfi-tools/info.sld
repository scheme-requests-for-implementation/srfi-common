(define-library (srfi-tools info)
  (export srfi-info)
  (import (scheme base)
          (srfi-tools private path)
          (srfi-tools private port)
          (srfi-tools private string)
          (srfi-tools private format)
          (srfi-tools private command)
          (srfi-tools data)
          (srfi-tools html))
  (begin

    (define (srfi-info srfi)
      (string-append
       "SRFI " (number->string (srfi-number srfi)) ": " (srfi-title srfi)
       "\n"
       "by " (srfi-format-authors (srfi-authors srfi))
       "\n"
       "\n"
       "Status: " (symbol->string (srfi-status srfi))
       " (" (srfi-date-of-last-update srfi) ")"
       "\n"
       "Keywords: "
       (string-join (map srfi-format-keyword (srfi-keywords srfi))
                    ", ")
       "\n"
       (let ((numbers (srfi-see-also srfi)))
         (if (null? numbers)
             ""
             (string-append
              "See also: "
              (string-join (map number->string numbers) ", ")
              "\n")))
       "\n"
       "Abstract:\n"
       "\n"
       (srfi-abstract-text srfi)))

    (define-command (info num)
      (write-string (srfi-info (parse-srfi-number num))))))
