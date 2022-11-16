(define-library (srfi-tools info)
  (export srfi-info
          srfi-paste)
  (import (scheme base)

          (srfi-tools private path)
          (srfi-tools private port)
          (srfi-tools private string)
          (srfi-tools private format)
          (srfi-tools private command)

          (srfi-tools data)
          (srfi-tools html)
          (srfi-tools library)
          (srfi-tools url))
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
      "Display a summary of SRFI <num>."
      (write-string (srfi-info (parse-srfi-number num))))

    (define (srfi-paste num)
      (let ((title (srfi-title num))
            (url (srfi-landing-url num)))
        (with-output-to-string
          (lambda ()
            (disp)
            (disp (format "SRFI ~a: ~a" num title))
            (disp (format "SRFI ~a (~a)" num title))
            (disp)
            (disp (srfi-r6rs-import num))
            (disp (srfi-r7rs-import num))
            (disp)
            (disp (format "<a href=\"~a\">SRFI ~a (~a)</a>" url num title))
            (disp)
            (disp "AsciiDoc:")
            (disp (format "~a[SRFI ~a (~a)]" url num title))
            (disp)
            (disp "Org (Emacs):")
            (disp (format "[[~a][SRFI ~a (~a)]]" url num title))
            (disp)
            (disp "Markdown:")
            (disp (format "[SRFI ~a (~a)](~a)" num title url))
            (disp)))))

    (define-command (paste num)
      "Display things about SRFI <num> handy to copy and paste."
      (write-string (srfi-paste (parse-srfi-number num))))))
