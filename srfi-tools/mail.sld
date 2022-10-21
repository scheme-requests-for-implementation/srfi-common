(define-library (srfi-tools mail)
  (export srfi-mail-archive-url
          srfi-mail-address
          srfi-mailto-url)
  (import (scheme base)
          (srfi-tools private path)
          (srfi-tools private port)
          (srfi-tools private string)
          (srfi-tools private command)
          (srfi-tools data)
          (srfi-tools path))
  (begin

    (define (srfi-mail-archive-url num)
      (string-append "https://srfi-email.schemers.org/"
                     (srfi-num-stem num) "/"))

    (define-command (mail-archive-url num)
      "Display mail archive URL for SRFI <num>."
      (write-line-about-srfi srfi-mail-archive-url num))

    (define (srfi-mail-address num)
      (string-append (srfi-num-stem num)
                     "@srfi.schemers.org"))

    (define-command (mail-address num)
      "Display email address URL for SRFI <num>."
      (write-line-about-srfi srfi-mail-address num))

    (define (srfi-mailto-url num)
      (string-append "mailto:"
                     (srfi-mail-address num)
                     "?subject=" (url-hexify-string (srfi-title num))))))
