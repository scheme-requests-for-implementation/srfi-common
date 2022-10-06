(define-library (srfi-tools mail)
  (export srfi-mail-archive-url
          srfi-mail-address)
  (import (scheme base)
          (srfi-tools private path)
          (srfi-tools private port)
          (srfi-tools private command)
          (srfi-tools data)
          (srfi-tools path))
  (begin

    (define (srfi-mail-archive-url num)
      (string-append "https://srfi-email.schemers.org/"
                     (srfi-num-stem num) "/"))

    (define-command (mail-archive-url num)
      (write-line-about-srfi srfi-mail-archive-url num))

    (define (srfi-mail-address num)
      (string-append (srfi-num-stem num)
                     "@srfi.schemers.org"))

    (define-command (mail-address num)
      (write-line-about-srfi srfi-mail-address num))))
