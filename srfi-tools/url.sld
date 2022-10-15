(define-library (srfi-tools url)
  (export srfi-home-url
          srfi-tar-gz-url
          srfi-landing-url
          srfi-html-url)
  (import (scheme base)
          (srfi-tools private path)
          (srfi-tools private port)
          (srfi-tools private command)
          (srfi-tools data)
          (srfi-tools path))
  (begin

    (define (srfi-home-url)
      "https://srfi.schemers.org/")

    (define-command (home-url)
      "Display the URL of the SRFI home page."
      (write-line (srfi-home-url)))

    (define (srfi-tar-gz-url)
      (string-append (srfi-home-url) "srfi.tgz"))

    (define (srfi-landing-url num)
      (path-append (srfi-home-url) (srfi-num-stem num) ""))

    (define-command (landing-url num)
      "Display the URL of the landing page for SRFI <num>."
      (write-line-about-srfi srfi-landing-url num))

    (define (srfi-html-url num)
      (path-append (srfi-home-url)
                   (srfi-num-stem num)
                   (srfi-num-ext num ".html")))

    (define-command (html-url num)
      "Display the URL of the document for SRFI <num>."
      (write-line-about-srfi srfi-html-url num))))
