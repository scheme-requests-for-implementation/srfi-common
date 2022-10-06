(define-library (srfi-tools path)
  (export srfi-num-stem
          srfi-num-ext
          srfi-home-dir
          srfi-common-dir
          srfi-data-file
          srfi-rss-file
          srfi-dir
          srfi-html-file
          srfi-landing-html-file
          srfi-abstract-html-file)
  (import (scheme base)
          (srfi-tools private path)
          (srfi-tools private port)
          (srfi-tools private format)
          (srfi-tools private os)
          (srfi-tools private command)
          (srfi-tools core))
  (begin

    (define (srfi-home-dir)
      (or (directory-from-envar "SRFI_HOME")
          (path-append (user-home-dir) "srfi")))

    (define-command (home-dir)
      (write-line (srfi-home-dir)))

    (define (srfi-common-dir)
      (path-append (srfi-home-dir) "srfi-common"))

    (define-command (common-dir)
      (write-line (srfi-common-dir)))

    (define (srfi-data-file)
      (path-append (srfi-home-dir) "srfi-common" "admin" "srfi-data.scm"))

    (define-command (data-file)
      (write-line (srfi-data-file)))

    (define (srfi-rss-file)
      (path-append (srfi-home-dir) "srfi-common" "srfi.rss"))

    (define-command (rss-file)
      (write-line (srfi-rss-file)))

    (define (srfi-dir num)
      (path-append (srfi-home-dir) (srfi-num-stem num)))

    (define-command (dir num)
      (write-line-about-srfi srfi-dir num))

    (define (srfi-html-file num)
      (path-append (srfi-dir num) (srfi-num-ext num ".html")))

    (define-command (html-file num)
      (write-line-about-srfi srfi-html-file num))

    (define (srfi-landing-html-file num)
      (path-append (srfi-dir num) "index.html"))

    (define-command (landing-html-file num)
      (write-line-about-srfi srfi-landing-html-file num))

    (define (srfi-abstract-html-file num)
      (path-append (srfi-home-dir)
                   "srfi-common"
                   "admin"
                   "abstracts"
                   (format "~a.html" num)))

    (define-command (abstract-html-file num)
      (write-line-about-srfi srfi-abstract-html-file num))

    ;;

    (define (srfi-data-file)
      (path-append (srfi-home-dir) "srfi-common" "admin" "srfi-data.scm"))

    (define-command (data-file)
      (write-line (srfi-data-file)))))
