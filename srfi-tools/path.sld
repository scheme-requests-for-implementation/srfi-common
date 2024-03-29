(define-library (srfi-tools path)
  (export srfi-num-stem
          srfi-num-ext
          srfi-home-dir
          srfi-common-dir
          srfi-data-file
          srfi-signatures-file
          srfi-rss-file
          srfi-dir
          srfi-html-file
          srfi-landing-html-file
          srfi-abstract-html-file
          srfi-do
          srfi-do-common)
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
      "Display the SRFI home directory."
      (write-line (srfi-home-dir)))

    (define (srfi-common-dir)
      (path-append (srfi-home-dir) "srfi-common"))

    (define-command (common-dir)
      "Display the pathname of the srfi-common directory."
      (write-line (srfi-common-dir)))

    (define (srfi-data-file)
      (path-append (srfi-home-dir) "srfi-common" "admin" "srfi-data.scm"))

    (define-command (data-file)
      "Display the pathname of the SRFI database file."
      (write-line (srfi-data-file)))

    (define (srfi-signatures-file)
      (path-append (srfi-common-dir) "index" "signatures.scm"))

    (define (srfi-rss-file)
      (path-append (srfi-home-dir) "srfi-common" "srfi.rss"))

    (define-command (rss-file)
      "Display the pathname of the SRFI RSS file."
      (write-line (srfi-rss-file)))

    (define (srfi-dir num)
      (path-append (srfi-home-dir) (srfi-num-stem num)))

    (define-command (dir num)
      "Display the pathname of SRFI <num>."
      (write-line-about-srfi srfi-dir num))

    (define (srfi-html-file num)
      (path-append (srfi-dir num) (srfi-num-ext num ".html")))

    (define-command (html-file num)
      "Display the pathname of the document for SRFI <num>."
      (write-line-about-srfi srfi-html-file num))

    (define (srfi-landing-html-file num)
      (path-append (srfi-dir num) "index.html"))

    (define-command (landing-html-file num)
      "Display the pathname of the landing page for SRFI <num>."
      (write-line-about-srfi srfi-landing-html-file num))

    (define (srfi-abstract-html-file num)
      (path-append (srfi-home-dir)
                   "srfi-common"
                   "admin"
                   "abstracts"
                   (format "~a.html" num)))

    (define-command (abstract-html-file num)
      "Display the pathname of the landing page for SRFI <num>."
      (write-line-about-srfi srfi-abstract-html-file num))

    (define (srfi-do num thunk)
      (with-current-directory (srfi-dir num) thunk))

    (add-command!
     "do"
     '(num program arg ...)
     "Run <program> with <arg>s in the SRFI <num> directory."
     2
     #f
     (lambda (num program . args)
       (srfi-do (parse-srfi-number num)
                (lambda () (run-program (cons program args))))))

    (define (srfi-do-common thunk)
      (with-current-directory (srfi-common-dir) thunk))

    (add-command!
     "do-common"
     '(program arg ...)
     "Run <program> with <arg>s in the srfi-common directory."
     1
     #f
     (lambda (program . args)
       (srfi-do-common (lambda () (run-program (cons program args))))))))
