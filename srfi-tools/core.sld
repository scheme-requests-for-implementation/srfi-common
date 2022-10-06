(define-library (srfi-tools core)
  (export srfi-num-stem
          srfi-num-ext

          try-parse-srfi-number
          parse-srfi-number

          write-string-about-srfi
          write-line-about-srfi

          srfi-default-command
          srfi-default-number-command)
  (import (scheme base))
  (begin

    (define (srfi-num-stem num)
      (string-append "srfi-" (number->string num)))

    (define (srfi-num-ext num ext)
      (string-append (srfi-num-stem num) ext))

    ;;

    (define (try-parse-srfi-number string)
      (let ((num (string->number string)))
        (and (integer? num) (>= num 0) num)))

    (define (parse-srfi-number string)
      (or (try-parse-srfi-number string)
          (error "Not a valid SRFI number" string)))

    ;;

    (define (write-string-about-srfi what-about num)
      (let ((num (parse-srfi-number num)))
        (write-string (what-about num))))

    (define (write-line-about-srfi what-about num)
      (write-string-about-srfi what-about num)
      (newline))

    ;;

    (define srfi-default-command
      (make-parameter "list"))

    (define srfi-default-number-command
      (make-parameter "browse"))))
