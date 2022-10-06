(define-library (srfi-tools private time)
  (export make-date
          date->string
          date->time-utc
          time<?
          ;;
          iso-date-year
          iso-date-month
          iso-date-day
          parse-iso-date
          iso-date->date)
  (import (scheme base)
          (only (srfi 19)
                make-date
                date->string
                date->time-utc
                time<?))
  (begin

    (define (iso-date-year string)
      (string->number (string-copy string 0 4)))

    (define (iso-date-month string)
      (string->number (string-copy string 5 7)))

    (define (iso-date-day string)
      (string->number (string-copy string 8 10)))

    (define (parse-iso-date string)
      (values (iso-date-year string)
              (iso-date-month string)
              (iso-date-day string)))

    (define (iso-date->date string)
      (let-values (((year month day) (parse-iso-date string)))
        (make-date 0 0 0 12 day month year -480)))))
