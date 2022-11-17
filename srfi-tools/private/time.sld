(define-library (srfi-tools private time)
  (export add-duration
          current-date
          date->string
          date->time-utc
          date-year
          julian-day->date
          date->julian-day
          make-date
          time<?)
  (export date->iso-date
          date->julian
          days-between
          iso-date-year
          iso-date-month
          iso-date-day
	  parse-iso-date
          iso-date->date)
  (import (scheme base)
          (only (srfi 19)
		current-date
		date->julian-day
		julian-day->date
		make-date
                add-duration
                date->string
                date->time-utc
                date-year
                time<?))
  (begin

    (define (date->iso-date date)
      (date->string date "~1"))

    (define (date->julian date)
      (ceiling (date->julian-day date))) ; Compensate for a bug in Chibi Scheme's
					 ; SRFI 19 implementation that breaks
					 ; julian-date->date on non-integer
					 ; values.

    (define (days-between start-date end-date)
      (- (truncate (date->julian-day end-date))
         (truncate (date->julian-day start-date))))

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
