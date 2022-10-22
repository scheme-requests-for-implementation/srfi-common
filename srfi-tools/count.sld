(define-library (srfi-tools count)
  (export srfi-count-by-year
          srfi-count-by-author)
  (import (scheme base)
          (srfi-tools private list)
          (srfi-tools private hash-table)
          (srfi-tools private time)
          (srfi-tools private port)
          (srfi-tools private command)
          (srfi-tools data))
  (begin

    (define (srfi-count-by-year)
      (list-sort
       (pair<? < <)
       (tally
        (lambda (count)
          (for-each
           (lambda (srfi)
             (when (srfi-final? srfi)
               (let ((year (iso-date-year (srfi-done-date srfi))))
                 (count year))))
           (srfi-list))))))

    (define-command (count-by-year)
      "Display counts of SRFIs by year."
      (display-two-column-table (srfi-count-by-year)))

    (define (srfi-count-by-author)
      (list-sort
       (reverse-pair<? string<? <)
       (tally
        (lambda (count)
          (for-each
           (lambda (srfi)
             (for-each (lambda (author) (count (srfi-author-name author)))
                       (srfi-authors srfi)))
           (srfi-list))))))

    (define-command (count-by-author)
      "Display counts of SRFIs by year."
      (display-two-column-table (srfi-count-by-author)))))
