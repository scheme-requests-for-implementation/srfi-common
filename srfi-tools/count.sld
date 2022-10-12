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
          (srfi-for-each
           (lambda (srfi)
             (when (srfi-final? srfi)
               (let ((year (iso-date-year (srfi-done-date srfi))))
                 (count year)))))))))

    (define-command (count-by-year)
      (display-two-column-table (srfi-count-by-year)))

    (define (srfi-count-by-author)
      (list-sort
       (reverse-pair<? string<? <)
       (tally
        (lambda (count)
          (srfi-for-each
           (lambda (srfi)
             (for-each (lambda (author) (count (srfi-author-name author)))
                       (srfi-authors srfi))))))))

    (define-command (count-by-author)
      (display-two-column-table (srfi-count-by-author)))))
