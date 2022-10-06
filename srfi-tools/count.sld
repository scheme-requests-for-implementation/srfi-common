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
      (for-each (lambda (pair)
                  (let ((year (car pair))
                        (count (cdr pair)))
                    (disp year "  " count)))
                (srfi-count-by-year)))

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
      (for-each (lambda (pair)
                  (let ((name (car pair))
                        (count (cdr pair)))
                    (disp count "  " name)))
                (srfi-count-by-author)))))
