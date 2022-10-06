(define-library (srfi-tools private hash-table)
  (export make-hash-table
          hash-table-exists?
          hash-table-set!
          hash-table-update!/default
          hash-table-keys
          hash-table->alist
          tally)
  (import (scheme base)
          (srfi 69))
  (begin

    (define (tally proc)
      (define (inc x) (+ x 1))
      (let* ((counts (make-hash-table))
             (count (lambda (key)
                      (hash-table-update!/default counts key inc 0))))
        (proc count)
        (hash-table->alist counts)))))
