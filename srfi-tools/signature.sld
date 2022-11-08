(define-library (srfi-tools signature)
  (export srfi-signatures-data
          srfi-signatures
          srfi-identifier-list
          srfi-identifier-signatures
          srfi-identifier-search)
  (import (scheme base)
          (scheme char)
          (scheme file)

          (srfi-tools private command)
          (srfi-tools private list)
          (srfi-tools private port)
          (srfi-tools private pretty-print)
          (srfi-tools private string)

          (srfi-tools core)
          (srfi-tools path))
  (begin

    (define (srfi-signatures-data)
      (with-input-from-text-file (srfi-signatures-file)
        read-all))

    (define (signature-source sig)
      (and (list? sig)
           (let ((entry (assoc 'source sig)))
             (and entry (second entry)))))

    (define (signature-identifier sig)
      (and (list? sig)
           (list? (first sig))
           (list? (second (first sig)))
           (equal? 'signature (first (first sig)))
           (first (second (first sig)))))

    (define (srfi-signatures num)
      (filter (lambda (sig) (equal? `(srfi ,num) (signature-source sig)))
              (srfi-signatures-data)))

    (define-command (signatures num)
      "Output the type signatures from SRFI <num>."
      (pretty-print-all (srfi-signatures (parse-srfi-number num))))

    (define (srfi-identifier-list)
      (filter-map signature-identifier (srfi-signatures-data)))

    (define-command (identifier-list)
      "Output the type signatures from all SRFIs."
      (for-each disp (srfi-identifier-list)))

    (define (srfi-identifier-signatures identifier)
      (let ((identifier (if (string? identifier)
                            (string->symbol identifier)
                            identifier)))
        (filter (lambda (sig) (equal? identifier (signature-identifier sig)))
                (srfi-signatures-data))))

    (define-command (identifier-signatures identifier)
      "Output the type signatures for <identifier> from all SRFIs."
      (pretty-print-all (srfi-identifier-signatures identifier)))

    (define (srfi-identifier-search string)
      (list-sort
       (pair<? string<? (lambda (a b) (< (second a) (second b))))
       (filter-map (lambda (sig)
                     (let* ((id (signature-identifier sig))
                            (id (and id (symbol->string id))))
                       (and id
                            (string-contains id string)
                            (cons id (signature-source sig)))))
                   (srfi-signatures-data))))

    (define-command (identifier-search string)
      "Output the type signatures for <identifier> from all SRFIs."
      (display-two-column-table (srfi-identifier-search string)))))
