(define-library (srfi-tools data)
  (export srfi-format-keyword
          srfi-available-keywords
	  srfi-keyword-alist

          srfi-by-number

          srfi-number
          srfi-status
          srfi-title
          srfi-authors
          srfi-based-on
          srfi-see-also
          srfi-keywords
          srfi-library-name
          srfi-done-date
          srfi-draft-date

          srfi-draft?
          srfi-final?
          srfi-date-of-last-update

          srfi-author-name
          srfi-author-role
          srfi-format-author
          srfi-format-authors
          srfi-one-line-summary

          srfi-for-each
          srfi-filter

          srfi-data
          srfi-list
          srfi-drafts
          srfi-by-author
          srfi-search

          try-parse-srfi-number
          parse-srfi-number

          write-string-about-srfi
          write-line-about-srfi

          srfi-default-command
          srfi-default-number-command

	  keyword->name

	  write-srfi-list)
  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme cxr)
          (scheme file)
          (scheme read)
          (srfi-tools private error)
	  (srfi-tools private list)
          (srfi-tools private string)
          (srfi-tools private port)
          (srfi-tools private file)
          (srfi-tools private command)
          (srfi-tools core)
          (srfi-tools path))
  (include "data.scm")
  (cond-expand
   (mit
    (begin
      (set-record-type-unparser-method!
       srfi
       (standard-unparser-method
        'srfi
        (lambda (srfi port)
          (write-string " #" port)
          (write (srfi-number srfi) port)
          (write-char #\space port)
          (write (srfi-title srfi) port))))))
   (else)))
