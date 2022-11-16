(define-library (srfi-tools data)
  (export srfi-format-keyword
          srfi-available-keywords
	  srfi-keyword-alist

          srfi-by-number
          srfi-last-number

          srfi-number
          srfi-status
          srfi-status-string
          srfi-status->name
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
	  srfi-date-to-show

          srfi-author-name
          srfi-author-role
          srfi-format-author
          srfi-format-authors
          srfi-format

          srfi-data
          srfi-list
          srfi-range
          srfi-near
          srfi-age-in-days
          srfi-drafts
          srfi-by-author
          srfi-by-keyword
          srfi-search

          try-parse-srfi-number
          parse-srfi-number

          write-string-about-srfi
          write-line-about-srfi

	  keyword->name

	  write-custom-srfi-list
          write-srfi-list)
  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme cxr)
          (scheme read)
	  (srfi-tools private error)
	  (srfi-tools private list)
          (srfi-tools private format)
          (srfi-tools private string)
          (srfi-tools private port)
          (srfi-tools private file)
          (srfi-tools private time)
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
