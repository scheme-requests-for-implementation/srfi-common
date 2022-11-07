(define-library (srfi-tools private html-writer)
  (export sxml-display-as-html
          sxml-display-as-text
          html-escape
	  write-html)
  (import (scheme base)
	  (scheme file)
          (scheme write)
	  (srfi 227))
  (include "chibi-sxml.scm")
  (begin
    (define (write-html file sxml)
      (call-with-output-file file
	(lambda (port) (sxml-display-as-html sxml port #true))))))
