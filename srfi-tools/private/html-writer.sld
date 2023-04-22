(define-library (srfi-tools private html-writer)
  (export sxml-display-as-html
          sxml-display-as-text
          html-escape
	  sxml->xml
          write-html-file)
  (import (scheme base)
	  (scheme file)
          (scheme write))
  (include "chibi-sxml.scm")
  (begin
    (define (write-html-file file sxml)
      (call-with-output-file file
	(lambda (port) (sxml-display-as-html sxml port #true))))))
