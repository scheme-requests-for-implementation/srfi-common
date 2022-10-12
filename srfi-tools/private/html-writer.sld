(define-library (srfi-tools private html-writer)
  (export sxml-display-as-html
          sxml-display-as-text
          html-escape)
  (import (scheme base)
          (scheme write)
	  (srfi 227))
  (include "chibi-sxml.scm"))
