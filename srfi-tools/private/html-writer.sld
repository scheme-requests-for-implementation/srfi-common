(define-library (srfi-tools private html-writer)
  (export sxml-display-as-html
          sxml-display-as-text
          html-escape)
  (import (scheme base)
          (scheme write))
  (include "chibi-sxml.scm"))
