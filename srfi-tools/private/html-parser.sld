(define-library (srfi-tools private html-parser)
  (export make-html-parser
          html->sxml
          html-strip

          read-html-file)
  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (scheme file)
          (scheme write))
  (include "chibi-html-parser.scm")
  (begin
    (define (read-html-file file)
      (with-input-from-file file html->sxml))))
