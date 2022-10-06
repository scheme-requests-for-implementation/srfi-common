(define-library (srfi-tools private html-parser)
  (export make-html-parser html->sxml html-strip)
  (import (scheme base) (scheme char) (scheme cxr) (scheme write))
  (include "chibi-html-parser.scm"))
