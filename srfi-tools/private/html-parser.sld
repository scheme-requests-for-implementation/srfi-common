(define-library (srfi-tools private html-parser)
  (export make-html-parser
          html->sxml
          html-parser-position
          html-strip
          read-html-file
          read-html-token)
  (import (except (scheme base) read-char)
          (rename (only (scheme base) read-char)
                  (read-char %base-read-char))
          (scheme char)
          (scheme cxr)
          (scheme file)
          (scheme write))
  (include "chibi-html-parser.scm")
  (begin
    (define (read-html-file file)
      (with-input-from-file file html->sxml))))
