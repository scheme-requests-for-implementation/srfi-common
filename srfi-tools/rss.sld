(define-library (srfi-tools rss)
  (export srfi-rss-sxml
          srfi-rss-html)
  (import (scheme base)
          (scheme file)
          (srfi 19)
	  (srfi-tools private html-writer)
          (srfi-tools private list)
          (srfi-tools private file)
          (srfi-tools private format)
          (srfi-tools private time)
          (srfi-tools private port)
          (srfi-tools private sxml)
          (srfi-tools private html-parser)
          (srfi-tools private html-writer)
          (srfi-tools private command)
          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools url)
          (srfi-tools html))
  (include "rss.scm"))
