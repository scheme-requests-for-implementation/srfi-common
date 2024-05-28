(define-library (srfi-tools generate)
  (export write-single-srfi-landing-page
	  write-single-srfi-readme
          write-single-srfi-document
	  write-srfi-home-page
	  write-srfi-landing-pages
	  write-srfi-list-subscribe-page
	  write-srfi-readmes)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (scheme write)
          (srfi 1)
	  (srfi 19)
          (srfi 28)

	  (srfi-tools private command)
	  (srfi-tools private html-writer)
	  (srfi-tools private list)
	  (srfi-tools private path)
	  (srfi-tools private port)
	  (srfi-tools private string)
          (srfi-tools private sxml-org)
          (srfi-tools private sxml)
          (srfi-tools private html-parser)
          (srfi-tools private html-writer)

	  (srfi-tools data)
	  (srfi-tools html)
          (srfi-tools legal)
	  (srfi-tools path))
  (include "generate.scm"))
