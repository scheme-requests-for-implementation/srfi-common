(define-library (srfi-tools generate)
  (export write-single-srfi-landing-page
	  write-single-srfi-readme
	  write-srfi-home-page
	  write-srfi-landing-pages
	  write-srfi-list-subscribe-page
	  write-srfi-readmes)
  (import (scheme base)
	  (scheme file)
	  (scheme write)

	  (srfi-tools private command)
	  (srfi-tools private html-writer)
	  (srfi-tools private list)
	  (srfi-tools private path)
	  (srfi-tools private port)
	  (srfi-tools private string)

	  (srfi-tools data)
	  (srfi-tools html)
	  (srfi-tools path)
	  (srfi-tools sxml-org))
  (include "generate.scm"))
