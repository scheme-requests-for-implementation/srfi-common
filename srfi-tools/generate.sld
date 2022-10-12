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
	  (srfi 1)
	  (srfi 95)
	  (srfi 130)
	  (srfi-tools data)
	  (srfi-tools html)
	  (srfi-tools path)
	  (srfi-tools private command)
	  (srfi-tools private html-writer)
	  (srfi-tools private path)
	  (srfi-tools private port)
	  (srfi-tools sxml-org))
  (include "generate.scm"))
