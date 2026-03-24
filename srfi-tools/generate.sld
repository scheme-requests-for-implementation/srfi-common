(define-library (srfi-tools generate)
  (export write-single-srfi-landing-page
	  write-single-srfi-readme
	  write-srfi-about-page
	  write-srfi-editors-page
	  write-srfi-faq-page
	  write-srfi-history-page
	  write-srfi-home-page
	  write-srfi-landing-pages
	  write-srfi-list-subscribe-page
	  write-srfi-privacy-page
	  write-srfi-process-page
	  write-srfi-readmes)
  (import (scheme base)

	  (srfi 19)

	  (srfi-tools private command)
	  (srfi-tools private file)
	  (srfi-tools private html-writer)
	  (srfi-tools private list)
	  (srfi-tools private path)
	  (srfi-tools private port)
	  (srfi-tools private string)
	  (srfi-tools private sxml-org)

	  (srfi-tools data)
	  (srfi-tools html)
	  (srfi-tools path))
  (include "generate.scm"))
