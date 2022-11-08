(define-library (srfi-tools private sxml-org)
  (export sxml-display-as-org
	  write-org-file)
  (import (scheme base)
	  (scheme write)

	  (srfi-tools private error)
	  (srfi-tools private file)
	  (srfi-tools private sxml))
  (include "sxml-org.scm"))