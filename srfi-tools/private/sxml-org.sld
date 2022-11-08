(define-library (srfi-tools private sxml-org)
  (export sxml-display-as-org
	  write-org-file)
  (import (scheme base)
	  (scheme file)
	  (scheme write)
	  (srfi-tools private error)
	  (srfi-tools private sxml))
  (include "sxml-org.scm"))