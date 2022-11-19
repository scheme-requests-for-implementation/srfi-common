(define-library (srfi-tools mail)
  (export srfi-mail-archive-url
          srfi-mail-address
          srfi-mailto-url)
  (import (scheme base)
          (scheme char)

          (srfi-tools private command)
	  (srfi-tools private html-writer)
          (srfi-tools private list)
          (srfi-tools private path)
          (srfi-tools private port)
          (srfi-tools private string)
          (srfi-tools private time)

          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools url))
  (include "mail.scm"))
