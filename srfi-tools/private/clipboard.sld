(define-library (srfi-tools private clipboard)
  (export copy-html-to-clipboard)
  (import (scheme base)

	  (srfi-tools private html-writer)
          (srfi-tools private file)
          (srfi-tools private os)
          (srfi-tools private port))
  (begin

    (define (copy-html-to-clipboard/mac html)
      (let ((temp-file (make-temp-file-name)))
        (with-output-to-text-file temp-file
          (lambda ()
            (write-string "set the clipboard to «data HTML")
            (write-bytevector-as-hex (string->utf8 html))
            (write-string "»")))
        (run-program `("osascript" ,temp-file))))

    (define (copy-html-to-clipboard/x html)
      (let ((temp-file (make-temp-file-name)))
        (write-text-file temp-file html)
        (run-program
         `("xclip"
           "-selection" "clipboard"
           "-target" "text/html"
           ,temp-file))))

    (define (copy-html-to-clipboard html)
      (cond ((equal? "Darwin\n" (run-program/get-output-string '("uname")))
             (copy-html-to-clipboard/mac html))
            (else
             (copy-html-to-clipboard/x html))))))
