(define-library (srfi-tools local)
  (import (scheme base)
          (srfi-tools private command)
          (srfi-tools private external)
          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools interactive))
  (begin

    ;; You can define custom commans in this file. For example:

    (define-command (links num)
      (browse-url-with "links" (srfi-html-file (parse-srfi-number num))))

    (define-command (lynx num)
      (browse-url-with "lynx" (srfi-html-file (parse-srfi-number num))))

    (define-command (w3m num)
      (browse-url-with "w3m" (srfi-html-file (parse-srfi-number num))))

    ;; Define what you want the shell command `srfi` to do.
    (srfi-default-command "list")

    ;; Define what you want the shell command `srfi 123` to do.
    (srfi-default-number-command "info")))
