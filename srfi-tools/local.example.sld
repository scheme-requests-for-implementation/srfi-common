(define-library (srfi-tools local)
  (import (scheme base)
          (srfi-tools private command)
          (srfi-tools private external)
          (srfi-tools data)
          (srfi-tools interactive))
  (begin

    (define-command (grep num text)
      (srfi-run num (list "grep" text)))

    ;; Examples:

    (define-command (links num)
      (browse-url-with "links" (srfi-html-url num)))

    (define-command (lynx num)
      (browse-url-with "lynx" (srfi-html-url num)))

    (define-command (w3m num)
      (browse-url-with "w3m" (srfi-html-url num)))

    ;; Define what you want the shell command `srfi` to do.
    (srfi-default-command 'list)

    ;; Define what you want the shell command `srfi 123` to do.
    (srfi-default-number-command 'info)))
