;;;; Run checklink command.

;;;; To install <checklink>, run <apt install w3c-linkchecker>.

(define-command (checklink num)
  (run-program (list "checklink"
		     "--follow-file-links"
		     (srfi-html-file (string->number num)))))