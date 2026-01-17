(define-library (srfi-tools main)
  (export)
  (import (scheme base)
          (scheme process-context)
          (only (srfi 193) command-args))
  (import (srfi-tools private list)
          (srfi-tools private string)
          (srfi-tools private port)
          (srfi-tools private error)
          (srfi-tools private command)
          (srfi-tools private os))
  (import (srfi-tools chart)
          (srfi-tools checklink)
          (srfi-tools convert)
          (srfi-tools core)
          (srfi-tools count)
          (srfi-tools data)
          (srfi-tools generate)
          (srfi-tools git)
          (srfi-tools github)
          (srfi-tools help)
          (srfi-tools html)
          (srfi-tools info)
          (srfi-tools interactive)
          (srfi-tools legal)
          (srfi-tools library)
          (srfi-tools mail)
          (srfi-tools missing)
          (srfi-tools path)
          (srfi-tools pre)
          (srfi-tools rss)
          (srfi-tools signature)
          (srfi-tools simplelists)
          (srfi-tools tar)
          (srfi-tools toc))
  (cond-expand
   ((library (srfi-tools local))
    (import (srfi-tools local)))
   (else))
  (begin

    (define (main** args)
      (if (null? args)
          (command-apply
           (command-by-name (srfi-default-command))
           '())
          (let ((num (and (try-parse-srfi-number (first args))
                          (first args))))
            (cond ((and num (not (null? (rest args))))
                   (usage "Too many args"))
                  (num
                   (command-apply
                    (command-by-name (srfi-default-number-command))
                    (list num)))
                  (else
                   (let* ((name (first args))
                          (args (rest args))
                          (command (command-by-name name)))
                     (if (or (< (length args)
                                (command-min-args command))
                             (and (command-max-args command)
                                  (> (length args)
                                     (command-max-args command))))
                         (apply usage
                                "usage: srfi"
                                name
                                (command-arg-names command))
                         (command-apply command args))))))))

    (define (main* args)
      (let ((debug? (boolean-from-envar "SRFI_DEBUG")))
        (if debug?
            (main** args)
            (guard (err (else
                         (display-error err)
                         (exit #f)))
                   (main** args))))))
  (cond-expand
   (chibi
    (begin
      (define (main command-line)
        (main* (rest command-line)))))
   ((or chicken gauche)
    (begin
      (main* (command-args))))))
