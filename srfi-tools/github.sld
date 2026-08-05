(define-library (srfi-tools github)
  (export srfi-github-org
          srfi-github-authorization-token
          srfi-github-url
          srfi-github-https-url
          srfi-github-ssh-url
          srfi-github-compare-url
          srfi-create-github-repository
          srfi-subscribe-to-github-repository)
  (import (scheme base)
          (scheme process-context)

	  (srfi-tools private command)
          (srfi-tools private format)
          (srfi-tools private os)
          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools private error)
          (srfi-tools url))
  (cond-expand
   ((library (srfi 180))
    (import (srfi 180)))
   (chibi
    (import (chibi json))))
  (begin

    (define (srfi-github-org)
      "scheme-requests-for-implementation")

    (define (srfi-github-authorization-token)
      (get-environment-variable "SRFI_GITHUB_TOKEN"))

    (define (github-api-repos)
      (string-append "https://api.github.com/orgs/"
                     (srfi-github-org)
                     "/repos"))

    (define (srfi-github-relative num)
      (string-append (srfi-github-org) "/" (srfi-num-stem num)))

    (define (srfi-github-relative-git num)
      (string-append (srfi-github-relative num) ".git"))

    (define (srfi-github-url num)
      (string-append "https://github.com/"
                     (srfi-github-relative num)))

    (define-command (github-url num)
      "Display the GitHub URL for SRFI <num>."
      (write-line-about-srfi srfi-github-url num))

    ;; Is this superfluous? `srfi-github-url` can fetch both the repo
    ;; and the web page.
    (define (srfi-github-https-url num)
      (string-append "https://github.com/"
                     (srfi-github-relative-git num)))

    (define (srfi-github-ssh-url num)
      (string-append "git@github.com:"
                     (srfi-github-relative-git num)))

    (define (srfi-github-compare-url num old-git-ref new-git-ref)
      (format "~a/compare/~a..~a"
              (srfi-github-url num)
              old-git-ref
              new-git-ref))

    (define (github-api-subscription num)
      (format "https://api.github.com/repos/~a/~a/subscription"
              (srfi-github-org)
              (srfi-num-stem num)))

    (define (github-api-request method url data)
      (run-program/get-output-string
       (list
        "curl"
        "--data" data
        "--fail"
        "--header" (format "Authorization: token ~a"
                           (srfi-github-authorization-token))
        "--request" method
        "--show-error"
        "--silent"
        url)))

    (define (check-field name expected)
      (lambda (json)
        (let ((field (assoc name json)))
          (and field (equal? (cdr field) expected)))))

    (define (github-api-request/check method url data check message)
      (let* ((response (github-api-request method url data))
             (json (json-read (open-input-string response))))
        (unless (check json)
          (user-error message))))

    (define (srfi-subscribe-to-github-repository num)
      (github-api-request/check
       "PUT"
       (github-api-subscription num)
       "{ \"subscribed\": true, \"ignored\": false }"
       (check-field 'subscribed #t)
       (format "Failed to subscribe to ~a." (srfi-num-stem num))))

    (define (srfi-create-github-repository num)
      (github-api-request/check
       "POST"
       (github-api-repos)
       (format (string-append
                "{ \"name\": \"srfi-~a\""
                ", \"description\": \"~a\""
                ", \"has_issues\": false"
                ", \"has_wiki\": false"
                " }")
               num
               (srfi-title num))
       (check-field 'name (srfi-num-stem num))
       (format "Failed to create ~a." (srfi-num-stem num)))
      (srfi-subscribe-to-github-repository num))

    (define-command (create-github-repository num)
      "Create a GitHub repository for SRFI num."
      (srfi-create-github-repository (parse-srfi-number num)))

    (define-command (subscribe-to-github-repository num)
      "Subscribe to notifications for SRFI <num>."
      (srfi-subscribe-to-github-repository (parse-srfi-number num)))))