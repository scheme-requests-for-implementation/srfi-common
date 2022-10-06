(define-library (srfi-tools github)
  (export srfi-github-org
          srfi-github-authorization-token
          srfi-github-https-url
          srfi-github-ssh-url
          srfi-create-github-repository)
  (import (scheme base)
          (srfi-tools private command)
          (srfi-tools private format)
          (srfi-tools private os)
          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools url))
  (begin

    (define (srfi-github-org)
      "scheme-requests-for-implementation")

    ;; TODO: Get this from an environment variable.
    (define srfi-github-authorization-token
      (make-parameter ""))

    (define (github-api-repos)
      (string-append "https://api.github.com/orgs/"
                     (srfi-github-org)
                     "/repos"))

    (define (srfi-github-relative num)
      (string-append "scheme-requests-for-implementation/"
                     (srfi-num-stem num)))

    (define (srfi-github-relative-git num)
      (string-append (srfi-github-relative num) ".git"))

    (define (srfi-github-https-url num)
      (string-append "https://github.com/"
                     (srfi-github-relative-git num)))

    (define (srfi-github-ssh-url num)
      (string-append "git@github.com:"
                     (srfi-github-relative-git num)))

    (define (srfi-create-github-repository num)
      (let ((description (srfi-title (srfi-by-number num))))
        (run-program
         (list
          "curl"
          "-i"
          "-H" (format "Authorization: token ~a"
                       (srfi-github-authorization-token))
          "-d" (format (string-append
                        "{ \"name\": \"srfi-~a\""
                        ", \"description\": \"~a\""
                        ", \"has_issues\": false"
                        ", \"has_wiki\": false"
                        " }")
                       num
                       description)
          (github-api-repos)))))

    (define-command (create-github-repository num)
      (srfi-create-github-repository (parse-srfi-number num)))))
