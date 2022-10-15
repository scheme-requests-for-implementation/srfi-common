(define-library (srfi-tools git)
  (export srfi-git-https-url
          srfi-git-ssh-url)
  (import (scheme base)
          (scheme file)
          (srfi-tools private command)
          (srfi-tools private format)
          (srfi-tools private port)
          (srfi-tools private os)
          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools url)
          (srfi-tools github))
  (begin

    (define srfi-git-https-url srfi-github-https-url)

    (define-command (git-https-url num)
      "Display the Git HTTPS URL for SRFI <num>."
      (write-line-about-srfi srfi-git-https-url num))

    (define srfi-git-ssh-url srfi-github-ssh-url)

    (define-command (git-ssh-url num)
      "Display the Git SSH URL for SRFI <num>."
      (write-line-about-srfi srfi-git-ssh-url num))))
