(define-library (srfi-tools interactive)
  (export srfi-open-dir
          srfi-open
          srfi-open-landing
          srfi-browse
          srfi-browse-url
          srfi-browse-landing
          srfi-browse-landing-url
          srfi-browse-github-url
          srfi-pager
          srfi-edit
          srfi-browse-mail-archive-url)
  (import (scheme base)
          (srfi-tools private external)
          (srfi-tools private os)
          (srfi-tools private command)
          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools html)
          (srfi-tools mail)
          (srfi-tools url)
          (srfi-tools github))
  (begin

    (define (srfi-open-dir num)
      (desktop-open (srfi-dir num)))

    (define-command (open-dir num)
      (srfi-open-dir (parse-srfi-number num)))

    (define (srfi-open-home-dir)
      (desktop-open (srfi-home-dir)))

    (define-command (open-home-dir)
      (srfi-open-home-dir))

    ;;

    (define (srfi-open num)
      (desktop-open (srfi-html-file num)))

    (define-command (open num)
      (srfi-open (parse-srfi-number num)))

    (define (srfi-open-landing num)
      (desktop-open (srfi-landing-html-file num)))

    (define-command (open-landing num)
      (srfi-open-landing (parse-srfi-number num)))

    ;;

    (define (srfi-browse num)
      (browse-url (srfi-html-file num)))

    (define-command (browse num)
      (srfi-browse (parse-srfi-number num)))

    (define (srfi-browse-url num)
      (browse-url (srfi-html-url num)))

    (define-command (browse-url num)
      (srfi-browse-url (parse-srfi-number num)))

    (define (srfi-browse-landing num)
      (browse-url (srfi-landing-html-file num)))

    (define-command (browse-landing num)
      (srfi-browse-landing (parse-srfi-number num)))

    (define (srfi-browse-landing-url num)
      (browse-url (srfi-landing-url num)))

    (define-command (browse-landing-url num)
      (srfi-browse-landing-url (parse-srfi-number num)))

    (define (srfi-browse-github-url num)
      (browse-url (srfi-github-url num)))

    (define-command (browse-github-url num)
      (srfi-browse-github-url (parse-srfi-number num)))

    ;;

    (define (srfi-pager num)
      (run-pager-on-url (srfi-html-file num)))

    (define-command (pager num)
      (srfi-pager (parse-srfi-number num)))

    ;;

    (define (srfi-edit num)
      (edit-text-file (srfi-html-file num)))

    (define-command (edit num)
      (srfi-edit (parse-srfi-number num)))

    ;;

    (define (srfi-browse-mail-archive-url num)
      (browse-url (srfi-mail-archive-url num)))

    (define-command (browse-mail-archive-url num)
      (srfi-browse-mail-archive-url (parse-srfi-number num)))))
