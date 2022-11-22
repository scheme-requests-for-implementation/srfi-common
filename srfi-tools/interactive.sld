(define-library (srfi-tools interactive)
  (export srfi-open-dir
          srfi-open-home-dir
          srfi-open-file
          srfi-open-landing-file
          srfi-browse
          srfi-browse-file
          srfi-browse-landing
          srfi-browse-landing-file
          srfi-browse-github
          srfi-browse-home
          srfi-lucky
          srfi-pager
          srfi-edit
          srfi-browse-mail
          srfi-compose-mail)
  (import (scheme base)
          (scheme char)

          (srfi-tools private external)
          (srfi-tools private format)
          (srfi-tools private list)
          (srfi-tools private string)
          (srfi-tools private os)
          (srfi-tools private command)
          (srfi-tools private port)

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
      "Open home directory for SRFI <num>."
      (srfi-open-dir (parse-srfi-number num)))

    (define (srfi-open-home-dir)
      (desktop-open (srfi-home-dir)))

    (define-command (open-home-dir)
      "Open home directory for all SRFIs."
      (srfi-open-home-dir))

    ;;

    (define (srfi-open-file num)
      (desktop-open (srfi-html-file num)))

    (define-command (open-file num)
      "Open local copy of SRFI document <num>."
      (srfi-open-file (parse-srfi-number num)))

    (define (srfi-open-landing-file num)
      (desktop-open (srfi-landing-html-file num)))

    (define-command (open-landing-file num)
      "Open local copy of SRFI landing page <num>."
      (srfi-open-landing-file (parse-srfi-number num)))

    ;;

    (define (srfi-browse num)
      (browse-url (srfi-html-url num)))

    (define-command (browse num)
      "Browse SRFI document <num> on SRFI site."
      (srfi-browse (parse-srfi-number num)))

    (define (srfi-browse-file num)
      (browse-url (srfi-html-file num)))

    (define-command (browse-file num)
      "Browse local copy of SRFI document <num>."
      (srfi-browse-file (parse-srfi-number num)))

    (define (srfi-browse-landing num)
      (browse-url (srfi-landing-url num)))

    (define-command (browse-landing num)
      "Browse SRFI landing page <num> on SRFI site."
      (srfi-browse-landing (parse-srfi-number num)))

    (define (srfi-browse-landing-file num)
      (browse-url (srfi-landing-html-file num)))

    (define-command (browse-landing-file num)
      "Browse local copy of SRFI landing page <num>."
      (srfi-browse-landing-file (parse-srfi-number num)))

    (define (srfi-browse-github num)
      (browse-url (srfi-github-url num)))

    (define-command (browse-github num)
      "Browse GitHub page for SRFI <num>."
      (srfi-browse-github (parse-srfi-number num)))

    (define (srfi-browse-home)
      (browse-url (srfi-home-url)))

    (define-command (browse-home)
      "Browse home page of the SRFI project."
      (srfi-browse-home))

    (define (srfi-lucky words)
      (let ((matches (srfi-search words)))
        (cond ((null? matches)
               (error "No luck. Try another query?"))
              (else
	       (unless (null? (cdr matches))
		 (write-srfi-list matches)
		 (newline))
	       (let ((srfi (car matches)))
		 (write-line (format "Opening ~a." (srfi-format srfi)))
		 (srfi-browse (srfi-number srfi)))))))

    (add-command!
     "lucky"
     '(word ...)
     "Browse first SRFI whose title matches all <word>s."
     1
     #f
     (lambda words (srfi-lucky words)))

    ;;

    (define (srfi-pager num)
      (run-pager-on-url (srfi-html-file num)))

    (define-command (pager num)
      "Run pager on local copy of SRFI document <num>."
      (srfi-pager (parse-srfi-number num)))

    ;;

    (define (srfi-edit num)
      (edit-text-file (srfi-html-file num)))

    (define-command (edit num)
      "Edit local copy of SRFI document <num>."
      (srfi-edit (parse-srfi-number num)))

    ;;

    (define (srfi-browse-mail num)
      (browse-url (srfi-mail-archive-url num)))

    (define-command (browse-mail num)
      "Browse mailing list archive for SRFI <num>."
      (srfi-browse-mail (parse-srfi-number num)))

    (define (srfi-compose-mail num)
      (desktop-open (srfi-mailto-url num)))

    (define-command (compose-mail num)
      "Open email app with a new email to SRFI <num> mailing list."
      (srfi-compose-mail (parse-srfi-number num)))))
