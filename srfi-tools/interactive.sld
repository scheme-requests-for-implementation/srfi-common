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
          srfi-browse-mail-archive-url
          srfi-send-mail)
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
      "Open the home directory for SRFI <num>."
      (srfi-open-dir (parse-srfi-number num)))

    (define (srfi-open-home-dir)
      "Open the home directory for all SRFIs."
      (desktop-open (srfi-home-dir)))

    (define-command (open-home-dir)
      "Open the home directory for all SRFIs."
      (srfi-open-home-dir))

    ;;

    (define (srfi-open num)
      (desktop-open (srfi-html-file num)))

    (define-command (open num)
      "Open document for SRFI <num>."
      (srfi-open (parse-srfi-number num)))

    (define (srfi-open-landing num)
      (desktop-open (srfi-landing-html-file num)))

    (define-command (open-landing num)
      "Open landing page for SRFI <num>."
      (srfi-open-landing (parse-srfi-number num)))

    ;;

    (define (srfi-browse num)
      (browse-url (srfi-html-file num)))

    (define-command (browse num)
      "Browse local document for SRFI <num>."
      (srfi-browse (parse-srfi-number num)))

    (define (srfi-browse-url num)
      (browse-url (srfi-html-url num)))

    (define-command (browse-url num)
      "Browse SRFI <num> on SRFI site."
      (srfi-browse-url (parse-srfi-number num)))

    (define (srfi-browse-landing num)
      (browse-url (srfi-landing-html-file num)))

    (define-command (browse-landing num)
      "Browse locaol landing page for SRFI <num>."
      (srfi-browse-landing (parse-srfi-number num)))

    (define (srfi-browse-landing-url num)
      (browse-url (srfi-landing-url num)))

    (define-command (browse-landing-url num)
      "Browse landing page for SRFI <num> on SRFI site."
      (srfi-browse-landing-url (parse-srfi-number num)))

    (define (srfi-browse-github-url num)
      (browse-url (srfi-github-url num)))

    (define-command (browse-github-url num)
      "Browse Github page for SRFI <num>."
      (srfi-browse-github-url (parse-srfi-number num)))

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
     "Browse the first SRFI whose title matches all <word>s."
     1
     #f
     (lambda words (srfi-lucky words)))

    ;;

    (define (srfi-pager num)
      (run-pager-on-url (srfi-html-file num)))

    (define-command (pager num)
      "Run pager on local file for SRFI <num>."
      (srfi-pager (parse-srfi-number num)))

    ;;

    (define (srfi-edit num)
      (edit-text-file (srfi-html-file num)))

    (define-command (edit num)
      "Edit SRFI <num>."
      (srfi-edit (parse-srfi-number num)))

    ;;

    (define (srfi-browse-mail-archive-url num)
      (browse-url (srfi-mail-archive-url num)))

    (define-command (browse-mail-archive-url num)
      "Browse the email archive for SRFI <num>."
      (srfi-browse-mail-archive-url (parse-srfi-number num)))

    (define (srfi-send-mail num)
      (desktop-open (srfi-mailto-url num)))

    (define-command (send-mail num)
      "Open email app with a new email to SRFI <num> mailing list."
      (srfi-send-mail (parse-srfi-number num)))))
