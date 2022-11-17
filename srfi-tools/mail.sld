(define-library (srfi-tools mail)
  (export srfi-mail-archive-url
          srfi-mail-address
          srfi-mailto-url)
  (import (scheme base)
          (scheme char)

          (srfi-tools private command)
          (srfi-tools private list)
          (srfi-tools private path)
          (srfi-tools private port)
          (srfi-tools private string)
          (srfi-tools private time)

          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools url))
  (begin

    (define (srfi-mail-archive-url num)
      (string-append "https://srfi-email.schemers.org/"
                     (srfi-num-stem num) "/"))

    (define-command (mail-archive-url num)
      "Display mail archive URL for SRFI <num>."
      (write-line-about-srfi srfi-mail-archive-url num))

    (define (srfi-mail-address num)
      (string-append (srfi-num-stem num)
                     "@srfi.schemers.org"))

    (define-command (mail-address num)
      "Display email address URL for SRFI <num>."
      (write-line-about-srfi srfi-mail-address num))

    (define (srfi-mailto-url num)
      (string-append "mailto:"
                     (srfi-mail-address num)
                     "?subject=" (url-hexify-string (srfi-title num))))

    (define (cap-first string)
      (if (zero? (string-length string)) ""
          (string-append (string-upcase (string-copy string 0 1))
                         (string-copy string 1 (string-length string)))))

    (define (srfi-last-call num author-name-part)
      (let* ((srfi (srfi-by-number num))
             (url (srfi-landing-url num))
             (num (number->string num))
             (title (srfi-title srfi))
             (author (srfi-author-name
                      (or (find (lambda (author)
                                  (string-contains
                                   (string-downcase
                                    (srfi-author-name author))
                                   (string-downcase
                                    author-name-part)))
                                (srfi-authors srfi))
                          (error "Cannot find author."))))
             (author/co-author
              (if (= 1 (length (srfi-authors srfi)))
                  "author"
                  "co-author"))
             (pronoun "he")
             (date (date->iso-date
                    (julian-day->date
                     (+ 7 (date->julian-day (current-date)))))))
        (string-join-lines
         (map
          string-concatenate
          `(("Subject: Last call for comments on SRFI " ,num ": " ,title)
            ("")
            (,author ", " ,author/co-author " of [[" ,url "][SRFI")
            (,num "]]: " ,title ", has asked me to announce *last call*"
                  " for this SRFI.  " ,(cap-first pronoun))
            ("believes that it is ready for finalization,"
             " but would like to give")
            ("reviewers one last chance to submit corrections and"
             " feedback before we")
            ("finalize it.")
            ("")
            ("In particular, I appeal to anyone reading this to try"
             " the sample")
            ("implementation, run the tests, and send feedback about"
             " your results.")
            ("")
            ("If you're interested in this SRFI, please give your"
             " feedback via the")
            ("SRFI " ,num " mailing list before *" ,date "*.  After that,"
             " assuming that no")
            ("major revisions are required, we will declare it"
             " final.  It is")
            ("important that we get your feedback before " ,date
             ".  *If that deadline")
            ("is too soon for you, but you would like"
             " to contribute, please let me know")
            ("so that I can extend the last-call period.*")
            ("")
            ("Regards,")
            ("")
            ("")
            ("SRFI Editor"))))))

    (define-command (last-call num author-name-part)
      "Display email address URL for SRFI <num>."
      (write-string (srfi-last-call (parse-srfi-number num)
                                    author-name-part)))))
