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

(define (mailto-address-subject address subject)
  (string-append "mailto:"
                 address
                 "?subject=" (url-hexify-string subject)))

(define (srfi-mailto-url num)
  (mailto-address-subject (srfi-mail-address num)
                          (srfi-title num)))

(define (srfi-last-call num author-name-part)
  (let* ((srfi (srfi-by-number num))
         (email (srfi-mail-address num))
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
                 (+ 7
                    (truncate  ; Work around bug in Chibi's SRFI 19.
                     (date->julian-day (current-date))))))))
    `((p ,author
	 ", "
	 ,author/co-author
	 " of "
	 (a (@ (href ,url)) "SRFI " ,num)
	 ": "
	 ,title
	 ", has asked me to announce "
	 (b "last call")
	 " for this SRFI.  "
	 ,(string-capitalize-first pronoun)
         " believes that it is ready for finalization, but would like "
	 "to give reviewers one last chance to submit corrections and "
	 "feedback before we finalize it.")
      (p
       "In particular, I appeal to anyone reading this to try the "
       "sample implementation, run the tests, and send feedback "
       "about your results.")
      (p "If you're interested in this SRFI, please give your "
	 "feedback via the SRFI "
	 ,num
	 " mailing list before "
	 (b ,date)
	 ".  After that, assuming that no major revisions are required, we "
	 "will declare it final.  It is important that we get your feedback "
	 "before "
	 ,date
         ".  "
	 (b "If that deadline is too soon for you, but you would like to "
	    "contribute, please let me know so that I can extend the last-"
	    "call period."))
      (p "Regards,")
      (p (@ (style "margin-top: 3em;")) "SRFI Editor"))))

;; TODO: `xclip-sxml' only works with X11.  On MacOS, make it run the equivalent
;; of the following two lines instead of running xclip, which isn't appropriate
;; there.
;;
;; HEX=`hexdump -ve '1/1 "%.2x"'`
;; osascript -e "set the clipboard to «data HTML${HEX}»"
(define (xclip-sxml sxml)
  (let ((temp-file (make-temp-file-name)))
    (call-with-output-file temp-file
      (lambda (port) (sxml-display-as-html sxml port #true)))
    (run-program
     `("xclip"
       "-selection" "clipboard"
       "-target" "text/html"
       ,temp-file))))

(define-command (last-call num author-name-part)
  "Put HTML of last-call message for SRFI <num> with author <author-name-part>
into clipboard, then open email client with new message addressed to mailing
list for SRFI <num>, ready for pasting the body."
  (let* ((n (parse-srfi-number num))
	 (subject
          (string-append
           "Last call for comments on SRFI "
	   num
	   ": "
	   (srfi-title n))))
    (browse-url (mailto-address-subject (srfi-mail-address n) subject))
    (xclip-sxml (srfi-last-call n author-name-part))))