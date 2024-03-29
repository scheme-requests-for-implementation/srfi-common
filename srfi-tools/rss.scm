(define (rss-outline channels)
  `(rss (@ (version "2.0")
           (xmlns:atom "http://www.w3.org/2005/Atom")
           (xmlns:dc "http://purl.org/dc/elements/1.1/"))
        ,channels))

(define (rss-channel title
                     link
                     description
                     feed-url
                     language
                     items)
  `(channel (title ,title)
            (link ,link)
            (description ,description)
            (language ,language)
            (webMaster "srfi@speechcode.com (Arthur A. Gleckler)")
            (atom:link (@ (href ,feed-url)
                          (rel "self")
                          (type "application/rss+xml")))
            ,@items))

(define (rss-item guid title link creator date html-summary)
  `(item (title ,title)
         (link ,link)
         (guid ,guid)
         (description ,html-summary)
         (dc:creator ,creator)
         (pubDate ,date)))

(define (iso-date->rss-time string)
  (date->string (iso-date->date string)
                "~a, ~d ~b ~Y ~H:~M:~S -0800"))

(define (srfi-guid srfi)
  (let ((n (srfi-number srfi)))
    (format "https://srfi.schemers.org/srfi-~a" n)))

(define (srfi-item srfi)
  (let ((n (srfi-number srfi)))
    (rss-item (srfi-guid srfi)
              (format "SRFI ~a: ~a" n (srfi-title srfi))
              (srfi-landing-url (srfi-number srfi))
              (srfi-format-authors (srfi-authors srfi))
              (iso-date->rss-time (srfi-date-of-last-update srfi))
              (sxml->xml `("SRFI "
			  ,n
			  " is now in "
			  (em ,(srfi-status srfi))
			  " status."
			  (blockquote ,@(srfi-abstract-sxml n)))))))

(define (srfi-rss-sxml)
  (define (srfi-time srfi)
    (date->time-utc (iso-date->date (srfi-date-of-last-update srfi))))
  (let ((srfis (list-sort
                (lambda (s1 s2)
                  (time<? (srfi-time s2)
                          (srfi-time s1)))
                (srfi-list))))
    (rss-outline
     (rss-channel "Scheme Requests for Implementation"
                  "https://srfi.schemers.org/"
                  "Updates to SRFI documents"
                  "https://srfi.schemers.org/srfi.rss"
                  "en-US"
                  (map srfi-item srfis)))))

(define (srfi-rss-html)
  (with-output-to-string
    (lambda () (sxml-display-as-html (srfi-rss-sxml)))))

(define-command (generate-rss)
  "Generate the RSS file of SRFI updates."
  (let ((sxml (srfi-rss-sxml)))
    (disp "Writing " (srfi-rss-file))
    (write-text-file (srfi-rss-file) (srfi-rss-html))))
