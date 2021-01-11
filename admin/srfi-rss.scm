(import (chibi html-parser))
(import (chibi show))
(import (chibi show pretty))
(import (chibi sxml))
(import (srfi 1))
(import (srfi 19))
(import (srfi 132))

(define (pp object) (show #t (pretty object)))

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

(define (date->rss-time day month year)
  (date->string (make-date 0 0 0 12 day month year -480)
	       "~a, ~d ~b ~Y ~H:~M:~S -0800"))

(define (parse-iso-date string)
  (values (string->number (substring string 0 4))
	  (string->number (substring string 5 7))
	  (string->number (substring string 8 10))))

(define (iso-date->rss-time string)
  (call-with-values (lambda () (parse-iso-date string))
    (lambda (year month day)
      (date->rss-time day month year))))

(define (iso-date->date string)
  (call-with-values (lambda () (parse-iso-date string))
    (lambda (year month day)
      (make-date 0 0 0 12 day month year -480))))

(define (srfi-guid srfi)
  (let ((n (srfi/number srfi)))
    (show #f "https://srfi.schemers.org/srfi-" n)))

(define (srfi-url srfi)
  (let ((n (srfi/number srfi)))
    (show #f "https://srfi.schemers.org/srfi-" n "/srfi-" n ".html")))

(define (srfi-update-date srfi)
  (or (srfi/done-date srfi)
      (srfi/draft-date srfi)))

(define (srfi-abstract srfi)
  (let ((pathname (string-append (get-environment-variable "sc")
				 "/admin/abstracts/"
				 (number->string (srfi/number srfi))
				 ".html")))
    (cdr (call-with-input-file pathname html->sxml))))

(define (srfi-item srfi)
  (let ((n (srfi/number srfi)))
    (rss-item (srfi-guid srfi)
	      (show #f "SRFI " n ": " (srfi/title srfi))
	      (srfi-url srfi)
	      (format-srfi-authors (srfi/authors srfi))
	      (iso-date->rss-time (srfi-update-date srfi))
	      `("SRFI "
		,n
		" is now in "
		(em ,(srfi/status srfi))
		" status."
		(h2 "Abstract")
		(blockquote ,(srfi-abstract srfi))))))

(define (srfi-feed)
  (define (srfi-time srfi)
    (date->time-utc (iso-date->date (srfi-update-date srfi))))
  (let ((srfis (list-sort
		(lambda (s1 s2) (time>? (srfi-time s1) (srfi-time s2)))
		(read-srfi-data "srfi-data.scm"))))
    (rss-outline
     (rss-channel "Scheme Requests for Implementation"
		  "https://srfi.schemers.org/"
		  "Updates to SRFI documents"
		  "https://srfi.schemers.org/rss"
		  "en-US"
		  (map srfi-item srfis)))))

(define (main)
  (sxml-display-as-html (srfi-feed)))