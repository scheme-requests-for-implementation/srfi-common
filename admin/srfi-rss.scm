(import (chibi html-parser))
(import (chibi show))
(import (chibi show pretty))
(import (chibi sxml))
(import (srfi 1))
(import (srfi 19))

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

(define (srfi-abstract-html srfi)
  (let ((n (srfi/number srfi)))
    (call-with-input-file (show #f "abstracts/" n ".html")
      html->sxml)))

(define (srfi-guid srfi)
  (let ((n (srfi/number srfi)))
    (show #f "https://srfi.schemers.org/srfi-" n)))

(define (srfi-url srfi)
  (let ((n (srfi/number srfi)))
    (show #f "https://srfi.schemers.org/srfi-" n "/srfi-" n ".html")))

;; <> Add description of the change in state of the SRFI.

(define (srfi-item srfi)
  (let ((n (srfi/number srfi)))
    (rss-item (srfi-guid srfi)
	      (show #f "SRFI " n ": " (srfi/title srfi))
	      (srfi-url srfi)
	      (format-srfi-authors (srfi/authors srfi))
	      (iso-date->rss-time
	       (or (srfi/done-date srfi)
		   (srfi/draft-date srfi)))
	      (srfi-abstract-html srfi))))

;; <> Incorporate the output HTML from this procedure into the SRFI home page.
(define (header-rss-link url)
  `(a (@ (href ,url)
         (rel "alternate")
         (type "application/rss+xml"))
      (img (@ (alt "RSS")
	      (src "/blog/rss.svg")))))

;; <> Drop duplicates, e.g. remove draft when finalization or withdrawal
;; happens.

(define (srfi-feed)
  (rss-outline
   (rss-channel "Scheme Requests for Implementation"
		"https://srfi.schemers.org/"
		"Updates to SRFI documents"
		"https://srfi.schemers.org/rss"
		"en-US"
		(map srfi-item (read-srfi-data "srfi-data.scm")))))

(define (main)
  (sxml-display-as-html (srfi-feed)))