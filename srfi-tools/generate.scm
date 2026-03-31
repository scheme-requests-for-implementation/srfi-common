;;;; Generate "index.html" pages, etc. for SRFIs.

(define srfi-editor "Arthur A. Gleckler")

(define dark-light-script
  (read-text-file (path-append (srfi-common-dir) "dark-light.js")))

(define (admin-page-head title . extra)
  `(head
    (meta (@ (charset "utf-8")))
    (title ,title)
    (link (@ (href "favicon.png")
	     (rel "icon")
	     (sizes "192x192")
	     (type "image/png")))
    (link (@ (rel "stylesheet") (type "text/css") (href "admin.css")))
    (script (@raw ,dark-light-script))
    ,@extra
    (meta (@ (name "viewport")
	     (content "width=device-width, initial-scale=1")))))

(define (faq-anchor-template description name)
  `(a (@ (href ,(concat "/srfi-faq.html#" name))) ,description))

(define (home-template keyword-options srfi-list)
  `(*TOP*
    (!DOCTYPE html)
    (html (@ (lang "en"))
	  (head
	   (meta (@ (charset "utf-8")))
	   (script (@raw ,dark-light-script))
	   (title "Scheme Requests for Implementation")
	   (link (@ (href "favicon.png")
		    (rel "icon")
		    (sizes "192x192")
		    (type "image/png")))
	   (link (@ (rel "stylesheet") (type "text/css") (href "admin.css")))
	   (link (@ (rel "stylesheet") (type "text/css") (href "home.css")))
	   (link (@ (rel "stylesheet") (type "text/css") (href "list.css")))
	   (link (@ (rel "alternate")
		    (type "application/rss+xml")
		    (href "/srfi.rss")))
	   (meta (@ (name "viewport")
		    (content "width=device-width, initial-scale=1")))
	   (script (@ (type "text/x-mathjax-config"))
		   "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]}});")
	   (script (@ (crossorigin "anonymous")
		      (integrity "sha384-Ra6zh6uYMmH5ydwCqqMoykyf1T/+ZcnOQfFPhDrp2kI4OIxadnhsvvA2vv9A7xYv")
		      (src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML"))))
	  (body
	   (h1 (img (@ (class "srfi-logo")
		       (src "srfi-logo.svg")
		       (alt "SRFI surfboard logo")))
	       "Scheme Requests for Implementation"
	       (span (@ (class "rss-logo"))
		     (a (@ (href "https://srfi.schemers.org/srfi.rss"))
			(img (@ (alt "RSS") (src "https://srfi.schemers.org/rss.svg"))))))
	   (p (@ (class "hide"))
	      "SRFIs extend the Scheme programming language.  You can help.  Start by "
	      (a (@ (href "srfi-list-subscribe.html")) "joining the discussion")
	      " on one of our mailing lists.  "
	      (a (@ (href "about.html")) "Learn more")
	      ".")
	   (p "Arvydas Silanskas's "
	      (a (@ (href "https://index.scheme.org/")) "index.scheme.org")
	      " is an increasingly comprehensive index of Scheme implementations, standards, and SRFIs.")
	   (p "Shiro Kawai's "
	      (a (@ (href "https://practical-scheme.net/")) "Practical Scheme")
	      " includes a "
	      (a (@ (href "https://practical-scheme.net/wiliki/schemexref.cgi?SRFI"))
		 "cross-reference")
	      " showing which Scheme implementations support which SRFIs.  It's a wiki page, so please help keep it up to date.")
	   (h2 "The SRFIs")
	   (noscript (p "Javascript is not enabled in your browser, so filtering, searching, and sorting the list of SRFIs is turned off."))
	   (div (@ (id "srfis"))
		(div (@ (class "invisible") (id "parameters"))
		     (div (span "Search for")
			  (input (@ (class "search")
				    (id "search")
				    (placeholder "numbers or words")
				    (tabindex "1"))))
		     (div (span "Filter by ")
			  (div (@ (class "dropdown") (id "keywords"))
			       (button (@ (class "controls"))
				       "keywords"
				       (span " ×"))
			       (div (@ (class "options"))
				    (select (@ (multiple)
					       (size
						,(+ 1
						    (length keyword-options))))
					    (option (@ (selected) (value "any"))
						    "any")
					    ,keyword-options))
			       (span (@ (class "chosen"))))
			  (div (@ (class "dropdown") (id "statuses"))
			       (button (@ (class "controls"))
				       "status"
				       (span " ×"))
			       (div (@ (class "options"))
				    (select (@ (multiple) (size "4"))
					    (option (@ (selected) (value "any"))
						    "any")
					    (option (@ (value "draft")) "Draft")
					    (option (@ (value "final")) "Final")
					    (option (@ (value "withdrawn"))
						    "Withdrawn")))
			       (span (@ (class "chosen")))))
		     (div (span "Show")
			  (label (@ (class "checkbox-label controls"))
				 (input (@ (class "controls")
					   (id "abstracts-control")
					   (tabindex "7")
					   (type "checkbox")))
				 "abstracts"))
		     (div (span "Sort by")
			  (button (@ (class "controls sort")
				     (data-sort "authors")
				     (tabindex "2"))
				  "authors")
			  (button (@ (class "controls sort")
				     (data-sort "date")
				     (tabindex "3"))
				  "date")
			  (button (@ (class "controls sort")
				     (data-sort "name")
				     (tabindex "4"))
				  "name")
			  (button (@ (class "controls sort")
				     (data-sort "number")
				     (tabindex "5"))
				  "number")
			  (button (@ (class "controls sort")
				     (data-sort "status")
				     (tabindex "6"))
				  "status")))
		(ul (@ (class "summary list"))
		    ,srfi-list))
	   (hr)
	   (p "Here is "
	      (a (@ (href "srfi-privacy.html")) "our privacy statement")
	      ".")
	   (p "If you have any general questions about this site, please contact "
	      (a (@ (href "mailto:srfi-editors%20at%20srfi%20dot%20schemers%20dot%20org"))
		 "the SRFI editors")
	      ".")
	   (script (@ (src "srfi.js")))))))

(define (spdx-license-and-copyright authors)
  `((*COMMENT* "SPDX-FileCopyrightText: "
	       ,(number->string (date-year (current-date)))
	       " "
	       ,authors)		; Don't include email addresses.
    (*COMMENT* "SPDX-License-Identifier: MIT")))

(define (index-template abstract
			authors
			based-on
			date
			mail-archive
			keyword-links
			library-name
			number
			see-also
			status
			title)
  `(*TOP*
    (!DOCTYPE html)
    (html
     (head ,@(spdx-license-and-copyright srfi-editor)
	   (title  ,title)
	   (script (@raw ,dark-light-script))
	   (link (@ (href "/admin.css") (rel "stylesheet")))
	   (link (@ (href "/list.css") (rel "stylesheet")))
	   (link (@ (href "/favicon.png")
		    (rel "icon")
		    (sizes "192x192")
		    (type "image/png")))
	   (meta (@ (charset "utf-8")))
	   (meta (@ (name "viewport")
		    (content "width=device-width, initial-scale=1")))
	   (script (@ (type "text/x-mathjax-config"))
		   "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]}});")
	   (script (@ (crossorigin "anonymous")
		      (integrity "sha384-Ra6zh6uYMmH5ydwCqqMoykyf1T/+ZcnOQfFPhDrp2kI4OIxadnhsvvA2vv9A7xYv")
		      (src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
		      (type "text/javascript"))))
     (body (h1 "SRFI " ,number ": " ,title)
	   (p (@ (class "authors")) "by " ,authors)
	   (p (@ (class "based-on")) ,based-on)
	   (p (@ (class "status")) "status: " (em ,status) " (" ,date ")")
	   (p (@ (class "keywords"))
	      "keywords: "
	      ,keyword-links)
	   ,see-also
	   ,library-name
	   (ul (@ (class "info"))
	       (li (a (@ (href ,(concat "srfi-" number ".html")))
		      "The SRFI Document"))
	       ,mail-archive
	       (li (a (@ (href ,(concat "https://github.com/scheme-requests-for-implementation/srfi-"
					number)))
		      "Git repo (on GitHub)"))
	       (li (a (@ (href ,(concat "mailto:srfi-"
					number
					"@srfi.schemers.org")))
		      "srfi-"
		      ,number
		      "@"
		      (span (@ (class "antispam")) "nospam")
		      "srfi.schemers.org (subscribers only)"))
	       (li (span (@ (class "firefox-column-workaround"))
			 (form (@ (method "POST")
				  (action
				   "https://www.simplelists.com/subscribe.php"))
			       (div (@ (class "title"))
				    "Subscribe to srfi-"
				    ,number
				    " mailing list")
			       (input (@ (name "email")
					 (placeholder "email address")
					 (tabindex "1")
					 (type "email")))
			       (input (@ (name "name")
					 (placeholder "full name")
					 (tabindex "2")
					 (type "text")))
			       (p (input (@ (id "sub-digest")
					    (name "digest")
					    (tabindex "3")
					    (type "checkbox")
					    (value "digest")))
				  (label (@ (for "sub-digest"))
					 "daily digest?"))
			       (input (@ (class "submit")
					 (name "submit")
					 (tabindex "4")
					 (type "submit")
					 (value ,(concat "Subscribe to srfi-"
							 number))))
			       (input (@ (type "hidden")
					 (name "action")
					 (value "subscribe")))
			       (input (@ (type "hidden")
					 (name "list")
					 (value
					  ,(concat "srfi-"
						   number
						   "@srfi.schemers.org")))))))
	       (li (span (@ (class "firefox-column-workaround"))
			 (form (@ (method "POST")
				  (action "https://www.simplelists.com/subscribe.php"))
			       (div (@ (class "title"))
				    "Unsubscribe from srfi-"
				    ,number
				    " mailing list")
			       (input (@ (name "email")
					 (placeholder "email address")
					 (tabindex "5")
					 (type "email")))
			       (input (@ (class "submit")
					 (name "submit")
					 (tabindex "6")
					 (type "submit")
					 (value
					  ,(concat "Unsubscribe from srfi-"
						   number))))
			       (input (@ (type "hidden")
					 (name "action")
					 (value "unsubscribe")))
			       (input (@ (name "list")
					 (type "hidden")
					 (value
					  ,(concat "srfi-"
						   number
						   "@srfi.schemers.org"))))))))
	   (h2 "Abstract")
	   ,abstract))))

(define (readme-template authors
			 based-on
			 keyword-names
			 library-name
			 number
			 see-also
			 status
			 title)
  `(*TOP*
    ,@(spdx-license-and-copyright srfi-editor)
    (h1 "SRFI " ,number ": " ,title)
    (h2 "by " ,authors)
    (p ,based-on)
    (p "keywords: " ,keyword-names)
    (p ,library-name
       "This repository hosts "
       (a (@ (href ,(concat "https://srfi.schemers.org/srfi-"
			    number
			    "/")))
	  "SRFI "
	  ,number)
       ": "
       ,title
       ", a "
       (a (@ (href "https://srfi.schemers.org/"))
	  "Scheme Request for Implementation")
       ".")
    (p "This SRFI is in "
       (em ,status)
       " status.")
    ,see-also
    (p "The full documentation for this SRFI can be found in the "
       (a (@ (href ,(concat "https://srfi.schemers.org/srfi-"
			    number
			    "/srfi-"
			    number
			    ".html")))
	  "SRFI Document")
       ".")
    (p "If you'd like to participate in the discussion of this SRFI, or report issues with it, please "
       (a (@ (href ,(concat "https://srfi.schemers.org/srfi-" number "/")
		   "/"))
	  "join the SRFI-"
	  ,number
	  " mailing list")
       " and send your message there.")
    (p "Thank you.")
    (p (a (@ (href "mailto:srfi-editors@srfi.schemers.org"))
	  "The SRFI Editors"))))

(define (srfi-list-subscribe-template schemecomm
				      schemedoc
				      schemeorg
				      schemepersist
				      schemeregistry
				      schemetest
				      schemeweb
				      srfi-announce
				      srfi-auto-subscribe
				      srfi-discuss)
  `(*TOP*
    (!DOCTYPE html)
    (html (@ (lang "en"))
	  (head (meta (@ (charset "utf-8")))
		(script (@raw ,dark-light-script))
		(title "SRFI Mailing Lists")
		(link (@ (href "/favicon.png")
			 (rel "icon")
			 (sizes "192x192")
			 (type "image/png")))
		(link (@ (href "admin.css")
			 (rel "stylesheet")
			 (type "text/css")))
		(link (@ (href "list.css")
			 (rel "stylesheet")
			 (type "text/css")))
		(meta (@ (name "viewport")
			 (content "width=device-width, initial-scale=1"))))
	  (body
	   (h1 (a (@ (href "/"))
		  (img (@ (class "srfi-logo")
			  (src "srfi-logo.svg")
			  (alt "SRFI logo"))))
	       "SRFI Mailing Lists")
	   (p "There are many SRFI mailing lists, including one for "
	      (a (@ (href "#srfi-announce")) "announcements")
	      ", one for "
	      (a (@ (href "#srfi-discuss")) "general discussion")
	      " of the SRFI process, one for "
	      (a (@ (href "#srfi-n")) "each SRFI")
	      ", one to "
	      (a (@ (href "#srfi-auto-subscribe"))
		 "subscribe to all")
	      " of the individual SRFI discussions, and several for discussing "
	      (a (@ (href "#scheme-topics"))
		 "Scheme topics")
	      " not directly related to SRFIs"
	      ".  Anyone can subscribe to any mailing list.  Only subscribers can post.  Spammers will be ejected.")
	   (h2 "SRFI-specific Lists")
	   (ul (@ (class "posts wide"))
	       (li (@ (class "card maillist"))
		   (span (@ (class "firefox-column-workaround"))
			 (h3 (@ (id "srfi-n"))
			     (code "srfi-" (em "n")
				   (wbr)
				   "@"
				   (span (@ (class "antispam")) "nospam")
				   "srfi.schemers.org"))
			 (p "There is a separate, unmoderated mailing list for discussion of each SRFI.  All discussion of specific SRFI proposals should take place on these lists, as not all interested parties participate in other venues such as "
			    (a (@ (href "https://www.reddit.com/r/scheme/"))
			       "r/scheme")
			    " on Reddit.")
			 (p "To subscribe to the mailing list for a particular SRFI, visit that SRFI's page, e.g. "
			    (a (@ (href "http://srfi.schemers.org/srfi-0/"))
			       "the one for SRFI 0")
			    ", and fill out the web form there.  There is a form there for unsubscribing, too."))))
	   (h2 "General SRFI Topics")
	   (ul (@ (class "posts"))
	       ,srfi-announce
	       ,srfi-discuss
	       ,srfi-auto-subscribe)
	   (h2 (@ (id "scheme-topics")) "Scheme Topics")
	   (p "Scheme Topics mailing lists are for discussing specific subjects that we hope may lead to SRFIs and other cooperative work, but which are currently not ready for specific proposals through the SRFI process.  As with all these mailing lists, anyone can subscribe.  None of these mailing lists is included in the "
	      (a (@ (href "#srfi-auto-subscribe"))
		 (code "srfi-auto-subscribe"))
	      " mailing list, which includes "
	      (code "srfi-announce")
	      " and all the numbered SRFI mailing lists, but not "
	      (code "srfi-discuss")
	      ".")
	   (ul (@ (class "posts"))
	       ,schemecomm
	       ,schemedoc
	       ,schemeorg
	       ,schemepersist
	       ,schemeregistry
	       ,schemetest
	       ,schemeweb)
	   (hr)
	   (address (a (@ (href "mailto:srfi-editors@srfi.schemers.org"))
		       "The SRFI Editors"))))))

(define (srfi-list-template name description)
  `(li (@ (class "card maillist"))
       (span (@ (class "firefox-column-workaround"))
	     (h3 (@ (id ,name))
		 (a (@ (href ,(concat "https://srfi-email.schemers.org/"
				      name)))
		    (code ,name
			  (wbr)
			  "@"
			  (span (@ (class "antispam")) "nospam")
			  "srfi.schemers.org")))
	     (p ,description)
	     (div (@ (class "subunsub"))
		  (details (@ (name ,(concat name "-subunsub")))
		    (summary "Subscribe")
		    (form
		     (@ (class "sub")
			(method "POST")
			(action "https://www.simplelists.com/subscribe.php"))
		     (p (input (@ (name "email")
				  (placeholder "email address")
				  (type "email"))))
		     (p (input (@ (name "name")
				  (placeholder "full name")
				  (type "text"))))
		     (p (input (@ (id ,(concat "sub-" name "-digest"))
				  (name "digest")
				  (type "checkbox")
				  (value "digest")))
			(label (@ (for ,(concat "sub-" name "-digest")))
			       "daily digest?"))
		     (input (@ (class "submit")
			       (name "submit")
			       (type "submit")
			       (value ,(concat "Subscribe to " name))))
		     (input (@ (name "action")
			       (type "hidden")
			       (value "subscribe")))
		     (input (@ (name "list")
			       (type "hidden")
			       (value ,(concat name
					       "@srfi.schemers.org"))))))
		  (details (@ (name ,(concat name "-subunsub")))
		    (summary "Unsubscribe")
		    (form (@ (class "unsub")
			     (method "POST")
			     (action
			      "https://www.simplelists.com/subscribe.php"))
			  (p (input (@ (name "email")
				       (placeholder "email address")
				       (type "email"))))
			  (input (@ (class "submit")
				    (name "submit")
				    (type "submit")
				    (value ,(concat "Unsubscribe from "
						   name))))
			  (input (@ (type "hidden")
				    (name "action")
				    (value "unsubscribe")))
			  (input (@ (type "hidden")
				    (name "list")
				    (value ,(concat name
						   "@srfi.schemers.org"))))))))))

(define (keyword->link keyword)
  `(a (@ (href ,(concat "https://srfi.schemers.org/?keywords=" keyword)))
      ,(keyword->name keyword)))

;; This generates the "index.html" that is used as a landing page for an
;; individual SRFI.
(define (write-single-srfi-landing-page srfi)
  (let* ((number (srfi-number srfi))
	 (abstract (srfi-abstract-raw number))
	 (archive
	  `(li (a (@ (href ,(concat "https://srfi-email.schemers.org/srfi-"
				    number
				    "/")))
		  "Discussion Archive")))
	 (date (srfi-date-to-show srfi))
	 (keywords (srfi-keywords srfi))
	 (library-name-block
	  (cond ((srfi-library-name srfi)
		 => (lambda (name)
		      `(p (@ (class "library-name")) "library name: " ,name)))
		(else "")))
	 (status (srfi-status srfi))
	 (title (srfi-title srfi))
	 (authors (srfi-authors srfi)))
    (write-html-file (path-append (srfi-dir number) "index.html")
      (index-template
       abstract
       (srfi-format-authors authors)
       (or (srfi-based-on srfi) "")
       date
       archive
       (comma-list (map keyword->link keywords))
       library-name-block
       number
       (see-also srfi)
       status
       title))))

(define (write-single-srfi-readme srfi)
  (let* ((based-on (or (srfi-based-on srfi) ""))
	 (keywords (srfi-keywords srfi))
	 (library-name-block
	  (cond ((srfi-library-name srfi)
		 => (lambda (name) (concat "library-name: " name "\n\n")))
		(else "")))
	 (number (srfi-number srfi))
	 (status (srfi-status srfi))
	 (title (srfi-title srfi))
	 (authors (srfi-authors srfi)))
    (write-org-file (path-append (srfi-dir number) "README.org")
      (readme-template
       (srfi-format-authors authors)
       based-on
       (comma-list (map keyword->link keywords))
       library-name-block
       number
       (see-also srfi)
       status
       title))))

(define (write-srfi-landing-pages)
  (for-each write-single-srfi-landing-page (srfi-list)))

(define (write-srfi-readmes)
  (for-each write-single-srfi-readme (srfi-list)))

(define (srfi-anchor-template srfi)
  (let ((number (srfi-number srfi))
	(title (srfi-title srfi)))
    `(a (@ (href ,(concat "/srfi-" number "/")))
	"SRFI "
	,number
	": "
	,title)))

(define (see-also srfi)
  (let ((others (srfi-see-also srfi)))
    (if (null? others)
	""
	`(span (@ (class "see-also"))
	       "See also "
	       ,(english-list
		 (map (lambda (n)
			(srfi-anchor-template (srfi-by-number n)))
		      (list-sort < others)))
	       "."))))

(define (srfi-card-template srfi)
  (let* ((number (srfi-number srfi))
	 (abstract (srfi-abstract-raw number))
	 (authors (srfi-format-authors (srfi-authors srfi)))
	 (based-on (or (srfi-based-on srfi) ""))
	 (date (srfi-date-to-show srfi))
	 (status (srfi-status srfi))
	 (date-type (srfi-status->name status))
	 (keywords (srfi-keywords srfi))
	 (keyword-links (comma-list (map keyword->link keywords)))
	 (keyword-values (string-join (map symbol->string keywords) ","))
	 (name (srfi-title srfi))
	 (see-also (see-also srfi))
	 (library-name
	  (cond ((srfi-library-name srfi)
		 => (lambda (name)
		      `(span (@ (class "library-name"))
			     "Library name: "
			     ,(symbol->string name))))
		(else ""))))
    `(li (@ (class ,(concat "card " status)))
	 (a (@ (class "card-anchor") (href ,(concat "srfi-" number "/"))))
	 (a (@ (href ,(concat "srfi-" number "/")))
	    (span (@ (class "number")) ,number))
	 ": "
	 (span (@ (class "name")) ,name)
	 (span (@ (class "authors")) ", by " ,authors)
	 (span (@ (class "based-on")) ,based-on)
	 (span (@ (class "date-group"))
	       ,date-type
	       ": "
	       (span (@ (class "date")) ,date))
	 (span (@ (class "keywords") (data-keywords ,keyword-values))
	       "Keywords: "
	       ,keyword-links)
	 ,library-name
	 (span (@ (class "status") (data-status ,status)))
	 ,see-also
	 (div (@ (class "abstract")) ,abstract))))

(define (keyword-options)
  (map (lambda (k) `(option (@ (value ,(car k))) ,(cadr k)))
       srfi-keyword-alist))

;; This generates the "index.html" that is used as the SRFI home page.
(define (write-srfi-home-page)
  (write-html-file (path-append (srfi-common-dir) "index.html")
    (home-template (keyword-options)
		   (map srfi-card-template (reverse (srfi-list))))))

(define schemecomm
  (srfi-list-template
   "schemecomm"
   "This is an unmoderated discussion of support for all kinds of machine-to-machine communications in Scheme, e.g. UDP/TCP/web sockets, distributed hash tables, and radio."))

(define schemedoc
  (srfi-list-template
   "schemedoc"
   '("This is an unmoderated discussion of collecting, organizing, and serving indexes of Scheme code found in SRFIs, Scheme implementations, R"
     (sup "n")
     "RS standards, etc.")))

(define schemeorg
  (srfi-list-template
   "schemeorg"
   '("This is an unmoderated discussion of the "
     (code "scheme.org")
     " domain and its subdomains and how they will be organized.")))

(define schemepersist
  (srfi-list-template
   "schemepersist"
   "This is an unmoderated discussion of persistent storage and serialization for Scheme implementations, including existing, new, and improved persistence libraries; interface protocols to support; SRFIs to write; etc."))

(define schemeregistry
  (srfi-list-template
   "schemeregistry"
   "This is an unmoderated discussion of the Scheme Registry, a database of defined symbols, identifiers and other pieces of data used by Scheme standards and implementations."))

(define schemetest
  (srfi-list-template
   "schemetest"
   "This is an unmoderated discussion of anything and everything having to do with testing in the world of Scheme."))

(define schemeweb
  (srfi-list-template
   "schemeweb"
   "This is an unmoderated discussion of web technology implemented in Scheme, e.g. web servers and web request handlers."))

(define srfi-announce
  (srfi-list-template
   "srfi-announce"
   "This is a moderated mailing list where all announcements about new SRFI proposals and changes in status are posted."))

(define srfi-auto-subscribe
  (srfi-list-template
   "srfi-auto-subscribe"
   '("Subscribe to this list if you'd like to receive messages from "
     (a (@ (href "#srfi-announce"))
	(code "srfi-announce"))
     " and all "
     (a (@ (href "#srfi-n"))
	"srfi-"
	(em "n"))
     " discussion mailing lists.  Note that this list does "
     (em "not")
     " deliver messages from "
     (a (@ (href "#srfi-discuss"))
	(code "srfi-discuss"))
     ", "
     (a (@ (href "#schemedoc"))
	(code "schemedoc"))
     ", "
     (a (@ (href "#schemeorg"))
	(code "schemeorg"))
     ", "
     (a (@ (href "#schemepersist"))
	(code "schemepersist"))
     ", "
     (a (@ (href "#schemeregistry"))
	(code "schemeregistry"))
     ", "
     (a (@ (href "#schemetest"))
	(code "schemetest"))
     ", or "
     (a (@ (href "#schemeweb"))
	(code "schemeweb"))
     ".  You can subscribe to them independently.")))

(define srfi-discuss
  (srfi-list-template
   "srfi-discuss"
   '("This is an unmoderated mailing list for general discussion of the SRFI process.  It is archived "
     (a (@ (href "https://srfi-email.schemers.org/srfi-discuss"))
	"here")
     ".")))

(define (about-template)
  `(*TOP*
    (!DOCTYPE html)
    (html (@ (lang "en"))
	  (head
	   (meta (@ (charset "utf-8")))
	   (title "About Scheme Requests for Implementation")
	   (link (@ (href "favicon.png")
		    (rel "icon")
		    (sizes "192x192")
		    (type "image/png")))
	   (link (@ (rel "stylesheet") (type "text/css") (href "admin.css")))
	   (script (@raw ,dark-light-script))
	   (meta (@ (name "viewport")
		    (content "width=device-width, initial-scale=1"))))
	  (body
	   (h1 (a (@ (href "/"))
		  (img (@ (class "srfi-logo")
			  (src "srfi-logo.svg")
			  (alt "SRFI logo"))))
	       "About Scheme Requests for Implementation")

	   (figure (@ (id "cumulative-srfis"))
		   (span (a (@ (href "#")) "×"))
		   (a (@ (href "#cumulative-srfis"))
		      (img (@ (alt "total SRFIs over time")
			      (src "srfi.svg"))))
		   (figcaption "total SRFIs"))

	   (p (a (@ (href "srfi-history.html")) "Since 1998")
	      ", the Scheme Requests for Implementation (SRFI) process has been helping Scheme users write portable, useful Scheme code.  We write concrete, detailed proposals and sample implementations for libraries and other additions to the Scheme language, and we encourage Scheme implementors to adopt them.")

	   (p "If you're interested in reading existing proposals, writing a new one ("
	      (a (@ (href "srfi-template.html")) "template")
	      "), providing feedback on a draft proposal, helping with a sample implementation, or reporting a bug, please read about "
	      (a (@ (href "srfi-process.html")) "our process")
	      ", skim our "
	      (a (@ (href "srfi-faq.html")) "FAQ")
	      ", and subscribe to some of our "
	      (a (@ (href "srfi-list-subscribe.html")) "mailing lists")
	      ", including "
	      (a (@ (href "https://srfi-email.schemers.org/srfi-announce")) "srfi-announce")
	      " and "
	      (a (@ (href "https://srfi-email.schemers.org/srfi-discuss")) "srfi-discuss")
	      ".")

	   (p "In addition to mailing lists about SRFIs, we host the Scheme Topics mailing lists, which are for discussing specific subjects that we hope may lead to SRFIs and other cooperative work, but which are currently not ready for specific proposals through the SRFI process.  So far, we have these lists: "
	      (a (@ (href "https://srfi-email.schemers.org/schemecomm")) (code "schemecomm"))
	      ", "
	      (a (@ (href "https://srfi-email.schemers.org/schemedoc")) (code "schemedoc"))
	      ", "
	      (a (@ (href "https://srfi-email.schemers.org/schemeorg")) (code "schemeorg"))
	      ", "
	      (a (@ (href "https://srfi-email.schemers.org/schemepersist")) (code "schemepersist"))
	      ", "
	      (a (@ (href "https://srfi-email.schemers.org/schemeregistry")) (code "schemeregistry"))
	      ", "
	      (a (@ (href "https://srfi-email.schemers.org/schemetest")) (code "schemetest"))
	      ", and "
	      (a (@ (href "https://srfi-email.schemers.org/schemeweb")) (code "schemeweb"))
	      ".")

	   (p "Everyone is welcome to join our discussions.  Please "
	      (a (@ (href "srfi-list-subscribe.html")) "subscribe here")
	      ".  (You must subscribe in order to post messages.)")

	   (p "Every SRFI is hosted on Github, but if you'd like a complete archive of all SRFI documents and code, please download "
	      (a (@ (href "srfi.tgz")) (code "srfi.tgz"))
	      ".")

	   (h2 "History")

	   (p "The SRFI process  has been important to the development of some of the official Scheme standards, including "
	      (a (@ (href "http://www.r6rs.org/")) "R" (sup "6") "RS")
	      " and "
	      (a (@ (href "http://r7rs.org")) "R" (sup "7") "RS")
	      ".  It has been running "
	      (a (@ (href "srfi-history.html")) "since 1998")
	      ", and there have been many "
	      (a (@ (href "srfi-editors.html")) "different editors")
	      ".")

	   (p "At the "
	      (a (@ (href "http://www.schemeworkshop.org/2018/")) "2018 Scheme Workshop")
	      ", SRFI editor Arthur A. Gleckler gave a talk entitled Growing Schemes: Twenty Years of Scheme Requests for Implementation ("
	      (a (@ (href "https://www.youtube.com/watch?v=YuqZKHwUygU")) "YouTube")
	      ", "
	      (a (@ (href "https://www.brinckerhoff.org/scheme2018/papers/Gleckler.pdf")) "PDF")
	      ") on SRFI, its history, and future plans.  At the "
	      (a (@ (href "https://icfp22.sigplan.org/home/scheme-2022#program")) "2022 Scheme Workshop")
	      ", he gave a follow-up talk entitled Scheme Requests for Implementation Status Report ("
	      (a (@ (href "https://www.youtube.com/watch?v=xzZfdPtHvOk")) "YouTube")
	      ").")

	   (hr)

	   (p "Here is " (a (@ (href "srfi-privacy.html")) "our privacy statement") ".")

	   (p "If you have any general questions about this site, please contact "
	      (a (@ (href "mailto:srfi-editors%20at%20srfi%20dot%20schemers%20dot%20org")) "the SRFI editors")
	      ".")))))

(define (write-srfi-about-page)
  (write-html-file (path-append (srfi-common-dir) "about.html")
    (about-template)))

(define (editors-template)
  `(*TOP*
    (!DOCTYPE html)
    (html (@ (lang "en"))
	  ,(admin-page-head "SRFI Editors")
	  (body
	   (h1 (a (@ (href "/"))
		  (img (@ (class "srfi-logo")
			  (src "srfi-logo.svg")
			  (alt "SRFI surfboard logo"))))
	       "SRFI Editors")
	   (h2 "Present")
	   (dl
	    (dt (a (@ (href "http://www.speechcode.com/")) "Arthur A. Gleckler"))
	    (dd "2015-present"))
	   (h2 "Past")
	   (dl
	    (dt (a (@ (href "http://www.cs.brown.edu/~sk/")) "Shriram Krishnamurthi"))
	    (dd "1998-2002")
	    (dt (a (@ (href "http://www.sarg.ryerson.ca/dmason/")) "Dave Mason"))
	    (dd "1998-2002")
	    (dt (a (@ (href "http://www.deinprogramm.de/sperber/")) "Mike Sperber"))
	    (dd "1998-2015")
	    (dt (a (@ (href "http://people.netscape.com/drush/")) "David Rush"))
	    (dd "2002-2005")
	    (dt (a (@ (href "http://biblio06.fciencias.unam.mx/solsona/")) "Francisco Solsona"))
	    (dd "2002-2015")
	    (dt (a (@ (href "http://www.ccs.neu.edu/home/dvanhorn/")) "David Van Horn"))
	    (dd "2004-2015")
	    (dt (a (@ (href "http://www.rscheme.org/~donovan/")) "Donovan Kolbly"))
	    (dd "2005-2015"))
	   (p "The history of this document is "
	      (a (@ (href "https://github.com/scheme-requests-for-implementation/srfi-common/commits/master/srfi-editors.html")) "here")
	      ".")))))

(define (write-srfi-editors-page)
  (write-html-file (path-append (srfi-common-dir) "srfi-editors.html")
    (editors-template)))

(define (history-template)
  `(*TOP*
    (!DOCTYPE html)
    (html (@ (lang "en"))
	  ,(admin-page-head "History of Scheme Requests for Implementation")
	  (body
	   (h1 (a (@ (href "/"))
		  (img (@ (class "srfi-logo")
			  (src "srfi-logo.svg")
			  (alt "SRFI surfboard logo"))))
	       "History of Scheme Requests for Implementation")
	   (p "At the "
	      (a (@ (href "https://web.archive.org/web/19990428131650/http://www.neci.nj.nec.com/homepages/kelsey/workshop.html"))
		 "Scheme Workshop")
	      " held in Baltimore, Maryland, on September 26, 1998, the attendees considered a number of proposals for standardized feature sets for inclusion in Scheme implementations.  (Here are the "
	      (a (@ (href "history/minutes.txt")) "minutes")
	      ".)")
	   (p "Alan Bawden proposed that there be a "
	      (a (@ (href "history/bawden.txt")) "repository for library proposals")
	      ".  "
	      (a (@ (href "http://www.cs.brown.edu/~sk/")) "Shriram Krishnamurthi")
	      " volunteered to host the library, and "
	      (a (@ (href "http://www.sarg.ryerson.ca/dmason/")) "Dave Mason")
	      " and "
	      (a (@ (href "http://www-pu.informatik.uni-tuebingen.de/users/sperber/")) "Mike Sperber")
	      " joined him as initial editors and coordinators of the library process.  The term \"Requests for Implementation,\" an allusion to the Internet \"Requests for Comments,\" was coined at the workshop, and expanded into \"Scheme Requests for Implementation\" by the editors.")
	   (p "On November 1, 1998, the " (code "srfi minus discuss") " mailing list was established which had as subscribers many major implementors of Scheme as well as other contributors to the language.  An archive of the discussion is split between "
	      (a (@ (href "srfi-discuss-1998/maillist.html")) "1998-1999")
	      " and "
	      (a (@ (href "https://srfi-email.schemers.org/srfi-discuss/")) "2002-present")
	      ".")
	   (p "The SRFI web site along with the other SRFI procedures was established in late December 1998.")
	   (p "On January 27, 2002, Dave Mason and Shriram Krishnamurthi "
	      (a (@ (href "http://groups.google.com/groups?selm=w7dn0yzrs3j.fsf%40cs.brown.edu&output=gplain")) "retired")
	      " as SRFI editors.  At the same time, "
	      (a (@ (href "http://people.netscape.com/drush/")) "David Rush")
	      " and "
	      (a (@ (href "http://biblio06.fciencias.unam.mx/solsona/")) "Francisco Solsona")
	      " joined as new SRFI editors.")
	   (p "On March 14, 2004, "
	      (a (@ (href "http://www.cs.brandeis.edu/~dvanhorn/")) "David Van Horn")
	      " joined as a new SRFI editor.")
	   (p "On September 2, 2005, David Rush retired as SRFI editor; at the same time, "
	      (a (@ (href "http://www.rscheme.org/~donovan/")) "Donovan Kolbly")
	      " joined as a new SRFI editor.")
	   (p "On April 21, 2015, Arthur A. Gleckler joined as an editor after Michael announced his coming retirement as editor.")
	   (p "On July 7, 2015, David Van Horn, Donovan Kolbly, Francisco Solona, and Mike Sperber retired.")
	   (p "On the same date, web and email hosting for SRFI "
	      (a (@ (href "http://srfi-email.schemers.org/srfi-announce/msg/2736115")) "was moved")
	      " based on a new "
	      (a (@ (href "hosting-plan.html")) "Hosting Plan")
	      ".")
	   (hr)
	   (address (a (@ (href "mailto:srfi-editors at srfi dot schemers dot org"))))
	   (p "The history of this document is "
	      (a (@ (href "https://github.com/scheme-requests-for-implementation/srfi-common/commits/master/srfi-history.html")) "here")
	      ".")))))

(define (write-srfi-history-page)
  (write-html-file (path-append (srfi-common-dir) "srfi-history.html")
    (history-template)))

(define (privacy-template)
  `(*TOP*
    (!DOCTYPE html)
    (html (@ (lang "en"))
	  ,(admin-page-head "Scheme Requests For Implementation - Privacy Statement")
	  (body
	   (h1 (a (@ (href "/"))
		  (img (@ (class "srfi-logo")
			  (src "srfi-logo.svg")
			  (alt "SRFI surfboard logo"))))
	       "Scheme Requests For Implementation - Privacy Statement")
	   (p "The purpose of the SRFI web site is to facilitate creating and disseminating information about proposed additions to the Scheme programming language.  This work is done in public through documents and email messages published on this and other web sites.  All of the work is done by volunteers.  In support of this work, we use these third-party tools:")
	   (ul
	    (li (p (a (@ (href "https://www.linode.com/")) "Linode")
		   ", for the Linux server that runs the main web site, "
		   (a (@ (href "https://srfi.schemers.org/")) "srfi.schemers.org")
		   ".")
		(p "Here is " (a (@ (href "https://www.linode.com/privacy")) "Linode's privacy policy") ".")
		(p "The " (a (@ (href "https://srfi.schemers.org/")) "SRFI web site") " does not set cookies.")
		(p "As is standard for web sites, we maintain server logs that contain IP addresses and other identifying information.  We use this for debugging, for security, and for computing aggregate statistics.  Our intent is that only the editors are allowed access to this information.  We rotate the logs approximately every two weeks, so only our backups contain older log information.  However, we may indefinitely preserve log entries and other information that record apparent attacks on the site."))
	    (li (p (a (@ (href "https://github.com/")) "GitHub")
		   " for hosting code and documents that are part of the proposals.")
		(p "Here is " (a (@ (href "https://help.github.com/articles/github-privacy-statement/")) "GitHub's privacy statement") ".  Note that users who would prefer not to use GitHub can download all the code and documents "
		   (a (@ (href "https://srfi.schemers.org/srfi.tgz")) "here")
		   " (" (code ".tgz") "), directly from the SRFI web site."))
	    (li (p (a (@ (href "https://fonts.google.com/")) "Google Fonts")
		   " for hosting the open-source Noto Sans font, which includes many Unicode characters that other fonts don't."))
	    (li (p (a (@ (href "https://www.simplelists.com/")) "Simplelists")
		   " for delivering email messages and for displaying those messages in public archives.")
		(p "Note that most email messages sent to SRFI email addresses are displayed publicly in our archives, e.g. "
		   (a (@ (href "https://srfi-email.schemers.org/srfi-discuss")) "srfi-discuss")
		   ".  These archives are necessary because the point of SRFI is public, transparent discussion of proposals.")
		(p "Simplelists obfuscates email addresses in the public archives so that they can't be harvested by spammers.  However, since the point of the site is public discussion, we make no attempt to conceal the identities of participants.")
		(p "Here are the "
		   (a (@ (href "https://www.simplelists.com/privacy.php")) "Simplelists privacy policy")
		   " and "
		   (a (@ (href "https://www.simplelists.com/gdpr.php")) "GDPR statement")
		   "."))
	    (li (p "JavaScript library: "
		   (a (@ (href "https://www.mathjax.org/")) "MathJax")
		   ", which is used for displaying mathematics in some of the proposals.")
		(p "This library is hosted by the CloudFlare CDN ("
		   (a (@ (href "https://en.wikipedia.org/wiki/Content_delivery_network")) "Content Delivery Network")
		   ").  Here is "
		   (a (@ (href "https://www.cloudflare.com/security-policy/")) "CloudFlare's privacy policy")
		   ".")))
	   (p "If you have any concerns about SRFI, please contact "
	      (a (@ (href "mailto:srfi-editors at srfi dot schemers dot org")) "the SRFI editors")
	      ".  (Mail to the editors is archived, but typically not displayed publicly.)")
	   (hr)
	   (address (a (@ (href "mailto:srfi-editors at srfi dot schemers dot org")) "The SRFI Editors"))
	   (p "The history of this document is "
	      (a (@ (href "https://github.com/scheme-requests-for-implementation/srfi-common/commits/master/srfi-privacy.html")) "here")
	      ".")))))

(define (write-srfi-privacy-page)
  (write-html-file (path-append (srfi-common-dir) "srfi-privacy.html")
    (privacy-template)))

(define (process-template)
  `(*TOP*
    (!DOCTYPE html)
    (html (@ (lang "en"))
	  ,(admin-page-head "Scheme Request For Implementation \x2014; Process")
	  (body
	   (h1 (a (@ (href "/"))
		  (img (@ (class "srfi-logo")
			  (src "srfi-logo.svg")
			  (alt "SRFI surfboard logo"))))
	       "Scheme Request For Implementation \x2014; Process")
	   (p "by Dave Mason, with edits by various editors")

	   (h2 "Abstract")
	   (p "The goal of this mechanism is to provide a permanent registry for "
	      (em "Scheme Requests For Implementation")
	      " (SRFIs \x2014; pronounced \"surfie\").  This is not a formal standards-creation mechanism.  Rather, it is a formal way to manage the production of proposals for Scheme.")
	   (p "There are many other things that this process is not.  Discussion of those, rationales for some of the implementation details, and answers to related questions are to be found in the "
	      (a (@ (href "srfi-faq.html")) "SRFI FAQ page")
	      ".")
	   (p "Related to this process is a Scheme special form, documented in "
	      (a (@ (href "srfi-7/")) "SRFI 7")
	      ", that a Scheme program can use to ascertain whether an implementation supports a particular standard or feature.")

	   (h2 "Mechanism")
	   (p "The site "
	      (a (@ (href "https://srfi.schemers.org/")) (code "https://srfi.schemers.org/"))
	      " will provide an archive of "
	      (em "draft") ", " (em "final") " and " (em "withdrawn")
	      " SRFIs and a submission process to submit SRFIs for consideration.")
	   (p "The editors of the SRFIs will be experienced members of the Scheme community independent of the major implementors.  They will attempt to keep the quality of SRFIs high, but will ultimately accept any SRFI which conforms to the "
	      (a (@ (href "#structure")) "structure requirements")
	      " below.")
	   (p "A moderated mailing list, "
	      (code "srfi-announce at srfi dot schemers dot org")
	      ", will be used to announce when new SRFI proposals become "
	      (em "drafts")
	      ".  It will carry a final notification when the SRFI has been either "
	      (em "final") " or " (em "withdrawn")
	      ".  Anyone can subscribe to the list, and implementors are especially encouraged to subscribe.")
	   (p "There will be a mailing list created for the evaluation period of each SRFI where all discussion of the proposal will take place.  Anyone may join these lists.  The discussion will be archived and the archived discussion will remain part of the permanent record of the SRFI.")

	   (h2 "Process")
	   (p "All proposals must follow the following steps:")
	   (ol
	    (li "Authors submit a proposal by sending email to "
		(a (@ (href "mailto:srfi%20minus%20editors%20at%20srfi%20dot%20schemers%20dot%20org"))
		   (code "srfi minus editors at srfi dot schemers dot org"))
		".")
	    (li "Within seven days, one of the editors will read and respond to the proposal.  The response may be a request to clarify, justify, or withdraw the proposal.  Such a request must not reflect the personal bias of an editor.  Rather, it will be made strictly to maintain a high quality of submissions.  The editors may not turn a proposal back more than twice.  On the third submission, the editors will move the proposal to "
		(em "draft")
		" status if it conforms to the specification below.  At the discretion of the editors, a proposal that does not completely conform may be moved to "
		(em "draft")
		" status.  Every proposal must conform before it is moved to "
		(em "final") " status.")
	    (li "When the proposal has been vetted by the editors, it receives its SRFI number and becomes "
		(em "draft")
		".  The editors will create a mailing list for the discussion of the proposal.  A short notice of the new draft SRFI, including the title and abstract, SRFI number, URL, and instructions to access the SRFI's mailing list, will be sent to "
		(code "srfi minus announce at srfi dot schemers dot org")
		".  As part of the initial editing process, the editors will ensure that related standards (R"
		(sup "n")
		"RS, SRFIs, RFCs and others) are appropriately identified and that the proposal meets the structural requirements described below.  If other related standards are identified during the comment process or after acceptance, the editors will keep the references up-to-date.  A proposal cannot normally be finalized until "
		(strong "60 days")
		" have passed since its initial submission.")
	    (li (p "If the authors choose, they may submit revised versions of the proposal at any point during the comment period.  Every such revision shall be announced to the SRFI's mailing list, and all revisions will be retained in the permanent record of the SRFI.")
		(p "The SRFI process used to set a hard limit of ninety days on the discussion period.  This was done because \"Active discussion or revision after 90 days normally suggests that a proposal has been revised at least three times and is not yet mature enough for standardization.\"  The current editor has removed this limit, but "
		   (strong "strongly encourages authors to submit SRFIs that they believe will only take ninety days")
		   ", and "
		   (strong "reserves the right to withdraw SRFIs if authors become unresponsive")
		   "."))
	    (li "At any time during the comment period, the authors can choose to withdraw the proposal.  If the editors determine that insufficient time for discussion has followed a significant revision of the proposal, the proposal will be withdrawn.  Otherwise, the proposal will be made final if it meets the requirements below.  The outcome will be announced to "
		(code "srfi minus announce at srfi dot schemers dot org")
		".")
	    (li "If the SRFI is withdrawn at the end of the comment period, it will be moved to a "
		(em "withdrawn")
		" proposal archive.  Subsequent related proposals by the same or different authors may include/modify the text of the withdrawn proposal, and may include references to it.")
	    (li "When the authors are ready to submit an SRFI for finalization, they may, at their discretion, ask the editors to announce a \"last call\" period.  This is a period, typically a week, in which reviewers on the mailing list are encouraged to submit their final comments.  The hope is that all substantial issues have already been resolved, and that the last call period will just encourage people on the mailing list to do one final check.  If authors are confident that discussion of the SRFI is already finished, they may request finalization without a last call period.")
	    (li "When the SRFI is accepted, it will be placed on the list of "
		(em "final")
		" SRFIs.  This will include a link to the history of the proposal, including all earlier versions and the archive of the discussion from the comment period.  Any identified SRFIs that are superseded by or incompatible with the newly final SRFI will be updated to reflect this fact.")
	    (li "A final SRFI may later be withdrawn, but only if it has been replaced with a newer SRFI and the author of the original SRFI agrees.  In that case, there will be a \"See also\" link from the withdrawn SRFI to the new one.  Note that withdrawn SRFIs are still present on the web site, in full \x2014; just marked withdrawn."))

	   (figure (@ (id "srfi-states"))
		   (span (a (@ (href "#")) "\x00d7;"))
		   (a (@ (href "#srfi-states"))
		      (img (@ (alt "SRFI state transition diagram")
			      (src "states.svg"))))
		   (figcaption "SRFI state transitions"))

	   (p "Once the SRFI number has been assigned, the proposal will be in one of three states: "
	      (em "draft") ", " (em "final") ", or " (em "withdrawn")
	      ".  Lists of proposals in all three states will be available and archived indefinitely, and SRFI numbers will not be reused.  The only changes that may be made to a "
	      (em "final") " SRFI are:")
	   (ol
	    (li "updating of URLs (including those of related SRFIs)")
	    (li "noting that the SRFI has been deprecated or has been superseded by a subsequent SRFI")
	    (li "withdrawing the SRFI, but only with permission of the author, and only if a replacement SRFI has been finalized")
	    (li "improving the sample implementation and tests")
	    (li "correcting errors (but not errors of design), e.g. spelling errors, typos, or contradictory statements")
	    (li "adding \"post-finalization notes,\" which document later recommendations by the authors and are placed in the Status section"))
	   (p "Every Scheme implementation is encouraged to provide implementations of final SRFIs where possible, and to retain existing implementations of deprecated SRFIs for a reasonable time period.")
	   (p "New standards, such as R" (sup "n") "RS, may supersede or conflict with existing SRFIs.  The editors and authors will work to update the relationship of active SRFIs to such standards.")

	   (h3 (@ (id "errata")) "Errata")
	   (p "Anyone who finds an error should report it to the SRFI's discussion mailing list.  That way, it will be recorded publicly.")
	   (p "We always attempt to reach the author for guidance.")
	   (p "If the corrections are only to the implementation, not the document, we just make the change.")
	   (p "If the author agrees to proposed corrections to the document, or if the author never expresses an opinion but there is consensus about corrections on the mailing list, we'll revise the document.  See the example of "
	      (a (@ (href "https://srfi.schemers.org/srfi-158/srfi-158.html")) "SRFI 158")
	      ".  In this case, a tag like " (code "errata-1") " is added in the Git repository.")

	   (h2 (@ (id "structure")) "Structure")
	   (p "Every SRFI must meet the following requirements:")
	   (ol
	    (li "It must have a succinct title.")
	    (li "It must list the authors.")
	    (li "It must list related standards and SRFIs, including dependencies, conflicts, and replacements.")
	    (li "It must begin with an abstract.  This will be fewer than 200 words long.  It will outline the need for, and design of, the proposal.")
	    (li "It must contain a detailed rationale.  This will typically be 200-500 words long and will explain why the proposal should be incorporated as a standard feature in Scheme implementations.  If there are other standards which this proposal will replace or with which it will compete, the rationale should explain why the present proposal is a substantial improvement.")
	    (li (@ (id "detailed-spec"))
		"It must contain a detailed specification.  This should be detailed enough that a conforming implementation could be completely created from this description.")
	    (li "It must contain a sample implementation.  This requirement may be met (in order from the most to the least preferred) by:"
		(ol (@ (type "a"))
		    (li "A portable Scheme implementation (possibly using earlier SRFIs).  This is the most desirable option, because then implementors can provide a (possibly slow) implementation with no effort.")
		    (li "A mostly-portable solution that uses some kind of hooks provided in some Scheme interpreter/compiler.  In this case, a detailed specification of the hooks must be included so that the SRFI is self-contained.")
		    (li "An implementation-specific solution.  Ideally, tricky issues that had to be dealt with in the implementation will be identified.")
		    (li "A separately available implementation, where a sample implementation is large or requires extensive modifications (rather than just additions) to an existing implementation.  This implementation will eventually be archived along with the SRFI and the discussion related to it.")
		    (li (@ (id "outline-impl"))
			"An outline of how it might be implemented.  This should be considered a last resort, and in this case the rationale for the feature must be stronger."))
		(p "The sample implementation should normally conform to the specification in point 6 "
		   (a (@ (href "#detailed-spec")) "above")
		   ".  If there is any variance (such as the implementation being overly restrictive), the "
		   (strong "specification")
		   " will be considered correct, the variance should be explained, and a timetable provided for the sample implementation to meet the specification.")
		(p "The sample implementation should include automated tests.  Tests make porting to new Scheme implementations easier.  They also help users understand how your SRFI is to be used.  However, if the sample implementation is trivial or not really meant to be used, i.e. it is just a proof of concept, it's okay to omit tests.  That should be a rare case.  No specific test framework is required, but both SRFI 64 and SRFI 78 are available.")
		(p "A SRFI may be submitted without an implementation so that the specification may be refined before effort is invested in implementing it.  However, an implementation should be provided as soon as possible."))
	    (li "A proposal must be submitted in " (strong "HTML") " (3.2 or later) format following the "
		(a (@ (href "srfi-template.html")) "template located here")
		".  All proposals must be written in English, be properly formatted, and be reasonably grammatical.")
	    (li (@ (id "license"))
		"It must contain an MIT/Expat copyright statement as follows (where "
		(em "AUTHOR")
		" should be replaced by the name(s) of the author(s) and "
		(em "YEAR")
		" will be the year in which the SRFI number is allocated):"
		(blockquote
		 "Copyright (C) " (em "AUTHOR") " (" (em "YEAR") ")."
		 (p "Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the \"Software\"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:")
		 (p "The above copyright notice and this permission notice (including the next paragraph) shall be included in all copies or substantial portions of the Software.")
		 (p "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."))
		(p "In addition to the SRFI document, any software written specifically for the SRFI should be published under the above license.  However, at the editors' discretion, the SRFI may reference software that was already written and published under another license, citing that software as its sample implementation.  The BSD 2- and 3-clause licenses are specifically allowed for this purpose.")
		(p "Starting in December, 2023, SRFIs use the "
		   (a (@ (href "https://spdx.dev/")) "SPDX")
		   " (The Software Package Data Exchange) system for tracking licenses of SRFI sample implementations.  This is the checklist used to ensure that SPDX metadata is applied properly:"
		   (ol
		    (li "The SRFI text MUST include the text of the MIT/Expat license.")
		    (li "All source and data files included in the repo SHOULD be published under the MIT/Expat license."
			(ol (@ (type "a"))
			    (li "Any license notice already present in a file MUST be retained as is.")
			    (li "If the format of a file supports comments, and the file is at least fifteen lines long, it MUST contain a copyright notice.  It MUST also contain the corresponding SPDX metadata.  If the file uses a custom license, the SPDX "
				(a (@ (href "https://reuse.software/faq/#custom-license")) "LicenseRef")
				" strategy SHOULD be employed.  If a file is a derivative work, it may also include the license of the original work, or a reference to it, if that is required by the license of the original work.")
			    (li "If the format of a file does not support comments, nothing SHOULD be added to the file.  Instead, the standard external SPDX metadata SHOULD be used.")))
		    (li "The reuse lint command SHOULD pass, i.e. the SRFI SHOULD be REUSE-compliant.  This means that every source file SHOULD contain a SPDX-License-Identifier tag with the license, and that a LICENSES/ directory SHOULD contain the referenced license's text.")))))
	   (p "The editors may not reject a proposal because they disagree with the importance of the proposal, or because they think it is a wrong-headed approach to the problem.  The editors may, however, reject a proposal because it does not meet the requirements listed here.")
	   (p "In particular, lack of a sample implementation (as defined above) is grounds for rejection.  This can only occur if the \"sample implementation\" requirement is being met by an outlined implementation ("
	      (a (@ (href "#outline-impl")) "type 5")
	      "), and there is consensus that the implementation outline is not adequate.  Note that this is never a permanent rejection, because creation of an implementation of one of the other types is a complete refutation of this basis for rejection.")
	   (p "The other likely basis for rejection is an inadequate design specification.  In this case, the editors will attempt to help the author(s) conform to the requirements.")
	   (p "Remember, even if a proposal becomes a "
	      (em "final")
	      " SRFI, the need for it must be compelling enough for implementors to decide to incorporate it into their systems, or it will have been a waste of time and effort for everyone involved.  If the quality of any SRFI is not high, the likelihood of implementors adding this feature to their implementation is extremely low.")

	   (a (@ (id "definitions")))
	   (h2 "Definitions")
	   (p "The following terms may be used in SRFIs with the corresponding definitions: The term \"a SRFI <n>-conformant implementation\" means an implementation of Scheme which conforms to the specification described in SRFI <n>.")

	   (hr)
	   (address
	    (a (@ (href "mailto:srfi-editors%20at%20srfi%20dot%20schemers%20dot%20org"))
	       "The SRFI Editors"))
	   (p "The history of this document is "
	      (a (@ (href "https://github.com/scheme-requests-for-implementation/srfi-common/commits/master/srfi-process.html")) "here")
	      ".")))))

(define (write-srfi-process-page)
  (write-html-file (path-append (srfi-common-dir) "srfi-process.html")
    (process-template)))

(define (faq-template)
  `(*TOP*
    (!DOCTYPE html)
    (html (@ (lang "en"))
	  ,(admin-page-head "Scheme Request for Implementation - FAQ")
	  (body
	   (h1 (a (@ (href "/"))
		  (img (@ (class "srfi-logo")
			  (src "srfi-logo.svg")
			  (alt "SRFI surfboard logo"))))
	       "Scheme Request For Implementation - FAQ")
	   (p "The " (em "Scheme Request for Implementation") " process is "
	      (a (@ (href "srfi-process.html")) "documented elsewhere")
	      ".  This document is intended to provide some of the rationale about SRFIs rather than cluttering up that document.")
	   (p "The SRFI process grew out of the Scheme Workshop held in Baltimore, MD, on September 26, 1998, where the attendees considered a number of proposals for standardized feature sets for inclusion in Scheme implementations.  Many of the proposals received overwhelming support in a series of straw votes.  Along with this there was concern that the next Revised Report would not be produced for several years and this would prevent the timely implementation of standardized approaches to several important problems and needs in the Scheme community.")
	   (p "The SRFI process is a service provided to the Scheme community by the editors, currently Arthur A. Gleckler.  See "
	      (a (@ (href "srfi-history.html")) "SRFI History")
	      " for information on the previous editors.  As of July, 2015, SRFI is hosted on "
	      (a (@ (href "http://srfi-email.schemers.org/srfi-announce/msg/2736115")) "new mail and web hosts")
	      ".  (See the new "
	      (a (@ (href "hosting-plan.html")) "Hosting Plan")
	      ".)")
	   (dl
	    (dt "Are SRFIs standards?")
	    (dd
	     (p "Yes and no.  SRFIs are " (strong "not") " official standards.  There exist organizations such as ISO, IEEE, ANSI, etc. who are set up to develop official standards.  The SRFI process is designed as an attempt to maximize the quality of SRFIs within the constraint of not assigning authority to anyone.")
	     (p "There are official Scheme standards, ANSI Scheme and IEEE-1178-1990.  SRFIs are in addition to these standards.")
	     (p "On the other hand, the process for creating SRFIs " (strong "is") " standardized, and each final SRFI remains frozen and publicly available, hence they have many of the properties of standards.  So you might choose to think of them as unofficial standards."))

	    (dt "Are SRFIs replacements for R" (sup "n") "RS?")
	    (dd
	     (p "No.  Each Revised Report has been written by a different body, which determines what eventually is and is not included in that report.  The SRFI process is orthogonal to all of these reports.  Authors of SRFIs must not expect that their SRFIs will be included, or even be considered, by the authors of reports.")
	     (p "Of course anyone, including authors of future Revised Reports, is welcome to employ the definition and rationale of a SRFI, the discussion surrounding its adoption, and its widespread implementation to argue that the authors of a standard or report should consider adding the contents of the SRFI to that standard.")
	     (p "If SRFIs had existed before R" (sup "4") "RS, the macro appendix to that report might have made more sense to have been released as a SRFI.  But it didn't.  So it wasn't.")
	     (p "There is " (strong "considerable") " value in reading the "
		(a (@ (href "https://groups.csail.mit.edu/mac/projects/scheme/rrrs-archive.html")) "discussions of the R" (sup "n") "RS authors")
		", as they have often considered issues that are candidates for SRFIs."))

	    (dt (a (@ (id "preliminary")) "Are SRFIs a discussion forum for preliminary ideas?"))
	    (dd
	     (p "No.  SRFIs stands for " (em "Scheme Request for Implementation") ".  Note the last word.  If someone has amorphous ideas for something that would be cool, but has no idea how it might be done, they should discuss it in journals, workshops, seminars, or public forums like "
		(a (@ (href "https://www.reddit.com/r/scheme/")) "r/scheme")
		" on Reddit.  When the discussions have coalesced to the point where an implementation strategy is apparent, then it is time to write up a SRFI proposal."))

	    (dt "I really think that there should be a place to archive non-implementation documents.")
	    (dd
	     (p "You're not alone!  There seem to be lots of people who disagree with the editors on this point.  If you have ideas on how this should be done, please send mail to "
		(a (@ (href "mailto:srfi-editors%20at%20srfi%20dot%20schemers%20dot%20org"))
		   "<srfi minus editors at srfi dot schemers dot org>")))

	    (dt "Are SRFIs \"RFCs for the Scheme community?\"")
	    (dd
	     (p "Not quite.  As RFC1796 ("
		(a (@ (href "https://datatracker.ietf.org/doc/html/rfc1796")) "Not All RFCs are Standards")
		") says, RFCs serve a variety of purposes.  The SRFI editors feel that there are sufficient other venues for discussion of ideas.  The point of SRFIs is that a programmer can dependably test to see what features a Scheme implementation provides, and can therefore program in a portable way beyond the bounds of standardized Scheme."))

	    (dt "What's with all the time periods?  Why so little time?")
	    (dd
	     (p "The time periods are an attempt to drive the SRFI process as quickly as possible while maintaining sufficient time for sober second thought.  To maximize the quality of SRFIs, we want all of the relevant people to be involved in the discussion of any particular SRFI proposal.  Many of those people are very busy, and this mechanism constrains the time commitment they must make to stay on top of a proposal.  It also prevents discussion on a proposal from dragging on in an endless repetition of the same points, long after anyone's opinion is likely to be changed.")
	     (p "The other issue in the timing is to prevent editors from stone-walling a proposal.  It is not their job to make qualitative judgments about proposals, but rather to maintain the quality while expediting the creation of important unofficial standards.")
	     (p "If a proposal is still under discussion after 90 days, it means that it has been extended several times.  The editors will normally extend the discussion period to maintain a minimum of 15 days after any significant change.  Any proposal still under active discussion and revision after 90 days is not ready for codification.  It will then be withdrawn for a (normal) minimum of 30 days, after which it may be resubmitted.  If it is now in good shape, it will likely become final after the 60-day discussion period.  Thus, it will have been delayed a maximum of 90 days.  This is likely to happen only in very exceptional cases, and is the only cost of the fixed time periods."))

	    (dt (a (@ (id "teeth")) "What kind of standards are these, anyway?  There aren't any teeth in the rules!"))
	    (dd
	     (p "To have enforcement, there must be authority.  There is no absolute authority in the Scheme community, so there can be no absolute enforcement.  The final authority is the implementors.  If they believe that a particular SRFI documents a useful or important feature, they will add it to their implementations; if not, they won't.  The discussion relating to any SRFI will be retained indefinitely, and implementors can refer to that when making their decision.  Hence poorly worded, poorly reasoned, or poorly defended SRFIs will be nothing more than a waste of some time \x2014; regrettable, but necessary to retain an open process."))

	    (dt "Why do I have to include a sample implementation?")
	    (dd
	     (p "See the discussion above about "
		(a (@ (href "#preliminary")) "preliminary ideas")
		".  SRFIs are about implementation.  If you haven't either: (a) built one, or (b) have a very clear outline of how to build one, then you aren't documenting anything useful about implementation.  As the "
		(a (@ (href "srfi-process.html")) "process document")
		" says, if you think the editors are wrong to withdraw your proposal because it doesn't have a sufficient outline of implementation, then prove us wrong by going and implementing it on some system.  Then your SRFI will return to draft, and eventually active status."))

	    (dt "What standard should my sample implementation use?")
	    (dd
	     (p "We encourage contributors to use R" (sup "7") "RS combined with other SRFIs as a basis.  However, some SRFIs will require features not present in R" (sup "7") "RS and other SRFIs, and that's okay.  Furthermore, using other R" (sup "n") "RS standards, as well as IEEE Scheme, is acceptable."))

	    (dt "What should my sample implementation include?")
	    (dd
	     (p "It should implement all the features described in the SRFI document.")
	     (p "It should also include automated tests.  Having them will help implementors, and that will increase the likelihood that your SRFI will be incorporated in Scheme implementations.  It will also help users understand how your SRFI is to be used.")
	     (p "However, if the sample implementation is trivial or not really meant to be used, i.e. it is just a proof of concept, it's okay to omit tests.  That should be a rare case.")
	     (p "No specific test framework is required, but both SRFI 64 and SRFI 78 are available."))

	    (dt "Do SRFIs exist to describe the features of a particular Scheme implementation?")
	    (dd
	     (p "No.  Every SRFI should describe a cohesive feature set that is portable across a variety of Scheme implementations.  (Here we mean portable in the sense of being possible to implement, not in the sense of a portable implementation.) Rather than testing to see if the implementation is, e.g. Guile-4.3c, a program should test for the particular features that it requires.")
	     (p "This is a lesson learned from the Emacs world where it used to be that code would check to see what version of Emacs it was running on and make assumptions about the features that that particular version provided.  Unfortunately, that made the code un-portable to alternative Emacs implementations that had the required features but different version numbers.  The same lesson can be observed in old C code that assumed that such-and-such a system had particular features.  More commonly today, C programs use a configuration program that determines what libraries and functions are available, regardless of the system or compiler.  The mechanism documented in "
		(a (@ (href "srfi-7/")) "SRFI 7")
		" essentially provides a similar capability, while staying within Scheme code.")
	     (p "The biggest advantage of checking for features rather than implementations is that code becomes portable to systems of which the author was unaware, assuming that they provide the features that the program requires."))

	    (dt (p "The process document mentions that different SRFIs may conflict with each other.  Won't that make it impossible for an implementation to support conflicting SRFIs?"))
	    (dd "Not necessarily.  See " (a (@ (href "/srfi-7/")) "SRFI 7") "."
		(a (@ (id "copyright"))))

	    (dt "Does the SRFI copyright permit using a SRFI sample implementation (or a derivative of one) in my Scheme implementation?")
	    (dd (p "Yes."))

	    (dt "Does the SRFI copyright permit using parts of a SRFI in the documentation of my Scheme implementation?")
	    (dd (p "Yes."))

	    (dt "Where did the acronym come from?  It's a mouthful.")
	    (dd
	     (p "Alan Bawden suggested RFI at the Scheme workshop as a humorous reference to RFCs.  The S was added because the initial editors (and others) felt that \"Scheme\" should be in there somewhere.  If you pronounce it "
		(em "ess-are-eff-eye")
		" it certainly is a mouthful, but if you pronounce it "
		(em "surfie")
		", as we do, it's fine.  Scheme Implementation Request (SIR) was also proposed."))

	    (dt "Are there any special considerations if I use Github to propose or comment on a SRFI?")
	    (dd
	     (p "Please keep the discussion of each SRFI on its mailing list.  For example, if you create a pull request, send all your comments to the mailing list.  While Github's tools can be convenient, we don't want to lose any of our history to Github.  While their APIs make it straightforward to download almost everything (e.g. comments, issues, and pull requests), we don't have code in place to do that automatically."))

	    (dt "What if I don't want to use Github?")
	    (dd
	     (p "If you'd rather not use Github to propose SRFIs or revisions to them, feel free to point the editors at alternate Git repos or to send us patch files.  (Patch files can be created using "
		(code "git format-patch -M origin/master")
		" or " (code "git send-email")
		".) If you're a SRFI author and would rather not use Git, either, just send updated files, or links to them, or diffs, and we'll check in the changes."))

	    (dt "May I use Markdown or another format?")
	    (dd
	     (p "You may include a Markdown version, but you must still submit the SRFI document as HTML.")
	     (p "Below is a snippet from the " (code "Makefile") " of SRFI 123.  It generates HTML from Markdown using the Pandoc tool (GPL licensed).")
	     (pre "
      srfi-123.html: srfi-123.md
	pandoc \\
	  --css=http://srfi.schemers.org/srfi.css \\
	  --from=markdown_github-hard_line_breaks \\
	  --include-in-header=header.html \\
	  --standalone \\
	  --to=html \\
	  srfi-123.md \\
	  >srfi-123.html
      ")
	     (p "Remember that the editors need to make slight changes to the document, e.g. to add to its history of drafts, and won't edit the document in a format other than HTML."))

	    (dt (@ (id "conventions")) "What linguistic conventions do SRFIs use?")
	    (dd
	     (p "Starting in about 2019, new SRFIs use some of the conventions of the R7RS Small report ("
		(a (@ (href "https://small.r7rs.org/attachment/r7rs.pdf")) "PDF")
		"), section 1.3.2, a shortened form of which appears here.  Note that SRFIs before about 2019 may not follow these conventions.")
	     (p "When speaking of an error situation, we use the phrase \"an error is signaled\" to indicate that implementations must detect and report the error by raising a non-continuable exception, as if by the procedure "
		(code "raise")
		".  The phrase \"an error that satisfies " (i "predicate") " is signaled\" means that an error is signaled as above.  Furthermore, if the object that is signaled is passed to the specified predicate, the predicate returns " (code "#t") ".")
	     (p "The phrase \"is an error,\" however, means that implementations are not required to detect or report the error.  In fact, the implementation may take any action whatsoever, such as signaling an error, extending a procedure's behavior, or failing catastrophically.  If the value of an expression is said to be \"unspecified,\" then the expression must evaluate to some object without signaling an error, but the value depends on the implementation.")
	     (p "Finally, the words and phrases \"must,\" \"must not,\" \"shall,\" \"shall not,\" \"should,\" \"should not,\" \"may,\" \"required,\" \"recommended,\" and \"optional\" are to be interpreted as described in "
		(a (@ (href "https://www.ietf.org/rfc/rfc2119.html")) "RFC 2119")
		" unless otherwise specified.  They are used only with reference to implementer or implementation behavior, not with reference to programmer or program behavior."))

	    (dt "May I contribute code to a final SRFI, e.g. an implementation of the SRFI for a specific Scheme implementation?")
	    (dd
	     (p "We are happy to accept contributions.  We'll publish your code in the SRFI's Git repository.  Please include a "
		(code "README")
		" file, and use the copyright notice from the "
		(a (@ (href "srfi-process.html")) "SRFI process")
		"."))

	    (dt "Whom should I contact if I find a problem with the web site, etc?")
	    (dd
	     (p "Please send email to "
		(a (@ (href "mailto:srfi-editors%20at%20srfi%20dot%20schemers%20dot%20org"))
		   "srfi-editors@" (span (@ (class "antispam")) "nospam") "srfi.schemers.org")
		"."))

	    (dt (a (@ (id "errata")) "How are errata handled?"))
	    (dd
	     (p "See " (a (@ (href "srfi-process.html#errata")) "Errata") " in the Process document.")))
	   (hr)
	   (address
	    (a (@ (href "mailto:srfi-editors%20at%20srfi%20dot%20schemers%20dot%20org"))
	       "The SRFI Editors"))
	   (p "The history of this document is "
	      (a (@ (href "https://github.com/scheme-requests-for-implementation/srfi-common/commits/master/srfi-faq.html")) "here")
	      ".")))))

(define (write-srfi-faq-page)
  (write-html-file (path-append (srfi-common-dir) "srfi-faq.html")
    (faq-template)))

(define (write-srfi-list-subscribe-page)
  (write-html-file (path-append (srfi-common-dir) "srfi-list-subscribe.html")
    (srfi-list-subscribe-template
     schemecomm
     schemedoc
     schemeorg
     schemepersist
     schemeregistry
     schemetest
     schemeweb
     srfi-announce
     srfi-auto-subscribe
     srfi-discuss)))

(define-command (generate-common)
  "Generate the common pages."
  (write-srfi-about-page)
  (write-srfi-editors-page)
  (write-srfi-faq-page)
  (write-srfi-history-page)
  (write-srfi-home-page)
  (write-srfi-list-subscribe-page)
  (write-srfi-privacy-page)
  (write-srfi-process-page))

(define-command (generate-srfi num)
  "Generate the \"index.html\" and \"README.org\" file for SRFI <num>."
  (let ((number (string->number num)))
    (write-single-srfi-landing-page number)
    (write-single-srfi-readme number)))
