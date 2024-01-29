;;;; Generate "index.html" pages, etc. for SRFIs.

(define srfi-editor "Arthur A. Gleckler")

(define (faq-anchor-template description name)
  `(a (@ (href ,(concat "/srfi-faq.html#" name))) ,description))

(define (home-template keyword-options srfi-list)
  `(*TOP*
    (!DOCTYPE html)
    (html (@ (lang "en"))
	  (head
	   (meta (@ (charset "utf-8")))
	   (title "Scheme Requests for Implementation")
	   (link (@ (rel "preload") (href "srfi.js") (as "script")))
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
	   (p "Thanks to Lassi Kortela for his "
	      (code "srfi")
	      " command, which currently has eighty subcommands for carrying out various SRFI operations, e.g. show the abstract of a SRFI, search for a SRFI, open a SRFI in a browser, or clone the Git repo for a SRFI.  It can be found in the "
	      (a (@ (href "https://github.com/scheme-requests-for-implementation/srfi-common/tree/master/srfi-tools")) "SRFI tools")
	      " directory of the "
	      (a (@ (href "https://github.com/scheme-requests-for-implementation/srfi-common/")) "srfi-common")
	      " repo.")
	   (p "Thanks to Arvydas Silanskas for his "
	      (a (@ (href "https://index.scheme.org/")) "index.scheme.org")
	      ", which is an increasingly comprehensive index of Scheme implementations and standards.")
	   (p "Thanks to Shiro Kawai for his "
	      (a (@ (href "https://practical-scheme.net/")) "Practical Scheme")
	      ", which includes a "
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
	   (script
	    (@ (crossorigin "anonymous")
	       (integrity "sha384-JDmRxRiXkNkskRM5AD4qHRGk9ItwZ9flbqOpsRYs8SOrIRwcMtTGKP2Scnjptzgm")
	       (src "https://cdnjs.cloudflare.com/ajax/libs/list.js/1.5.0/list.min.js")))
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
		(title "SRFI Mailing Lists")
		(link (@ (rel "preload") (href "maillist.js") (as "script")))
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
	      "mailing list, which includes "
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
		       "The SRFI Editors"))
	   (script (@ (src "maillist.js") (type "application/javascript")))))))

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
		  (button (@ (class "invisible sub")) "Subscribe")
		  " "
		  (button (@ (class "invisible unsub")) "Unsubscribe")
		  " "
		  (button (@ (class "close invisible")) "X")
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
			     (value ,(concat name "@srfi.schemers.org")))))
		  (form (@ (class "unsub")
			   (method "POST")
			   (action "https://www.simplelists.com/subscribe.php"))
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
				  (value
				   ,(concat name "@srfi.schemers.org")))))))))

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
  "Generate the home page and subscription page."
  (write-srfi-home-page)
  (write-srfi-list-subscribe-page))

(define-command (generate-srfi num)
  "Generate the \"index.html\" and \"README.org\" file for SRFI <num>."
  (let ((number (string->number num)))
    (write-single-srfi-landing-page number)
    (write-single-srfi-readme number)))
