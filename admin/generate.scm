;;;; Generate "index.html" pages for SRFIs

;; This runs on MIT Scheme.

(declare (usual-integrations))

(load-option 'format)

(define (read-entire-file pathname)
  (with-input-from-file pathname
    (lambda ()
      (with-output-to-string
	(lambda ()
	  (let loop ()
	    (let ((char (read-char)))
	      (cond ((not (eof-object? char))
		     (write-char char)
		     (loop))))))))))

(define (parse-template string)
  (let ((size (string-length string)))
    (let loop ((accumulator '())
	       (index 0))
      (let ((start (substring-search-forward "<*<" string index size)))
	(if start
	    (let ((end (substring-search-forward ">*>"
						 string
						 (+ index 3)
						 size)))
	      (if end
		  (loop (cons* (string->symbol
				(substring string (+ start 3) end))
			       (substring string index start)
			       accumulator)
			(+ end 3))
		  (error "Missing variable end." index)))
	    (reverse! (cons (substring string index size)
			    accumulator)))))))

(define (read-template pathname)
  (parse-template (read-entire-file pathname)))

(define (invoke-template template variables)
  (do ((template template (cdr template)))
      ((null? template))
    (let ((element (car template)))
      (cond ((string? element) (write-string element))
	    ((symbol? element)
	     (let ((association (assq element variables)))
	       (if association
		   (display (cadr association))
		   (error "Missing variable." element))))))))

(define (invoke-template/string template variables)
  (with-output-to-string
    (lambda ()
      (invoke-template template variables))))

(define faq-anchor-template
  (read-template "srfi-common/admin/faq-anchor.template"))

(define home-template (read-template "srfi-common/admin/home.template"))

(define replace-template
  (read-template "srfi-common/admin/replace.template"))

(define replace-text-template
  (read-template "srfi-common/admin/replace-text.template"))

(define srfi-card-template
  (read-template "srfi-common/admin/srfi-card.template"))

(define index-template (read-template "srfi-common/admin/index.template"))
(define archive-simplelists-template
  (read-template "srfi-common/admin/archive-simplelists.template"))
(define readme-template (read-template "srfi-common/admin/readme.template"))

(define srfi-anchor-template
  (read-template "srfi-common/admin/srfi-anchor.template"))

(define srfi-list-template
  (read-template "srfi-common/admin/srfi-list.template"))

(define (srfi-date-to-show srfi)
  (case (srfi/status srfi)
    ((draft) (srfi/draft-date srfi))
    ((final) (srfi/done-date srfi))
    ((withdrawn) (srfi/done-date srfi))
    (else (error "Unknown status."))))

(define (write-single-srfi-index-page srfi)
  (let* ((number (srfi/number srfi))
	 (archives
	  (invoke-template/string
	   archive-simplelists-template
	   `((number ,number)
	     (prefix ""))))
	 (date (srfi-date-to-show srfi))
	 (status (srfi/status srfi))
	 (title (srfi/title srfi))
	 (authors (srfi/authors srfi))
	 (pathname (format #f "srfi-~A/index.html" number)))
    (with-output-to-file pathname
      (lambda ()
	(invoke-template index-template
			 `((authors ,authors)
			   (date ,date)
			   (email-archives ,archives)
			   (number ,number)
			   (replaced-by
			    ,(srfi-replace-fragment
			      "replaced-by"
			      (string-append
			       "This SRFI has been "
			       (faq-anchor-fragment "replacement"
						    "replaced by"))
			      (srfi-replaced-by srfi)))
			   (replaces
			    ,(srfi-replace-fragment
			      "replaces"
			      (string-append
			       "This SRFI is intended to "
			       (faq-anchor-fragment "replacement" "replace"))
			      (srfi/replaces srfi)))
			   (status ,status)
			   (title ,title)))))))

(define (srfi-replace-text prefix srfis)
  (define (srfi-link srfi)
    (let ((n (number->string srfi)))
      (string-append
       "[[https://srfi.schemers.org/srfi-"
       n
       "/][SRFI "
       n
       "]]")))
  (if (null? srfis)
      ""
      (invoke-template/string
       replace-text-template
       `((prefix ,prefix)
	 (srfis
	  ,(string-join-english
	    (map srfi-link (sort srfis <))))))))

(define (write-single-srfi-readme srfi)
  (let* ((number (srfi/number srfi))
	 (status (srfi/status srfi))
	 (title (srfi/title srfi))
	 (authors (srfi/authors srfi))
	 (pathname (format #f "srfi-~A/README.org" number)))
    (with-output-to-file pathname
      (lambda ()
	(invoke-template readme-template
			 `((authors ,authors)
			   (number ,number)
			   (replaced-by ,(srfi-replace-text
					  "It has been replaced by"
					  (srfi-replaced-by srfi)))
			   (replaces ,(srfi-replace-text
				       "It is intended to replace"
				       (srfi/replaces srfi)))
			   (status ,status)
			   (title ,title)))))))

(define (write-srfi-index-pages)
  (if (not (file-directory? "srfi-0"))
      (error "Working directory must contain all SRFI directories."))
  (do ((srfis srfis (cdr srfis)))
      ((null? srfis))
    (write-single-srfi-index-page (car srfis))))

(define (write-srfi-readmes)
  (if (not (file-directory? "srfi-0"))
      (error "Working directory must contain all SRFI directories."))
  (do ((srfis srfis (cdr srfis)))
      ((null? srfis))
    (write-single-srfi-readme (car srfis))))

(define (status->name status)
  (case status
    ((draft) "Draft")
    ((final) "Final")
    ((withdrawn) "Withdrawn")
    (else (error "Unknown status."))))

(define (faq-anchor-fragment name description)
  (invoke-template/string
   faq-anchor-template
   `((description ,description)
     (name ,name))))

(define (srfi-anchor-fragment srfi)
  (invoke-template/string
   srfi-anchor-template
   `((number ,(number->string (if (number? srfi)
				 srfi
				 (srfi/number srfi)))))))

(define (string-join-english string-list)
  "Return a string constructed by appending the elements of
`string-list', separating them with \", \" except for the last pair,
which should be separated by \" and\" if there are only two elements
and \", and\" otherwise."
  (cond ((null? string-list) "")
	((null? (cdr string-list)) (car string-list))
	((null? (cddr string-list))
	 (string-append (car string-list)
			" and "
			(cadr string-list)))
	(else
	 (with-output-to-string
	   (lambda ()
	     (let next ((remaining string-list))
	       (write-string (car remaining))
	       (cond ((null? (cddr remaining))
		      (write-string ", and ")
		      (write-string (cadr remaining)))
		     (else (write-string ", ")
			   (next (cdr remaining))))))))))

(define (srfi-replace-fragment class prefix srfis)
  (if (null? srfis)
      ""
      (invoke-template/string
       replace-template
       `((class ,class)
	 (prefix ,prefix)
	 (srfis
	  ,(string-join-english
	    (map srfi-anchor-fragment
		 (sort srfis <))))))))

(define (write-srfi-card srfi)
  (let ((status (srfi/status srfi)))
    (invoke-template
     srfi-card-template
     `((authors ,(srfi/authors srfi))
       (date ,(srfi-date-to-show srfi))
       (date-type ,(status->name status))
       (name ,(srfi/title srfi))
       (number ,(srfi/number srfi))
       (replaced-by ,(srfi-replace-fragment
		      "replaced-by"
		      (faq-anchor-fragment "replacement" "Replaced by")
		      (srfi-replaced-by srfi)))
       (replaces ,(srfi-replace-fragment
		   "replaces"
		   (faq-anchor-fragment "replacement" "Replaces")
		   (srfi/replaces srfi)))
       (status ,status)))))

;; Note that this generates "index.html", which is separate from the
;; "README.org" page.
(define (write-srfi-home-page)
  (let ((srfi-list
	 (with-output-to-string
	   (lambda ()
	     (for-each write-srfi-card srfis)))))
    (with-output-to-file "$ss/srfi-common/index.html"
      (lambda ()
	(invoke-template home-template
			 `((srfi-list ,srfi-list)))))))

(define (write-srfi-status status)
  (let* ((status-name (status->name status))
	 (srfi-list
	  (with-output-to-string
	    (lambda ()
	      (for-each write-srfi-card
			(filter (lambda (s)
				  (eq? (srfi/status s) status))
				srfis))))))
    (with-output-to-file (format #f "$ss/srfi-common/~A-srfis.html" status)
      (lambda ()
	(invoke-template srfi-list-template
			 `((srfi-list ,srfi-list)
			   (status ,status-name)))))))

(define (write-srfi-status-pages)
  (do-list (status '(draft final withdrawn))
      (write-srfi-status status)))