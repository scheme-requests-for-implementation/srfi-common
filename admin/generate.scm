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

(define home-template (read-template "srfi-common/admin/home.template"))

(define srfi-box-template
  (read-template "srfi-common/admin/srfi-box.template"))

(define index-template (read-template "srfi-common/admin/index.template"))
(define archive-simplelists-template
  (read-template "srfi-common/admin/archive-simplelists.template"))
(define readme-template (read-template "srfi-common/admin/readme.template"))

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
	  (with-output-to-string
	    (lambda ()
	      (invoke-template archive-simplelists-template
			       `((number ,number)
				 (prefix ""))))))
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
			   (status ,status)
			   (title ,title)))))))

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

;; Note that this generates "index.html", which is separate from the
;; "README.org" page.
(define (write-srfi-home-page)
  (let ((srfi-list
	 (with-output-to-string
	   (lambda ()
	     (for-each (lambda (s)
			 (let* ((number (srfi/number s))
				(status (srfi/status s))
				(url (format #f "srfi-~A" number)))
			   (invoke-template
			    srfi-box-template
			    `((authors ,(srfi/authors s))
			      (date ,(srfi-date-to-show s))
			      (date-type ,(status->name status))
			      (name ,(srfi/title s))
			      (number ,number)
			      (status ,status)
			      (url ,url)))))
		       srfis)))))
    (with-output-to-file "$ss/srfi-common/index.html"
      (lambda ()
	(invoke-template home-template
			 `((srfi-list ,srfi-list)))))))

(define (write-srfi-status status)
  (let* ((status-name (status->name status))
	 (srfi-list
	  (with-output-to-string
	    (lambda ()
	      (for-each (lambda (s)
			  (invoke-template
			   srfi-box-template
			   `((authors ,(srfi/authors s))
			     (date ,(srfi-date-to-show s))
			     (date-type ,status-name)
			     (name ,(srfi/title s))
			     (number ,(srfi/number s))
			     (status ,status))))
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