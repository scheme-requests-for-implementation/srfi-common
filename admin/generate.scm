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

(define index-template (read-template "srfi-common/admin/index.template"))
(define archive-old-template
  (read-template "srfi-common/admin/archive-old.template"))
(define archive-simplelists-template
  (read-template "srfi-common/admin/archive-simplelists.template"))
(define readme-template (read-template "srfi-common/admin/readme.template"))

(define (write-single-srfi-index-page srfi)
  (let* ((number (car srfi))
	 (archives
	  (if (< number first-simplelists-srfi)
	      (with-output-to-string
		(lambda ()
		  (invoke-template archive-old-template '())))
	      (with-output-to-string
		(lambda ()
		  (invoke-template archive-simplelists-template
				   `((number ,number)))))))
	 (status (cadr srfi))
	 (title (caddr srfi))
	 (authors (cadddr srfi))
	 (pathname (format #f "srfi-~A/index.html" number)))
    (with-output-to-file pathname
      (lambda ()
	(invoke-template index-template
			 `((authors ,authors)
			   (email-archives ,archives)
			   (number ,number)
			   (status ,status)
			   (title ,title)))))))

(define (write-single-srfi-readme srfi)
  (let* ((number (car srfi))
	 (status (cadr srfi))
	 (title (caddr srfi))
	 (authors (cadddr srfi))
	 (pathname (format #f "srfi-~A/readme.org" number)))
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