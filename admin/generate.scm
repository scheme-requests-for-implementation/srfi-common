;;;; Generate "index.html" pages for SRFIs

;;; Copyright MMXV Arthur A. Gleckler.  All rights reserved.

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

(define (write-srfi-index-pages)
  (if (not (file-directory? "admin"))
      (error "Working directory must be root of <gh-pages> branch."))
  (let ((database (with-input-from-file "admin/database.scm" read))
	(template (read-template "admin/index.template")))
    (do ((database database (cdr database)))
	((null? database))
      (let* ((srfi (car database))
	     (number (car srfi))
	     (status (cadr srfi))
	     (title (caddr srfi))
	     (authors (cadddr srfi))
	     (pathname (format #f "srfi-~A/index.html" number)))
	(with-output-to-file pathname
	  (lambda ()
	    (invoke-template template `((authors ,authors)
					(number ,number)
					(status ,status)
					(title ,title)))))))))