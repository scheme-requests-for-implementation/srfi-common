;;; This code works in Chibi Scheme and MIT Scheme.

(define-record-type srfi
    (make-srfi number status title authors based-on see-also keywords
	       library-name done-date draft-date)
    srfi?
  (number	srfi/number)
  (status	srfi/status)
  (title	srfi/title)
  (authors	srfi/authors)
  (based-on	srfi/based-on)
  (see-also	srfi/see-also)
  (keywords	srfi/keywords)
  (library-name srfi/library-name)
  (done-date	srfi/done-date)
  (draft-date	srfi/draft-date))		; final or withdrawn

(define srfi-attribute
  (case-lambda
    ((alist name)
     (srfi-attribute alist name #f #f))
    ((alist name multiple)
     (srfi-attribute alist name multiple #f))
    ((alist name multiple optional?)
     (let ((matches   (filter (lambda (entry) (eq? name (car entry)))
                              alist)))
       (when (and (null? matches) (not optional?))
	 (error "Missing required attribute." name alist))
       (case multiple
	 ((#f)
	  (cond (optional? (if (null? matches) #f (cadar matches)))
		(else
		 (unless (or (null? matches)
			     (and (= 1 (length matches))
				  (= 2 (length (car matches)))))
		   (error "Duplicate property." name alist))
		 (cadar matches))))
	 ((multiple) (append-map cdr matches))
	 ((multiple-distinct) (map cdr matches))
	 (else (error "Bad argument")))))))

(define (alist->srfi alist)
  (make-srfi (srfi-attribute alist 'number)
	     (srfi-attribute alist 'status)
	     (srfi-attribute alist 'title)
	     (srfi-attribute alist 'author 'multiple-distinct)
             (srfi-attribute alist 'based-on #f 'optional)
	     (srfi-attribute alist 'see-also 'multiple 'optional)
	     (srfi-attribute alist 'keywords 'multiple)
	     (srfi-attribute alist 'library-name #f 'optional)
	     (srfi-attribute alist 'done-date #f 'optional)
	     (srfi-attribute alist 'draft-date)))

(define (read-srfi-data pathname)
  (with-input-from-file pathname
    (lambda ()
      (let next ((accumulator '()))
	(let ((record (read)))
	  (if (eof-object? record)
	      (reverse! accumulator)
	      (next (cons (alist->srfi record) accumulator))))))))

(define (srfi-assoc number srfis)
  (let find ((srfis srfis))
    (cond ((null? srfis) #f)
	  ((= number (srfi/number (car srfis)))
	   (car srfis))
	  (else (find (cdr srfis))))))

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
	 (let ((output (open-output-string)))
	   (let next ((remaining string-list))
	     (write-string (car remaining) output)
	     (cond ((null? (cddr remaining))
		    (write-string ", and " output)
		    (write-string (cadr remaining) output))
		   (else (write-string ", " output)
			 (next (cdr remaining)))))
	   (get-output-string output)))))

(define (format-srfi-authors authors)
  (string-join-english
   (map (lambda (author)
          (let ((author-name (car author)))
            (if (null? (cdr author))
                author-name
                (let ((author-role (cadr author)))
                  (string-append author-name " (" author-role ")")))))
        authors)))