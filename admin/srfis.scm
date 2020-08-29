;;;; Create SRFI repositories on Github
;;;;
;;;; based on download from srfi.schemers.org created using
;;;;   <wget --mirror srfi.schemers.org>

(load-option 'format)

;; Set this manually.
(define github-authorization-token #f)

(define github-api-srfi-repos
  "https://api.github.com/orgs/scheme-requests-for-implementation/repos")

(define-record-type srfi
    (make-srfi number status title authors based-on see-also keywords
	       done-date draft-date)
    srfi?
  (number     srfi/number)
  (status     srfi/status)
  (title      srfi/title)
  (authors    srfi/authors)
  (based-on   srfi/based-on)
  (see-also   srfi/see-also)
  (keywords   srfi/keywords)
  (done-date  srfi/done-date)
  (draft-date srfi/draft-date))		; final or withdrawn

(set-record-type-unparser-method!
 srfi
 (standard-unparser-method
  'srfi
  (lambda (srfi port)
    (write-string " #" port)
    (write (srfi/number srfi) port)
    (write-char #\space port)
    (write (srfi/title srfi) port))))

(define (alist->srfi alist)
  (make-srfi (srfi-attribute alist 'number)
	     (srfi-attribute alist 'status)
	     (srfi-attribute alist 'title)
	     (srfi-attribute alist 'author 'multiple-distinct)
             (srfi-attribute alist 'based-on #f 'optional)
	     (srfi-attribute alist 'see-also 'multiple 'optional)
	     (srfi-attribute alist 'keywords 'multiple)
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

(define (srfi-attribute alist name #!optional multiple optional?)
  (let ((multiple  (if (default-object? multiple)  #f multiple))
        (optional? (if (default-object? optional?) #f optional?))
        (matches   (filter (lambda (entry) (eq? name (car entry)))
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
      (else (error "Bad argument")))))

(define srfi-keywords
  '((algorithm "Algorithm")
    (assignment "Assignment")
    (binding "Binding")
    (comparison "Comparison")
    (concurrency "Concurrency")
    (continuations "Continuations")
    (control-flow "Control Flow")
    (data-structure "Data Structure")
    (error-handling "Error Handling")
    (exceptions "Exceptions")
    (features "Features")
    (i/o "I/O")
    (internationalization "Internationalization")
    (introspection "Introspection")
    (lazy-evaluation "Lazy Evaluation")
    (miscellaneous "Miscellaneous")
    (modules "Modules")
    (multiple-value-returns "Multiple Value Returns")
    (numbers "Numbers")
    (operating-system "Operating System")
    (optimization "Optimization")
    (parameters "Parameters")
    (pattern-matching "Pattern Matching")
    (r6rs-process "R6RS process")
    (r7rs-large "R7RS Large")
    (r7rs-large-red "R7RS Large: Red Edition")
    (r7rs-large-tangerine "R7RS Large: Tangerine Edition")
    (randomness "Randomness")
    (reader-syntax "Reader Syntax")
    (superseded "Superseded")
    (syntax "Syntax")
    (testing "Testing")
    (type-checking "Type Checking")))

(define srfi-assoc
  (association-procedure = srfi/number))

(define (create-github-repository number)
  (let ((description (srfi/title (srfi-assoc number srfis))))
    (run-shell-command
     (format #f
	     "curl -i -H 'Authorization: token ~A' -d '{ \"name\": \"srfi-~A\", \"description\": \"~A\", \"has_issues\": false, \"has_wiki\": false }' ~A~%"
	     github-authorization-token
	     number
	     description
	     github-api-srfi-repos))))