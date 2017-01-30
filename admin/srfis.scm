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
    (%make-srfi number status title authors done-date draft-date)
    srfi?
  (number     srfi/number)
  (status     srfi/status)
  (title      srfi/title)
  (authors    srfi/authors)
  (done-date  srfi/done-date)
  (draft-date srfi/draft-date))		; final or withdrawn

(set-record-type-unparser-method!
 srfi
 (standard-unparser-method
  'srfi
  (lambda (srfi port)
    (write-char #\space port)
    (write (srfi/number srfi) port)
    (write-char #\space port)
    (write (srfi/title srfi) port))))

(define (make-srfi number status title authors draft-date #!optional done-date)
  (%make-srfi number
	      status
	      title
	      authors
	      (and (not (default-object? done-date))
		   done-date)
	      draft-date))

(define srfis (map (lambda (l) (apply make-srfi l)) srfi-data))

;; This is the first SRFI whose email archive was hosted first on
;; Simplelists.  SRFI with smaller numbers were originally hosted on
;; Michael Sperber's FreeBSD system, so the archive links are different.
(define first-simplelists-srfi 122)

(define srfi-no-post-mail '(104 117 118 120 121))

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