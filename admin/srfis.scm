;;;; Create SRFI repositories on Github
;;;;
;;;; based on download from srfi.schemers.org created using
;;;;   <wget --mirror srfi.schemers.org>

;;; This code works in MIT Scheme.

(load-option 'format)

;; Set this manually.
(define github-authorization-token #f)

(define github-api-srfi-repos
  "https://api.github.com/orgs/scheme-requests-for-implementation/repos")

(set-record-type-unparser-method!
 srfi
 (standard-unparser-method
  'srfi
  (lambda (srfi port)
    (write-string " #" port)
    (write (srfi/number srfi) port)
    (write-char #\space port)
    (write (srfi/title srfi) port))))

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
    (multiple-value-returns "Multiple-Value Returns")
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
    (sicp "SICP")
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