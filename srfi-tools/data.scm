;; Update "admin/srfi-data-convert.el" at
;; <git@github.com:srfi-explorations/emacs-srfi> whenever this
;; changes.
(define srfi-keyword-alist
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
    (garbage-collection "Garbage Collection")
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
    (record-type "Record Type")
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

(define keyword-entry/symbol first)
(define keyword-entry/title second)

(define (srfi-format-keyword keyword)
  (keyword-entry/title
   (or (assoc keyword srfi-keyword-alist)
       (error "No such SRFI keyword" keyword))))

(define (srfi-available-keywords)
  (map keyword-entry/symbol srfi-keyword-alist))

(define (keyword->name keyword)
  (let ((a (assq keyword srfi-keyword-alist)))
    (assert a "No such keyword.")
    (keyword-entry/title a)))

(define-command (keyword-symbols)
  "Display the symbolic version of all the keywords that can be assigned to SRFIs."
  (for-each disp (srfi-available-keywords)))

(define-command (keywords)
  "Display the English version of all the keywords that can be assigned to SRFIs."
  (for-each disp (map keyword-entry/title srfi-keyword-alist)))

;;

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

(define-record-type srfi
  (make-srfi number status title authors based-on see-also keywords
             library-name done-date draft-date)
  srfi?
  (number       srfi/number)
  (status       srfi/status)
  (title        srfi/title)
  (authors      srfi/authors)
  (based-on     srfi/based-on)
  (see-also     srfi/see-also)
  (keywords     srfi/keywords)
  (library-name srfi/library-name)
  (done-date    srfi/done-date)
  (draft-date   srfi/draft-date))               ; final or withdrawn

(define (alist->srfi alist)
  (make-srfi
   (srfi-attribute alist 'number)
   (srfi-attribute alist 'status)
   (srfi-attribute alist 'title)
   (srfi-attribute alist 'author 'multiple-distinct)
   (srfi-attribute alist 'based-on #f 'optional)
   (srfi-attribute alist 'see-also 'multiple 'optional)
   (srfi-attribute alist 'keywords 'multiple)
   (srfi-attribute alist 'library-name #f 'optional)
   (srfi-attribute alist 'done-date #f 'optional)
   (srfi-attribute alist 'draft-date)))

(define (srfi-data)
  (with-input-from-text-file (srfi-data-file) read-all))

(define-command (data)
  "Display the SRFI database, which is an S-expression."
  (dump-file (srfi-data-file))
  (newline))

;; (define srfi-assoc
;;   (association-procedure = srfi/number))

(define (srfi-assoc number srfis)
  (let find ((srfis srfis))
    (cond ((null? srfis) #f)
          ((= number (srfi/number (car srfis)))
           (car srfis))
          (else (find (cdr srfis))))))

(define srfi-list
  (let ((srfis #f))
    (lambda ()
      (or srfis
          (begin (set! srfis (map alist->srfi (srfi-data)))
                 srfis)))))

(define (srfi-by-number num)
  (or (find (lambda (srfi) (= num (srfi/number srfi)))
            (srfi-list))
      (error "No such SRFI" num)))

(define (srfi-last-number)
  (srfi/number (last (srfi-list))))

(define (resolve srfi)
  (cond ((srfi? srfi)
         srfi)
        ((number? srfi)
         (srfi-by-number srfi))
        (else (error "Not a SRFI" srfi))))

(define (resolved getter)
  (lambda (srfi)
    (getter (resolve srfi))))

(define srfi-number (resolved srfi/number))
(define srfi-status (resolved srfi/status))
(define srfi-title (resolved srfi/title))
(define srfi-authors (resolved srfi/authors))
(define srfi-based-on (resolved srfi/based-on))
(define srfi-see-also (resolved srfi/see-also))
(define srfi-keywords (resolved srfi/keywords))
(define srfi-library-name (resolved srfi/library-name))
(define srfi-done-date (resolved srfi/done-date))
(define srfi-draft-date (resolved srfi/draft-date))

;;

(define (srfi-status-string srfi)
  (symbol->string (srfi-status srfi)))

(define (srfi-status->name status)
  (case status
    ((draft) "Draft")
    ((final) "Final")
    ((withdrawn) "Withdrawn")
    (else (error "Unknown status."))))

(define (srfi-draft? srfi)
  (eqv? (srfi-status srfi) 'draft))

(define (srfi-final? srfi)
  (eqv? (srfi-status srfi) 'final))

(define (srfi-date-of-last-update srfi)
  (or (srfi-done-date srfi)
      (srfi-draft-date srfi)))

(define (srfi-date-to-show srfi)
  (case (srfi-status srfi)
    ((draft) (srfi-draft-date srfi))
    ((final) (srfi-done-date srfi))
    ((withdrawn) (srfi-done-date srfi))
    (else (error "Unknown status."))))

;;

(define srfi-author-name first)

(define (srfi-author-role author)
  (and (not (null? (rest author)))
       (second author)))

(define (srfi-format-author author)
  (let ((name (srfi-author-name author))
        (role (srfi-author-role author)))
    (if (not role) name (string-append name " (" role ")"))))

(define (srfi-format-authors authors)
  (string-join-english (map srfi-format-author authors)))

(define (srfi-author-first-name author)
  (let* ((full-name (srfi-author-name author))
	 (i (string-index full-name (lambda (c) (char=? c #\space)))))
    (if i
	(substring full-name 0 i)
	full-name)))

(define (srfi-format-authors/first-names authors)
  (string-join-english (map srfi-author-first-name authors)))

(define-command (authors num)
  "Display the authors of SRFI <num>."
  (for-each disp
            (map srfi-format-author
                 (srfi/authors
                  (srfi-by-number (parse-srfi-number num))))))

(define (srfi-format srfi . parentheses)
  (let ((number-title
         (string-append "SRFI " (number->string (srfi-number srfi))
                        ": " (srfi-title srfi))))
    (if (null? parentheses)
        number-title
        (string-append
         number-title
         " ("
         (string-join (map (lambda (fact)
                             (if (string? fact)
                                 fact
                                 (fact srfi)))
                           parentheses)
                      " ")
         ")"))))

(define (write-custom-srfi-list srfis . parentheses)
  (for-each (lambda (srfi)
              (write-line (apply srfi-format srfi parentheses)))
            srfis))

(define (write-srfi-list srfis)
  (write-custom-srfi-list srfis
                          srfi-status-string
                          srfi-date-of-last-update))

;;

(define-command (list)
  "Display a list of all the SRFIs."
  (write-srfi-list (srfi-list)))

(define (srfi-tail)
  (take-right (srfi-list) 10))

(define-command (tail)
  "Display a list of the most recent ten SRFIs."
  (write-srfi-list (srfi-tail)))

(define (srfi-range min max)
  (filter (lambda (srfi) (<= min (srfi-number srfi) max))
          (srfi-list)))

(define-command (range min max)
  "List SRFIs with numbers <min>..<max>."
  (write-srfi-list (srfi-range (parse-srfi-number min)
                               (parse-srfi-number max))))

(define (srfi-near num)
  (srfi-range (- num 5) (+ num 5)))

(define-command (near num)
  "List a few SRFIs with numbers around <num>."
  (write-srfi-list (srfi-near (parse-srfi-number num))))

(define (srfi-age-in-days srfi)
  (days-between (iso-date->date (srfi-draft-date srfi))
                (current-date)))

(define (srfi-format-age srfi)
  (format "~a days" (srfi-age-in-days srfi)))

(define (srfi-drafts)
  (filter srfi-draft? (srfi-list)))

(define-command (drafts)
  "Display a list of all the draft SRFIs."
  (let-values (((older newer)
                (partition (lambda (srfi) (>= (srfi-age-in-days srfi) 60))
                           (srfi-drafts))))
    (define (entry srfi)
      (cons (srfi-format srfi) (srfi-format-age srfi)))
    (define gap-entry
      (cons "----" "--"))
    (display-two-column-table
     (append (map entry older) (list gap-entry) (map entry newer)))))

(define (srfi-by get-strings query)
  (let ((query (string-downcase query)))
    (filter (lambda (srfi)
              (any (lambda (string)
                     (string-contains (string-downcase string) query))
                   (get-strings srfi)))
            (srfi-list))))

(define (srfi-by-author name)
  (srfi-by (lambda (srfi) (map srfi-author-name (srfi-authors srfi)))
           name))

(define-command (by-author name)
  "List all SRFIs with <name> as an author."
  (write-srfi-list (srfi-by-author name)))

(define (srfi-by-keyword keyword)
  (srfi-by (lambda (srfi) (map srfi-format-keyword (srfi-keywords srfi)))
           keyword))

(define-command (by-keyword keyword)
  "List all SRFIs filed under <keyword>."
  (write-srfi-list (srfi-by-keyword keyword)))

(define (srfi-search words)
  (let ((words (map string-downcase words)))
    (filter (lambda (srfi)
	      (every (lambda (w)
		       (string-contains (string-downcase (srfi-title srfi))
					w))
		     words))
	    (srfi-list))))

(add-command!
 "search"
 '(word ...)
 "Display a list of all the SRFIs whose titles contain all <word>s."
 1
 #f
 (lambda words (write-srfi-list (srfi-search words))))

(define (srfi-what . numbers)
  (filter (lambda (srfi) (member (srfi-number srfi) numbers))
          (srfi-list)))

(add-command!
 "what"
 '(number ...)
 "Display a one-line summmary of the given SRFI <number>s."
 1
 #f
 (lambda numbers
   (write-srfi-list
    (apply srfi-what (map parse-srfi-number numbers)))))
