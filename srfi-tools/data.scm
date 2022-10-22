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

(define (read-srfi-data pathname)
  (map alist->srfi (with-input-from-file pathname read-all)))

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
          (begin (set! srfis (read-srfi-data (srfi-data-file)))
                 srfis)))))

(define (srfi-by-number num)
  (or (find (lambda (srfi) (= num (srfi/number srfi)))
            (srfi-list))
      (error "No such SRFI" num)))

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

(define (srfi-draft? srfi)
  (eqv? (srfi-status srfi) 'draft))

(define (srfi-final? srfi)
  (eqv? (srfi-status srfi) 'final))

(define (srfi-date-of-last-update srfi)
  (or (srfi-done-date srfi)
      (srfi-draft-date srfi)))

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

(define (srfi-data)
  (with-input-from-file (srfi-data-file) read-all))

(define-command (data)
  "Display the SRFI database, which is an S-expression."
  (dump-file (srfi-data-file))
  (newline))

(define-command (list)
  "Display a list of all the SRFIs."
  (write-srfi-list (srfi-list)))

(define (srfi-tail)
  (take-right (srfi-list) 10))

(define-command (tail)
  "Display a list of the most recent ten SRFIs."
  (write-srfi-list (srfi-tail)))

(define (srfi-drafts)
  (filter srfi-draft? (srfi-list)))

(define-command (drafts)
  "Display a list of all the draft SRFIs."
  (write-custom-srfi-list (srfi-drafts) "since" srfi-draft-date))

(define (srfi-by-author name)
  (filter (lambda (srfi)
            (any (lambda (author)
                   (string-ci=? name (srfi-author-name author)))
                 (srfi/authors srfi)))
          (srfi-list)))

(define-command (by-author name)
  "Display a list of all the SRFIs by authors with <name> in their names."
  (write-srfi-list (srfi-by-author name)))

(define (srfi-search query)
  (let ((query (string-downcase query)))
    (filter (lambda (srfi)
              (string-contains (string-downcase (srfi-title srfi))
                               query))
            (srfi-list))))

(define-command (search query)
  "Display a list of all the SRFIs whose titles contain <query>."
  (write-srfi-list (srfi-search query)))
