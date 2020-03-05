;;;; "srfi.svg" graph generation

;; This code runs on MIT Scheme.

;; To use this, evaluate these expressions in MIT Scheme:

;;   (define srfi-data (load "srfi-common/admin/srfi-data"))
;;   (load "srfi-common/admin/srfis")
;;   (load "srfi-common/admin/date")
;;   (load "srfi-common/admin/graph")
;;   (write-srfi-data)

;; then run this command in Bash:

;;   (cd $ss/srfi-common/admin/ && gnuplot -e 'load "srfi.gnuplot"')

;; The "srfi.svg" graph will be placed in "/tmp/".

;; TODO(arthur): Refactor this to use SRFIs where possible.

(declare (usual-integrations))

(define (count-duplicates grouped-elements same?)
  "Return an alist mapping the elements of the list
`grouped-elements', which are grouped so that pairs of elements that
satisfy `same?' appear next to each other in the list, to the number
of occurrences of each element."
  (if (null? grouped-elements)
      '()
      (let next-element ((accumulator '())
			 (count 1)
			 (grouped-elements (cdr grouped-elements))
			 (previous-element (car grouped-elements)))
	(if (null? grouped-elements)
	    (reverse!
	     (cons (cons previous-element count)
		   accumulator))
	    (let ((first-element (car grouped-elements)))
	      (if (same? first-element previous-element)
		  (next-element accumulator
				(1+ count)
				(cdr grouped-elements)
				previous-element)
		  (next-element (cons (cons previous-element count)
				      accumulator)
				1
				(cdr grouped-elements)
				first-element)))))))

(define (split-string-transform string separator-regexp transform)
  "Return a list of the results of running `transform' on substrings
of `string', in order, having divided `string' wherever
`separator-regexp' matched."
  (let ((size (string-length string)))
    (let next-index ((index 0))
      (if (= index size)
	  '()
	  (let ((registers (re-substring-search-forward separator-regexp
							string
							index
							size)))
	    (if registers
		(cons (transform
		       (substring string
				  index
				  (re-match-start-index 0 registers)))
		      (next-index (re-match-end-index 0 registers)))
		(list (transform (substring string index size)))))))))

(define (ymd->iso-date-string ymd)
  (define (pad digits num)
    (let* ((num (number->string num))
           (pad (max 0 (- digits (string-length num)))))
      (string-append (make-string pad #\0) num)))
  (let ((y (list-ref ymd 0))
        (m (list-ref ymd 1))
        (d (list-ref ymd 2)))
    (string-append (pad 4 y) "-" (pad 2 m) "-" (pad 2 d))))

(define (parse-iso-date date-string)
  (let ((date-list
	 (split-string-transform date-string "-" string->number)))
    (make-absolute-date
     (cadr date-list)
     (caddr date-list)
     (car date-list))))

(define (srfi-dates date select)
  (fold-right
   (lambda (s accumulator)
     (if (date s)
	 (cons (parse-iso-date (date s)) accumulator)
	 accumulator))
   '()
   (filter (lambda (s) (select (srfi/status s))) srfis)))

(define (srfi-date-accumulate dates start end)
  (let ((dates (sort dates <)))
    (let next ((accumulator '())
	       (counts (count-duplicates dates =))
	       (date start))
      (cond ((= date end) (reverse accumulator))
	    ((null? counts)
	     (reverse (append (make-list (- end date) (car accumulator))
			      accumulator)))
	    ((= date (caar counts))
	     (next (cons (+ (cdar counts)
			    (if (null? accumulator)
				0
				(car accumulator)))
			 accumulator)
		   (cdr counts)
		   (1+ date)))
	    (else (next (cons (if (null? accumulator)
				  0
				  (car accumulator)) accumulator)
			counts
			(1+ date)))))))

(define (write-srfi-counts counts pathname start-date)
  (with-output-to-file pathname
    (lambda ()
      (do ((i 0 (1+ i))
	   (counts counts (cdr counts)))
	  ((null? counts))
	(let ((c (car counts))
	      (ymd (absolute->year-month-day (+ i start-date))))
          (format #t "~A ~A~%" (ymd->iso-date-string ymd) c))))))

(define (write-srfi-data)
  (let* ((done-dates (srfi-dates srfi/done-date (lambda (s) #t)))
	 (start-dates (srfi-dates srfi/draft-date (lambda (s) #t)))
	 (final-dates
	  (srfi-dates srfi/done-date (lambda (s) (eq? 'final s))))
	 (withdrawn-dates
	  (srfi-dates srfi/done-date (lambda (s) (eq? 'withdrawn s))))
	 (min-date (reduce-left min 0 start-dates))
	 (max-date (1+ (max (reduce-left max 0 done-dates)
			    (reduce-left max 0 start-dates)))))
    (write-srfi-counts
     (srfi-date-accumulate start-dates min-date max-date)
     "/tmp/srfi-all.dat"
     min-date)
    (write-srfi-counts
     (srfi-date-accumulate final-dates min-date max-date)
     "/tmp/srfi-final.dat"
     min-date)
    (write-srfi-counts
     (srfi-date-accumulate withdrawn-dates min-date max-date)
     "/tmp/srfi-withdrawn.dat"
     min-date)))
