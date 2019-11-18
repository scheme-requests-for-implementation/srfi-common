;;;; Date calculations

;; Derived from code placed in the public domain by Ozan S. Yigit.

;; This runs on MIT Scheme.

;; TODO(arthur): Make this into a SRFI.  The closest SRFI we have is
;; SRFI 19 Time Data Types and Procedures, but that doesn't support
;; date calculations.  For this code, we just need next-day.

(declare (usual-integrations))

(define (days-in-prior-months month year)
  "Return the total number of days in months prior to `month' in
`year'."
  (+ (vector-ref days-before-months (-1+ month))
     (if (and (> month 2)
	      (leap-year? year))
	 1
	 0)))

(define (make-absolute-date month day year)
  (let ((last-year (-1+ year)))
    (+ day				 ; days so far this month
       (days-in-prior-months month year) ; days in prior months this year
       (* 365 last-year)		 ; days in prior years
       (quotient last-year 4)		 ; Julian leap days in prior years
       (- (quotient last-year 100))	 ;   minus prior century years
       (quotient last-year 400))))

(define days-before-months
  (do ((accumulator '() (cons count accumulator))
       (count 0 (+ count (car days)))
       (days '(31 28 31 30 31 30 31 31 30 31 30 31)
	     (cdr days)))
      ((null? days) (list->vector (reverse accumulator)))))

(define (leap-year? year)
  (and (= (modulo year 4) 0)
       (not (let ((m (modulo year 400)))
	      (or (= m 100)
		  (= m 200)
		  (= m 300))))))

(define absolute->year
  (let ((range-start (make-absolute-date 1 1 2000)))
    (lambda (date)
      (if (<= 730120 date 766644)	; This works for 1 Jan 2000
					; through 31 Dec 2099.
	  (+ 2000
	     (quotient (* 4 (- date range-start))
		       1461))
	  (let approximate ((year (quotient date 366)))
	    (let ((next-year (1+ year)))
	      (if (>= date (make-absolute-date 1 1 next-year))
		  (approximate next-year)
		  year)))))))

(define (absolute->month date year)
  (let ((days-this-year (- date (make-absolute-date 1 1 year))))
    (if (< days-this-year 31)
	1
	(let ((leap? (leap-year? year)))
	  (cond ((and (not leap?)
		      (= days-this-year 59))
		 3)
		((< days-this-year 60) 2)
		(else ; unrolled binary search over `days-before-months'
		 (let ((offset (- days-this-year (if leap? 1 0))))
		   (if (< offset 212)
		       (if (< offset 151)
			   (if (< offset 90)
			       3
			       (if (< offset 120) 4 5))
			   (if (< offset 181) 6 7))
		       (if (< offset 304)
			   (if (< offset 243)
			       8
			       (if (< offset 273) 9 10))
			   (if (< offset 334) 11 12))))))))))

(define (absolute->year-month-day date)
  "Return the Gregorian year, month, and day corresponding to absolute
`date' as a list."
  (let* ((year (absolute->year date))
	 (month (absolute->month date year))
	 (day (1+ (- date (make-absolute-date month 1 year)))))
    (list year month day)))