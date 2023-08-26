;;;; "srfi.svg" chart generation

;; TODO: Make this write temporary files to a temporary directory, not "/tmp/".

(define (srfi-dates key include?)
  (filter-map (lambda (s)
		(and (include? (srfi-status s))
		     (cond ((key s) => iso-date->date)
			   (else #false))))
	      (srfi-list)))

(define (write-srfi-counts dates pathname today)
  (let* ((start (date->julian (iso-date->date "1998-01-01"))) ; year SRFI started
	 (counts (make-vector (+ 1 (- today start)) 0)))
    (for-each (lambda (d)
		(let ((i (- (date->julian d) start)))
		  (vector-set! counts
			       i
			       (+ 1 (vector-ref counts i)))))
	      dates)
    (with-output-to-text-file pathname
      (lambda ()
	(do ((julian start (+ 1 julian))
	     (i 0 (+ 1 i)))
	    ((= julian today))
	  (write-string (date->iso-date (julian-day->date julian)))
	  (write-string " ")
	  (write-string (number->string (vector-ref counts i)))
	  (newline))))))

(define (write-final-srfi-durations)
  (define (iso->julian iso)
    (date->julian-day (iso-date->date iso)))
  (with-output-to-text-file "/tmp/srfi-durations.txt"
    (lambda ()
      (for-each (lambda (s)
		  (write (srfi-number s))
		  (write-string " ")
		  (write
		   (if (eq? (srfi-status s) 'final)
		       (- (iso->julian (srfi-done-date s))
			  (iso->julian (srfi-draft-date s)))
		       0))
		  (write-string "\n"))
		(srfi-list)))))

(define (write-srfi-data today)
  (let ((start-dates (srfi-dates srfi-draft-date (lambda (status) #true)))
	(final-dates
	 (srfi-dates srfi-done-date (lambda (status) (eq? 'final status))))
	(withdrawn-dates
	 (srfi-dates srfi-done-date (lambda (status) (eq? 'withdrawn status))))
	(now (date->julian today)))
    (write-final-srfi-durations)
    (write-srfi-counts start-dates "/tmp/srfi-all-counts.txt" now)
    (write-srfi-counts final-dates "/tmp/srfi-final-counts.txt" now)
    (write-srfi-counts withdrawn-dates "/tmp/srfi-withdrawn-counts.txt" now)))

(define (gnuplot-commands today)
  (let* ((original (read-text-file
		    (path-append (srfi-common-dir) "admin" "srfi.gnuplot")))
	 (end (string-contains original "-12-31"))
	 (start (- end (string-length "YYYY"))))
    (string-append (string-copy original 0 start)
		   (number->string (date-year today))
		   (string-copy original end))))

(define (srfi-generate-chart today)
  (write-text-file "/tmp/srfi.gnuplot" (gnuplot-commands today))
  (write-srfi-data today)
  (run-program '("gnuplot" "/tmp/srfi.gnuplot"))
  (run-program `("cp" "-p" "/tmp/srfi.svg" ,(srfi-common-dir)))
  (run-program `("gnuplot"
		 ,(path-append (srfi-common-dir) "admin" "durations.gnuplot")))
  (run-program `("cp" "-p" "/tmp/durations.svg" ,(srfi-common-dir))))

(define-command (generate-chart)
  "Generate the SRFI progress chart as an SVG file."
  (srfi-generate-chart (current-date)))