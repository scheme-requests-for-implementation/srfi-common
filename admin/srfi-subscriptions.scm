(define srfi-subscriptions-loader
  (make-csv-loader
   "~/backups/simplelists/subscriptions/srfi.schemers.org.csv"
   (lambda vs
     (cons (caddr vs)
	   (let ((tail (drop vs 8)))
	     (if (null? tail)
		 '()
		 (split-string-trim (car tail) ",")))))))

(define srfi-subscriptions (cdr (srfi-subscriptions-loader)))