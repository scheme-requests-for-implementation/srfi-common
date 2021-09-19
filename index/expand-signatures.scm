;;;; Expand signatures

;; Often, macro and procedure signatures in SRFIs are expressed like this:
;;
;;   (string-every pred string [start end])
;;
;; This usually means that either `start', or `start' and `end', or neither can
;; be supplied as arguments.  I want to expand the above notation to the
;; following notation, which is unambiguous:

;;   (string-every pred string)
;;   (string-every pred string start)
;;   (string-every pred string start end)

;; The procedure `expand-signature', below, performs that expansion given a
;; slightly different input form:

;;   (string-every pred string #(start end))

;; Note that this assumes that closing never appear anywhere except at the end
;; of the containing list.  It also assumes that [foo bar] has the same meaning
;; as [foo [bar]].  (Sometimes, people define [foo bar] to mean that either both
;; symbols or neither must appear.)

(define (expand-signature x)
  (cond ((null? x) '(()))
	((pair? x)
	 (cond ((vector? (car x))
		(assert (null? (cdr x)))
		(let next-tail ((y (vector->list (car x))))
		  (if (null? y)
		      '(())
		      (cons '()
			    (let ((heads (expand-signature (car y))))
			      (append-map (lambda (t)
					    (map (lambda (h) (cons h t))
						 heads))
					  (next-tail (cdr y))))))))
	       (else
		(append-map (lambda (h)
			      (map (lambda (t) (cons h t))
				   (expand-signature (cdr x))))
			    (expand-signature (car x))))))
	((vector? x) (vector->list x))
	(else (list x))))

(define (expand-index-entry e)
  (append-map (lambda (x)
		(if (and (pair? x) (eq? (car x) 'signature))
		    (expand-signature x)
		    (list x)))
	      e))

(define (expand-index pathname)
  (with-input-from-file pathname
    (lambda ()
      (let next-signature ()
	(let ((input (read)))
	  (unless (eof-object? input)
	    (pp (expand-index-entry input))
	    (next-signature)))))))