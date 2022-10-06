(define-library (srfi-tools private string)
  (export ascii-alphabetic?
          ascii-numeric?
          string-fold
          string-index
          string-contains
          string-prefix?
          string-suffix?
          string-split
          string-join
          string-join-english)
  (import (scheme base)
          (srfi-tools private list))
  (cond-expand
   ((library (srfi 130))
    (import (srfi 130)))
   (else
    (import (srfi 13))))
  (begin

    ;; Subst of SRFI 175.
    (define (ascii-alphabetic? char)
      (or (char<=? #\A char #\Z)
          (char<=? #\a char #\z)))

    ;; Subst of SRFI 175.
    (define (ascii-numeric? char)
      (char<=? #\0 char #\9))

    ;; Subset of SRFI 140.
    (define (string-split char str)
      (define (maybe-add a b parts)
        (if (= a b) parts (cons (substring str a b) parts)))
      (let ((n (string-length str)))
        (let loop ((a 0) (b 0) (parts '()))
          (if (< b n)
              (if (not (char=? char (string-ref str b)))
                  (loop a (+ b 1) parts)
                  (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
              (reverse (maybe-add a b parts))))))

    (define (string-join-english string-list)
      "Return a string constructed by appending the elements of
`string-list', separating them with \", \" except for the last pair,
which should be separated by \" and\" if there are only two elements
and \", and\" otherwise."
      (cond ((null? string-list) "")
            ((null? (cdr string-list)) (car string-list))
            ((null? (cddr string-list))
             (string-append (car string-list)
                            " and "
                            (cadr string-list)))
            (else
             (let ((output (open-output-string)))
               (let next ((remaining string-list))
                 (write-string (car remaining) output)
                 (cond ((null? (cddr remaining))
                        (write-string ", and " output)
                        (write-string (cadr remaining) output))
                       (else (write-string ", " output)
                             (next (cdr remaining)))))
               (get-output-string output)))))))
