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
          string-join-english
          url-hexify-string)
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
               (get-output-string output)))))

    ;; From Emacs Lisp.
    (define (url-hexify-string str)
      (define safe (map char->integer (string->list "-./_")))
      (define (safe-byte? byte)
        (and (< byte #x80)
             (let ((char (integer->char byte)))
               (or (ascii-alphabetic? char)
                   (ascii-numeric? char)
                   (member char safe)))))
      (define (write-byte-safely byte)
        (cond ((safe-byte? byte)
               (write-char (integer->char byte)))
              (else
               (write-string "%")
               (write-string
                (string-downcase
                 (number->string byte 16))))))
      (let ((bytes (string->utf8 str)))
        (call-with-port (open-output-string)
          (lambda (out)
            (parameterize ((current-output-port out))
              (let loop ((i 0))
                (if (= i (bytevector-length bytes))
                    (get-output-string out)
                    (let ((byte (bytevector-u8-ref bytes i)))
                      (write-byte-safely byte)
                      (loop (+ i 1))))))))))))
