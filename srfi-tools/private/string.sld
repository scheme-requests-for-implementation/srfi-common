(define-library (srfi-tools private string)
  (export ascii-alphabetic?
          ascii-numeric?
          ascii-alphanumeric?)
  (export string-every
          string-any
          string-join
          string-tabulate
          string-take
          string-take-right
          string-drop
          string-drop-right
          string-pad
          string-pad-right
          string-trim
          string-trim-right
          string-trim-both
          string-prefix-length
          string-suffix-length
          string-prefix?
          string-suffix?
          string-index
          string-index-right
          string-skip
          string-skip-right
          string-contains
          string-concatenate
          string-fold
          string-fold-right
          string-filter)
  (export string-split
          string-join-english
          string->slug
          unique-string-accumulator
          url-hexify-string)
  (import (scheme base)
          (scheme char)
          (srfi-tools private list)
          (srfi-tools private string-shim))
  (begin

    ;; Subst of SRFI 175.
    (define (ascii-alphabetic? char)
      (or (char<=? #\A char #\Z)
          (char<=? #\a char #\z)))

    ;; Subst of SRFI 175.
    (define (ascii-numeric? char)
      (char<=? #\0 char #\9))

    (define (ascii-alphanumeric? char)
      (or (ascii-alphabetic? char)
          (ascii-numeric? char)))

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

    (define (with-output-to-string thunk)
      (call-with-port (open-output-string)
        (lambda (port)
          (parameterize ((current-output-port port))
            (thunk))
          (get-output-string port))))

    (define (string-join-english string-list)
      "Return a string constructed by appending the elements of
`string-list', separating them with \", \" except for the last pair,
which should be separated by \" and\" if there are only two elements
and \", and\" otherwise."
      (case (length string-list)
        ((0) "")
        ((1) (first string-list))
        ((2) (string-append (first string-list) " and " (second string-list)))
        (else (with-output-to-string
                (lambda ()
                  (let next ((remaining string-list))
                    (write-string (first remaining))
                    (cond ((not (null? (cddr remaining)))
                           (write-string ", ")
                           (next (cdr remaining)))
                          (else
                           (write-string ", and ")
                           (write-string (second remaining))))))))))

    ;; "Foo bar/baz!" -> "foo-bar-baz"
    (define (string->slug str)
      (let ((str (string-downcase str)))
        (with-output-to-string
          (lambda ()
            (let loop ((empty? #t) (want-dash? #f) (i 0))
              (when (< i (string-length str))
                (let ((char (string-ref str i)))
                  (cond ((ascii-alphanumeric? char)
                         (when (and want-dash? (not empty?))
                           (write-string "-"))
                         (write-char char)
                         (loop #f #f (+ i 1)))
                        (else
                         (loop empty? #t (+ i 1)))))))))))

    (define (unique-string-accumulator)
      (let ((used '()))
        (lambda (str)
          (if (eof-object? str)
              (reverse used)
              (let loop ((candidate str) (next 2))
                (cond ((member candidate used)
                       (loop (string-append str "-" (number->string next))
                             (+ next 1)))
                      (else
                       (set! used (cons candidate used))
                       candidate)))))))

    ;; From Emacs Lisp.
    (define (url-hexify-string str)
      (define safe (map char->integer (string->list "-./_")))
      (define (safe-byte? byte)
        (and (< byte #x80)
             (let ((char (integer->char byte)))
               (or (ascii-alphanumeric? char)
                   (member char safe)))))
      (define (write-byte-safely byte)
        (cond ((safe-byte? byte)
               (write-char (integer->char byte)))
              (else
               (write-string (if (< byte 16) "%0" "%"))
               (write-string (string-downcase (number->string byte 16))))))
      (let ((bytes (string->utf8 str)))
        (with-output-to-string
          (lambda ()
            (let loop ((i 0))
              (when (< i (bytevector-length bytes))
                (let ((byte (bytevector-u8-ref bytes i)))
                  (write-byte-safely byte)
                  (loop (+ i 1)))))))))))
