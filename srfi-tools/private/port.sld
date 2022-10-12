(define-library (srfi-tools private port)
  (export read-all
          read-all-chars
	  read-entire-file
          with-output-to-string
          write-line
          written
          displayed
          disp
          edisp
          display-two-column-table
          copy-binary-port)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (scheme write)
          (srfi-tools private list)
          (srfi-tools private string))
  (begin

    (define (read-all)
      (let loop ((accumulator '()))
        (let ((form (read)))
          (if (eof-object? form)
              (reverse accumulator)
              (loop (cons form accumulator))))))

    (define (read-all-chars)
      (let loop ((whole "") (attempt 1024))
        (let ((part (read-string attempt)))
          (if (eof-object? part)
              whole
              (loop (string-append whole part)
                    (* attempt 2))))))

    (define (read-entire-file pathname)
      (with-input-from-file pathname read-all-chars))

    (define (with-output-to-string thunk)
      (call-with-port (open-output-string)
        (lambda (port)
          (parameterize ((current-output-port port))
            (thunk))
          (get-output-string port))))

    (define (written obj)
      (with-output-to-string (lambda () (write obj))))

    (define (displayed obj)
      (with-output-to-string (lambda () (display obj))))

    (define (write-line string)
      (write-string string)
      (newline))

    (define disp
      (lambda args
        (for-each display args)
        (newline)))

    (define edisp
      (lambda args
        (parameterize ((current-output-port (current-error-port)))
          (apply disp args))
        (flush-output-port (current-error-port))))

    (define (display-two-column-table alist)
      (let* ((cars (map displayed (map car alist)))
             (cdrs (map displayed (map cdr alist)))
             (width (fold max 0 (map string-length cars))))
        (for-each (lambda (car cdr)
                    (let ((padding (- width (string-length car))))
                      (write-string car)
                      (write-string (make-string (+ padding 2) #\space))
                      (write-string cdr)
                      (newline)))
                  cars
                  cdrs)))

    (define (copy-binary-port input-port output-port)
      (let ((buffer (make-bytevector (* 100 1024))))
        (let loop ()
          (let ((n (read-bytevector! buffer input-port)))
            (let ((n (if (eof-object? n) 0 n)))
              (unless (zero? n)
                (write-bytevector buffer output-port 0 n)
                (loop)))))))))
