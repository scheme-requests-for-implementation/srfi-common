(define-library (srfi-tools private tar)
  (export tar-read-entry
          tar-entry-file?
          tar-entry-size
          tar-entry-path
          tar-read-data
          tar-skip-data)
  (import (scheme base))
  (begin

    (define (read-exactly-n-bytes n)
      (let ((bytes (read-bytevector n)))
        (let ((bytes (if (eof-object? bytes) (bytevector) bytes)))
          (if (= n (bytevector-length bytes))
              bytes
              (error "Short read")))))

    (define (bytevector-every match? bytes)
      (let loop ((n (bytevector-length bytes)))
        (or (zero? n)
            (and (match? (bytevector-u8-ref bytes (- n 1)))
                 (loop (- n 1))))))

    (define (drop-right drop? bytes start length)
      (let loop ((n (+ start length)))
        (if (and (< start n) (drop? (bytevector-u8-ref bytes (- n 1))))
            (loop (- n 1))
            n)))

    (define (ustar-string-ref header start length)
      (utf8->string header start (drop-right zero? header start length)))

    (define (ustar-number-ref header start length)
      (let ((string (ustar-string-ref header start length)))
        (or (string->number string 8)
            (error "Not an octal number" string))))

    ;;

    (define (tar-read-entry)
      (let ((header (read-exactly-n-bytes 512)))
        (if (bytevector-every zero? header)
            (eof-object)
            header)))

    (define (tar-entry-file? header)
      (= (bytevector-u8-ref header 156)
         (char->integer #\0)))

    (define (tar-entry-size header)
      (ustar-number-ref header 124 12))

    (define (tar-entry-path header)
      (let ((prefix (ustar-string-ref header 345 155)))
        (string-append prefix (ustar-string-ref header 0 100))))

    (define (tar-read-data header)
      (let* ((size (tar-entry-size header))
             (bytes (read-exactly-n-bytes size))
             (residue (truncate-remainder size 512))
             (padding (if (zero? residue) 0 (- 512 residue))))
        (read-exactly-n-bytes padding)
        bytes))

    (define (tar-skip-data header)
      (tar-read-data header)
      (values))))
