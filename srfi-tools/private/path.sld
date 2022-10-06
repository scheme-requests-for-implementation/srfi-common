(define-library (srfi-tools private path)
  (export with-trailing-slash
          path-append)
  (import (scheme base)
          (scheme char)
          (scheme process-context)
          (srfi-tools private list)
          (srfi-tools private string))
  (begin

    (define (with-trailing-slash str)
      (let loop ((n (string-length str)))
        (if (and (> n 1) (char=? #\/ (string-ref str (- n 1))))
            (loop (- n 1))
            (string-copy str 0 n))))

    (define path-append
      (lambda args
        (if (null? args)
            ""
            (let loop ((path (first args))
                       (args (rest args)))
              (if (null? args)
                  path
                  (loop (if (string-suffix? "/" path)
                            (string-append path (first args))
                            (string-append path "/" (first args)))
                        (rest args)))))))))
