(define-library (srfi-tools private file)
  (export with-input-from-binary-file
          with-output-to-binary-file
          read-text-file
          write-text-file
          dump-file)
  (import (scheme base)
          (scheme file)
          (srfi-tools private port)
          (srfi-tools private os))
  (begin

    (define (with-input-from-binary-file filename thunk)
      (call-with-port (open-binary-input-file filename)
        (lambda (port)
          (parameterize ((current-input-port port))
            (thunk)))))

    (define (with-output-to-binary-file filename thunk)
      (let ((newfilename (string-append filename ".new")))
        (call-with-port (open-binary-output-file newfilename)
          (lambda (port)
            (parameterize ((current-output-port port))
              (thunk))))
        (rename-file newfilename filename)))

    (define (read-text-file filename)
      (with-input-from-file filename read-all-chars))

    (define (write-text-file filename string)
      (with-output-to-file filename
        (lambda () (write-string string))))

    (define (dump-file filename)
      (call-with-port (open-binary-input-file filename)
        (lambda (input)
          (copy-binary-port input (current-output-port)))))))
