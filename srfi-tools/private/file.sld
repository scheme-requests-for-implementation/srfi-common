(define-library (srfi-tools private file)
  (export file-exists?
          with-input-from-binary-file
          with-input-from-text-file
          with-output-to-binary-file
          with-output-to-text-file
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

    (define with-input-from-text-file with-input-from-file)

    (define (with-output-to-*-file open file thunk)
      (let ((new-file (string-append file ".new")))
        (call-with-port (open new-file)
          (lambda (port)
            (parameterize ((current-output-port port))
              (thunk))))
        (rename-file new-file file)))

    (define (with-output-to-binary-file file thunk)
      (with-output-to-*-file open-binary-output-file file thunk))

    (define (with-output-to-text-file file thunk)
      (with-output-to-*-file open-output-file file thunk))

    (define (read-text-file filename)
      (with-input-from-file filename read-all-chars))

    (define (write-text-file file string)
      (with-output-to-text-file file (lambda () (write-string string))))

    (define (dump-file filename)
      (call-with-port (open-binary-input-file filename)
        (lambda (input)
          (copy-binary-port input (current-output-port)))))))
