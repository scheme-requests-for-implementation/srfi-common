(define-library (srfi-tools private os)
  (export rename-file
          create-directory
          ensure-directory
          directory-files
          run-program
          run-program/get-output-string
          run-program/file-to-file

          ensure-directories-exist
          directory-from-envar
          directory-files
          user-home-dir
          make-temp-file-name)
  (import (scheme base)
          (scheme file)
          (scheme process-context)
          (srfi-tools private list)
          (srfi-tools private string)
          (srfi-tools private path)
          (srfi-tools private port)
          (srfi-tools private sysdep))
  (begin

    (define (ensure-directories-exist dir)
      (let loop ((i 0))
        (cond ((= i (string-length dir))
               (ensure-directory dir))
              ((char=? #\/ (string-ref dir i))
               (ensure-directory (string-copy dir 0 (+ i 1)))
               (loop (+ i 1)))
              (else
               (loop (+ i 1))))))

    (define (directory-from-envar name)
      (let ((dir (get-environment-variable name)))
        (cond ((or (not dir) (zero? (string-length dir)))
               #f)
              ((not (string-prefix? "/" dir))
               (error "Not an absolute pathname" name dir))
              (else
               dir))))

    (define (user-home-dir)
      (or (directory-from-envar "HOME")
          (error "User home directory not known")))

    (define (make-temp-file-name)
      (path-append (user-home-dir) ".srfi-temp"))))
