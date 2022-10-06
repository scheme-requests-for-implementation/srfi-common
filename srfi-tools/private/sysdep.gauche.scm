(import (scheme base)
        (scheme file)
        (prefix (only (srfi 170)
                      create-directory
                      rename-file)
                srfi-170:)
        (prefix (only (gauche base)
                      make-keyword)
                gauche:)
        (prefix (gauche process)
                gauche:))

(begin

  (define (rename-file oldpath newpath)
    (srfi-170:rename-file oldpath newpath))

  (define (create-directory path)
    (srfi-170:create-directory path))

  (define (ensure-directory path)
    (unless (file-exists? path)
      (srfi-170:create-directory path)))

  (define (directory-files path)
    (srfi-170:directory-files path #t))

  (define (run-program command+args)
    (gauche:do-process command+args)
    (values))

  (define (run-program/file-to-file command+args
                                    input-filename
                                    output-filename)
    (gauche:do-process command+args
                       (gauche:make-keyword "input")
                       input-filename
                       (gauche:make-keyword "output")
                       output-filename))

  (define (run-program/get-output-string command+args)
    (gauche:with-input-from-process command+args read-all-chars)))
