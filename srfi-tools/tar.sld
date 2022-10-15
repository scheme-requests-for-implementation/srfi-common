(define-library (srfi-tools tar)
  (import (scheme base)
          (scheme file)
          (srfi-tools private list)
          (srfi-tools private string)
          (srfi-tools private port)
          (srfi-tools private path)
          (srfi-tools private file)
          (srfi-tools private tar)
          (srfi-tools private os)
          (srfi-tools private external)
          (srfi-tools private command)
          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools url))
  (export srfi-download-tar
          srfi-unpack-tar)
  (begin

    (define (srfi-download-tar)
      (let* ((basename "srfi.tgz")
             (filename (path-append (srfi-home-dir) basename)))
        (ensure-directories-exist (srfi-home-dir))
        (download-url-into-file (srfi-tar-gz-url) filename)))

    (define-command (download-tar)
      "Download the srfi.tgz file."
      (srfi-download-tar))

    (define (srfi-unpack-matching-files match?)
      (let ((tgz (path-append (srfi-home-dir) "srfi.tgz"))
            (tar (make-temp-file-name)))
        (gzip-decompress-file tgz tar)
        (with-input-from-binary-file
         tar
         (lambda ()
           (let loop ()
             (let ((entry (tar-read-entry)))
               (unless (eof-object? entry)
                 (let* ((raw-path (tar-entry-path entry))
                        (path-parts (drop (string-split #\/ raw-path) 1))
                        (path (apply path-append path-parts)))
                   (cond ((and (tar-entry-file? entry)
                               (not (null? path-parts))
                               (match? path-parts))
                          (disp "Unpacking " path)
                          (ensure-directories-exist
                           (apply path-append
                                  (srfi-home-dir)
                                  (drop-right path-parts 1)))
                          (with-output-to-binary-file
                           (path-append (srfi-home-dir) path)
                           (lambda ()
                             (write-bytevector (tar-read-data entry)))))
                         (else
                          (tar-skip-data entry))))
                 (loop))))))))

    (define (srfi-unpack-tar . numbers)
      (let ((stems (map srfi-num-stem numbers)))
        (srfi-unpack-matching-files
         (lambda (path-parts)
           (or (null? stems)
               (any (lambda (stem)
                      (equal? stem (first path-parts)))
                    stems))))))

    (define-command (unpack-tar num)
      "Unpack the srfi.tgz file."
      (srfi-unpack-tar (parse-srfi-number num)))

    (define (srfi-unpack-tar-html)
      (srfi-unpack-matching-files
       (lambda (path-parts)
         (string-suffix? ".html" (last path-parts)))))

    (define-command (unpack-tar-html)
      "Unpack the .html files from the srfi.tgz file."
      (srfi-unpack-tar-html))))
