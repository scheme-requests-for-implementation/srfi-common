(define-library (srfi-tools pre)
  (export pre-srfi-home-dir
          pre-srfi-list)
  (import (scheme base)
          (srfi-tools private path)
          (srfi-tools private port)
          (srfi-tools private format)
          (srfi-tools private os)
          (srfi-tools private command)
          (srfi-tools data)
          (srfi-tools path))
  (begin

    ;; Tools for dealing with unofficial "pre-SRFI" documents that
    ;; have not yet been submitted as SRFI drafts but may later be.

    ;; Several SRFI authors write and even collaborate on pre-SRFIs,
    ;; so why not store them alongside proper SRFIs.

    (define (pre-srfi-home-dir)
      (path-append (srfi-home-dir) "pre-srfi"))

    (define-command (pre-home-dir)
      (write-line (pre-srfi-home-dir)))

    (define (pre-srfi-list)
      (directory-files (pre-srfi-home-dir)))

    (define-command (pre-list)
      (for-each disp (pre-srfi-list)))))
