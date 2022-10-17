(define-library (srfi-tools pre)
  (export pre-srfi-home-dir
          pre-srfi-dir
          pre-srfi-list
          pre-srfi-new)
  (import (scheme base)
          (scheme file)
          (srfi-tools private list)
          (srfi-tools private path)
          (srfi-tools private port)
          (srfi-tools private format)
          (srfi-tools private os)
          (srfi-tools private command)
          (srfi-tools data)
          (srfi-tools legal)
          (srfi-tools path))
  (begin

    ;; Tools for dealing with unofficial "pre-SRFI" documents that
    ;; have not yet been submitted as SRFI drafts but may later be.

    ;; Several SRFI authors write and even collaborate on pre-SRFIs,
    ;; so why not store them alongside proper SRFIs.

    (define (pre-srfi-home-dir)
      (path-append (srfi-home-dir) "pre-srfi"))

    (define-command (pre-home-dir)
      "Display the directory of unofficial \"pre-SRFI\" documents."
      (write-line (pre-srfi-home-dir)))

    (define (pre-srfi-dir name)
      (path-append (pre-srfi-home-dir) name))

    (define-command (pre-dir name)
      "Display the directory of unofficial \"pre-SRFI\" document NAME."
      (write-line (pre-srfi-dir name)))

    (define (pre-srfi-list)
      (directory-files (pre-srfi-home-dir)))

    (define-command (pre-list)
      "Display the list of unofficial \"pre-SRFI\" documents."
      (for-each disp (pre-srfi-list)))

    (define makefile-lines
      '("all:"
        "\tpandoc --from=gfm README.md -o README.html"
        "\tpandoc --standalone README.html -o README.pdf"))

    (define readme-lines
      (append
       (interpose
        ""
        '("# SRFI nnn: Title"
          "by Firstname Lastname"
          "## Status"
          "Early Draft"
          "## Abstract"
          "## Issues"
          "## Rationale"
          "### Survey of prior art"
          "## Specification"
          "## Examples"
          "## Implementation"
          "## Acknowledgements"
          "## References"
          "## Copyright"))
       '("")
       (srfi-license-lines "Firstname Lastname (20XY)")))

    (define (prepare-file filename lines)
      (unless (file-exists? filename)
        (with-output-to-file filename
          (lambda () (for-each write-line lines)))))

    (define (pre-srfi-new name)
      (let ((dir (pre-srfi-dir name)))
        (ensure-directory dir)
        (prepare-file (path-append dir "Makefile") makefile-lines)
        (prepare-file (path-append dir "README.md") readme-lines)
        (disp dir)))

    (define-command (pre-new name)
      "Create new unofficial \"pre-SRFI\" document NAME."
      (pre-srfi-new name))))
