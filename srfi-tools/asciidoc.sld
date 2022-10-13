(define-library (srfi-tools asciidoc)
  (export srfi-from-asciidoc)
  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (scheme file)
          (scheme write)
          (srfi-tools private list)
          (srfi-tools private string)
          (srfi-tools private command)
          (srfi-tools private port)
          (srfi-tools private os)
          (srfi-tools private sxml)
          (srfi-tools private html-parser)
          (srfi-tools private html-writer))
  (begin

    (define (sxml-cleanup elem)
      (let-values (((name attrs body) (parse-tag elem)))
        (case name
          ((#f)
           elem)
          ((head)
           (make-tag name #f
                     (map sxml-cleanup
                          (remove-subtags
                           (flatten-subtags body 'meta)
                           'meta 'link 'style))))
          (else
           (make-tag name
                     (and attrs
                          (filter (lambda (attr) (eqv? 'href (car attr)))
                                  attrs))
                     (map sxml-cleanup (remove-subtags
                                        (flatten-subtags
                                         (remove-subtag-ids body "footer")
                                         'div)
                                        'div
                                        'colgroup)))))))

    (define (srfi-from-asciidoc asciidoc-file)
      (unless (string-suffix? ".adoc" asciidoc-file)
        (error "Input file name extension is not .adoc"))
      (let ((html-file (string-append
                        (substring asciidoc-file 0
                                   (- (string-length asciidoc-file)
                                      (string-length ".adoc")))
                        ".html")))
        (for-each display (list "Converting " asciidoc-file " to " html-file))
        (newline)
        (let ((asciidoc (with-input-from-file asciidoc-file read-all-chars)))
          (let ((html (run-program/get-output-string
                       (list "asciidoctor"
                             "-b" "xhtml5"
                             "-o" "-"
                             "--"
                             asciidoc-file))))
            (let ((sxml (html->sxml (open-input-string html))))
              (set! sxml (sxml-cleanup (find-html-tag sxml)))
              (set! html (with-output-to-string
                           (lambda ()
                             (write-string "<!doctype html>")
                             (sxml-display-as-html sxml))))
              ;; (set! html (run-pipe html "tidy" "-q" "--tidy-mark" "no" "-indent"))
              (call-with-output-file html-file
                (lambda (out) (write-string html out))))))))

    ;; This converts a SRFI written in AsciiDoc into HTML. (Note that
    ;; HTML is the required submittion format.) This command is useful
    ;; for the initial conversion, but afterwards it's easier for all
    ;; concerned to edit the HTML and throw away the original AsciiDoc
    ;; sources.
    (define-command (from-asciidoc asciidoc-file)
      (srfi-from-asciidoc asciidoc-file))))
