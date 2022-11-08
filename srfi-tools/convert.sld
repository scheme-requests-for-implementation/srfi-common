(define-library (srfi-tools convert)
  (export srfi-from-asciidoc
          srfi-from-markdown)
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

    ;; Convert a SRFI draft from another markup language to HTML.
    ;;
    ;; Note that HTML is the required submission format. These
    ;; commands are useful for the initial conversion, but afterwards
    ;; it's easier for all concerned to edit the HTML and throw away
    ;; the original source.

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

    (define (source-file->html-file source-file source-extension)
      (unless (string-suffix? source-extension source-file)
        (error "Source file name extension is not" source-extension))
      (let ((html-file (string-append
                        (substring source-file 0
                                   (- (string-length source-file)
                                      (string-length source-extension)))
                        ".html")))
        (edisp "Converting " source-file " to " html-file)
        html-file))

    (define (emit html-file sxml)
      (write-html-file html-file sxml)
      (edisp "Running HTML Tidy on " html-file)
      ;; `run-program/get-boolean` avoids raising an exception when
      ;; the exit code is 1. HTML Tidy uses that exit code when there
      ;; are warnings. The warnings are harmless in this case.
      (run-program/get-boolean
       (list "tidy"
             "-q"
             "-indent"
             "-modify"
             "--tidy-mark" "no"
             "--" html-file))
      (values))

    (define (srfi-from-asciidoc source-file)
      (let* ((html-file (source-file->html-file source-file ".adoc"))
             (html (run-program/get-output-string
                    (list "asciidoctor"
                          "-b" "xhtml5"
                          "-o" "-"
                          "--" source-file)))
             (sxml (call-with-port (open-input-string html) html->sxml))
             (sxml (sxml-cleanup (find-html-tag sxml))))
        (emit html-file sxml)))

    (define (srfi-from-pandoc source-file source-extension)
      (let* ((html-file (source-file->html-file source-file source-extension))
             (html (run-program/get-output-string
                    (list "pandoc"
                          "-t" "html5"
                          "--" source-file)))
             (sxml (call-with-port (open-input-string html) html->sxml)))
        (emit html-file sxml)))

    (define (srfi-from-markdown source-file)
      (srfi-from-pandoc source-file ".md"))

    (define-command (from-asciidoc source-file)
      "Convert SRFI draft written in AsciiDoc into HTML."
      (srfi-from-asciidoc source-file))

    (define-command (from-markdown source-file)
      "Convert SRFI draft written in Markdown into HTML."
      (srfi-from-markdown source-file))))
