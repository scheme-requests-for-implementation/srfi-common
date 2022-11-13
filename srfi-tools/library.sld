(define-library (srfi-tools library)
  (export srfi-map-library-names
          srfi-library-names
          srfi-r6rs-imports
          srfi-library-names-sxml
          srfi-generate-library-names)
  (import (scheme base)

          (srfi-tools private list)
          (srfi-tools private format)
          (srfi-tools private path)
          (srfi-tools private port)
          (srfi-tools private html-writer)
          (srfi-tools private command)
          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools url))
  (begin

    (define (srfi-map-library-names proc)
      (filter-map (lambda (srfi)
                    (let ((name (srfi-library-name srfi)))
                      (and name (proc (srfi-number srfi) name))))
                  (srfi-list)))

    (define (srfi-library-names)
      (srfi-map-library-names cons))

    (define-command (library-names)
      "List library names (SRFI 97 and beyond)."
      (display-two-column-table (srfi-library-names)))

    (define (r6rs-import num name)
      ;; Return a string instead of an S-expression because symbols
      ;; starting with a colon are handled inconsistently by Scheme
      ;; implementations. Some of them escape such symbols to avoid
      ;; confusing them with keywords. R6RS standard syntax does not
      ;; support those escapes.
      (format "(import (srfi :~a ~a))" num name))

    (define (r7rs-import num name)
      ;; R7RS may add the names at some point.
      (format "(import (srfi ~a))" num))

    (define (srfi-r6rs-imports)
      (srfi-map-library-names r6rs-import))

    (define-command (r6rs-imports)
      "List R6RS (import ...) for each SRFI (SRFI 97 and beyond)."
      (for-each write-line (srfi-r6rs-imports)))

    (define (srfi-library-names-sxml)
      (let ((title "SRFI library names"))
        `(html
          (head
           (title ,title)
           (style "table, th, td { border: 1px solid black; }"))
          (body
           (h1 ,title)
           (p "The initial batch of library names was coined in "
              (a (@ (href ,(srfi-landing-url 97)))
                 "SRFI 97") ". "
                 "The up to date list is kept in "
                 (code "srfi-data.scm"))
           (table
            (tr (th "")
                (th "R6RS")
                (th "R7RS"))
            ,@(srfi-map-library-names
               (lambda (num name)
                 `(tr (td (a (@ (href ,(srfi-landing-url num))
                                (title ,(srfi-title num)))
                             "SRFI " ,(number->string num)))
                      (td (code ,(r6rs-import num name)))
                      (td (code ,(r7rs-import num name)))))))))))

    (define (srfi-generate-library-names)
      (let ((file (path-append (srfi-common-dir) "library-names.html"))
            (sxml (srfi-library-names-sxml)))
        (disp "Writing " file)
        (write-html-file file sxml)))

    (define-command (generate-library-names)
      "Write web page of library names (SRFI 97 and beyond)."
      (srfi-generate-library-names))))
