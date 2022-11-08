(define-library (srfi-tools library)
  (export srfi-library-names-sxml
          srfi-generate-library-names)
  (import (scheme base)
          (scheme file)
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
            ,@(filter-map
               (lambda (srfi)
                 (let ((num (srfi-number srfi))
                       (name (srfi-library-name srfi)))
                   (and name
                        `(tr (td (a (@ (href ,(srfi-landing-url num))
                                       (title ,(srfi-title srfi)))
                                    "SRFI " ,(number->string num)))
                             (td (code ,(format "(import (srfi :~a ~a))"
                                                num name)))
                             (td (code ,(format "(import (srfi ~a))"
                                                num)))))))
               (srfi-list)))))))

    (define (srfi-generate-library-names)
      (let ((file (path-append (srfi-common-dir) "library-names.html"))
            (sxml (srfi-library-names-sxml)))
        (disp "Writing " file)
        (write-html-file file sxml)))

    (define-command (generate-library-names)
      "Display the SRFI 97 library names."
      (srfi-generate-library-names))))
