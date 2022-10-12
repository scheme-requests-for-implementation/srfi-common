(define-library (srfi-tools html)
  (export srfi-landing-sxml
          srfi-landing-html
          srfi-sxml
          srfi-html
          srfi-text
          srfi-abstract-raw
          srfi-abstract-sxml
          srfi-abstract-html
          srfi-abstract-text
          srfi-link-html
          srfi-link-md)
  (import (scheme base)
          (scheme file)
          (srfi-tools private list)
          (srfi-tools private string)
          (srfi-tools private hash-table)
          (srfi-tools private file)
          (srfi-tools private format)
          (srfi-tools private port)
          (srfi-tools private external)
          (srfi-tools private command)
          (srfi-tools private sxml)
          (srfi-tools private html-parser)
          (srfi-tools private html-writer)
          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools url))
  (begin

    (define (srfi-landing-sxml num)
      (call-with-input-file (srfi-landing-html-file num) html->sxml))

    (define (srfi-landing-html num)
      (file-contents-as-string (srfi-landing-html-file num)))

    (define-command (landing-html num)
      (write-string-about-srfi srfi-landing-html num))

    (define (srfi-sxml num)
      (call-with-input-file (srfi-html-file num) html->sxml))

    (define (srfi-html num)
      (file-contents-as-string (srfi-html-file num)))

    (define-command (html num)
      (write-string-about-srfi srfi-html num))

    (define (srfi-text num)
      (render-web-page-as-plain-text (srfi-html-file num)))

    (define-command (text num)
      (write-string-about-srfi srfi-text num))

    (define (srfi-abstract-raw num)
      `(@raw ,(read-entire-file (srfi-abstract-html-file num))))

    (define (srfi-abstract-sxml num)
      (define skip-top cdr)
      (skip-top
       (call-with-input-file (srfi-abstract-html-file num) html->sxml)))

    (define (srfi-abstract-html num)
      (file-contents-as-string (srfi-abstract-html-file num)))

    (define-command (abstract-html num)
      (write-string (srfi-abstract-html (parse-srfi-number num))))

    (define (srfi-abstract-text num)
      (with-output-to-string
	(lambda ()
	  (sxml-display-as-text
	   (call-with-input-file (srfi-abstract-html-file num) html->sxml)))))

    (define-command (abstract-text num)
      (write-string-about-srfi srfi-abstract-text num))

    (define (srfi-link-html num)
      (format "<a href=\"~a\">SRFI ~a (~a)</a>"
              (srfi-html-url num)
              num
              (srfi-title (srfi-by-number num))))

    (define-command (link-html num)
      (disp (srfi-link-html (parse-srfi-number num))))

    (define (srfi-link-md num)
      ;; Markdown
      (format "[SRFI ~a (~a)](~a)"
              num
              (srfi-title (srfi-by-number num))
              (srfi-html-url num)))

    (define-command (link-md num)
      (disp (srfi-link-md (parse-srfi-number num))))

    (define (tag-names-fold elem merge state)
      (let do-elem ((elem elem) (state state))
        (if (not (pair? elem))
            state
            (let do-list ((elems (sxml-body elem))
                          (state (merge (car elem) state)))
              (if (null? elems)
                  state
                  (do-list (cdr elems)
                           (do-elem (car elems) state)))))))

    (define (srfi-count-html-tags nums)
      (list-sort
       (reverse-pair<? string<? >)
       (tally
        (lambda (count)
          (for-each
           (lambda (num)
             (tag-names-fold
              (srfi-sxml num)
              (lambda (tag _)
                (let ((tag (symbol->string tag)))
                  (unless (string-prefix? "*" tag)
                    (count tag))))
              #f))
           nums)))))

    (define-command (count-html-tags num)
      (display-two-column-table
       (srfi-count-html-tags (list (parse-srfi-number num)))))))
