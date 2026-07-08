(define-library (srfi-tools toc)
  (export srfi-generate-toc
          srfi-update-toc)
  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (scheme process-context)
          (scheme write)

          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools private command)
          (srfi-tools private file)
          (srfi-tools private html-parser)
          (srfi-tools private html-writer)
          (srfi-tools private list)
          (srfi-tools private port)
          (srfi-tools private string)
          (srfi-tools private sxml))
  (begin

    (define (heading-level tag)
      (case tag
        ((h1) 1)
        ((h2) 2)
        ((h3) 3)
        ((h4) 4)
        ((h5) 5)
        ((h6) 6)
        (else #f)))

    (define-record-type <heading>
      (make-heading level text id slug)
      heading?
      (level heading-level*)
      (text  heading-text)
      (id    heading-id)
      (slug  heading-slug))

    ;; Find the name attribute of an <a name="..."> child, if any.
    (define (heading-anchor-name elem)
      (let loop ((body (sxml-body elem)))
        (cond ((null? body) #f)
              ((and (pair? (car body))
                    (eqv? 'a (caar body)))
               (let* ((attrs (sxml-attributes (car body)))
                      (name-pair (assoc 'name attrs)))
                 (if name-pair
                     (cadr name-pair)
                     (loop (cdr body)))))
              (else (loop (cdr body))))))

    (define (headings elem)
      (if (not (pair? elem))
          '()
          (let ((level (heading-level (car elem))))
            (if level
                (let* ((attrs (sxml-attributes elem))
                       (id-pair (assoc 'id attrs))
                       (id (or (and id-pair (cadr id-pair))
                               (heading-anchor-name elem))))
                  (list (make-heading level
                                      (sxml-body-as-string elem)
                                      id
				      #f)))
                (append-map headings (sxml-body elem))))))

    (define (headings->tree/values hs)
      (let deeper ((level #f) (hs hs))
        (let same-level ((acc '()) (hs hs))
          (if (null? hs) (values acc '())
              (let* ((h (car hs))
                     (l (heading-level* h)))
                (cond ((not level) (deeper l hs))
                      ((< l level) (values acc hs))
                      (else (let-values (((subtree hs)
                                          (deeper (+ l 1) (cdr hs))))
                              (same-level
                               (append acc
                                       (list (cons (cons (heading-text h)
                                                         (heading-id h))
                                                   subtree)))
                               hs)))))))))

    (define (headings->tree hs)
      (let-values (((subtree hs) (headings->tree/values hs)))
        subtree))

    (define (write-html-toc indent items)
      (disp indent "<h2 id=\"toc\">Table of contents</h2>")
      (let ((uniq (unique-string-accumulator)))
        (define (link-html title-and-id)
          (let ((title (car title-and-id))
                (id (cdr title-and-id)))
            (string-append "<a href=\"#"
                           (or id (uniq (string->slug title)))
                           "\">"
                           (html-escape title)
                           "</a>")))
        (let display-list ((indent indent) (items items))
          (disp indent "<ul>")
          (let ((indent (string-append indent "  ")))
            (for-each (lambda (item)
                        (let* ((title-and-id (car item))
                               (link (link-html title-and-id)))
                          (cond ((null? (cdr item))
                                 (disp indent "<li>" link "</li>"))
                                (else
                                 (disp indent "<li>")
                                 (let ((indent (string-append indent "  ")))
                                   (disp indent link)
                                   (display-list indent (cdr item)))
                                 (disp indent "</li>")))))
                      items))
          (disp indent "</ul>"))
        (newline)
        (for-each (lambda (slug) (disp " id=\"" slug "\""))
                  (uniq (eof-object)))))

    (define (wanted-heading? h)
      (and (<= 2 (heading-level* h) 4)
           (not (member (string-downcase (heading-text h))
                        '("abstract"
			  "author"
			  "issues"
			  "status"
			  "title"
                          "table of contents")))))

    (define (srfi-generate-toc html-file)
      (let* ((sxml (read-html-file html-file))
             (hdgs (filter wanted-heading? (headings sxml))))
        (write-html-toc "  " (headings->tree hdgs))))

    (define-command (generate-toc num)
      "Display an HTML table of contents for SRFI <num>."
      (srfi-generate-toc (srfi-html-file (parse-srfi-number num))))

    ;;; update-toc: Insert TOC, and add missing heading IDs.

    ;; Assign slugs to wanted headings that lack an existing id.
    (define (heading-plan sxml)
      (let ((hdgs (headings sxml))
            (uniq (unique-string-accumulator)))
        (map (lambda (h)
               (let ((slug (and (wanted-heading? h)
                                (not (heading-id h))
                                (uniq (string->slug (heading-text h))))))
                 (if slug
                     (make-heading (heading-level* h) (heading-text h)
                                   (heading-id h) slug)
                     h)))
             hdgs)))

    ;; Extract wanted headings with effective ids for headings->tree.
    (define (plan->toc-headings plan)
      (filter-map (lambda (h)
                    (and (wanted-heading? h)
                         (make-heading (heading-level* h)
                                       (heading-text h)
                                       (or (heading-id h)
                                           (heading-slug h))
                                       #f)))
                  plan))

    (define (find-toc-index plan)
      (let loop ((i 0) (entries plan))
        (cond ((null? entries) #f)
              ((string=? "table of contents"
                         (string-downcase
                          (heading-text (car entries))))
               i)
              (else (loop (+ i 1) (cdr entries))))))

    (define (find-first-wanted-index plan)
      (let loop ((i 0) (entries plan))
        (cond ((null? entries) #f)
              ((wanted-heading? (car entries)) i)
              (else (loop (+ i 1) (cdr entries))))))

    ;; Tokenizer pass: find the character offset of every heading
    ;; start tag in the raw HTML string.
    (define (heading-positions raw-html)
      (let ((in (open-input-string raw-html)))
        (parameterize ((html-parser-position 0))
          (let loop ((acc '()))
            (let* ((start-pos (html-parser-position))
                   (tok (read-html-token in)))
              (case (car tok)
                ((eof) (reverse acc))
                ((start start/end)
                 (if (heading-level (cadr tok))
                     (loop (cons start-pos acc))
                     (loop acc)))
                (else (loop acc))))))))

    ;; Return the position of the start of the line containing pos.
    (define (line-start str pos)
      (let loop ((i (- pos 1)))
        (cond ((< i 0) 0)
              ((char=? (string-ref str i) #\newline) (+ i 1))
              (else (loop (- i 1))))))

    ;; Return the whitespace from the start of the line to pos.
    (define (line-indent str pos)
      (substring str (line-start str pos) pos))

    ;; Generate TOC HTML as a string.  All heading ids must already be
    ;; assigned (no unique-string-accumulator needed here).
    (define (generate-toc-html indent tree)
      (with-output-to-string
        (lambda ()
          (disp indent "<h2 id=\"toc\">Table of contents</h2>")
          (let display-list ((indent indent) (items tree))
            (disp indent "<ul>")
            (let ((indent (string-append indent "  ")))
              (for-each
               (lambda (item)
                 (let* ((title-and-id (car item))
                        (title (car title-and-id))
                        (id (cdr title-and-id))
                        (link (string-append
                               "<a href=\"#" id "\">"
                               (html-escape title) "</a>")))
                   (cond ((null? (cdr item))
                          (disp indent "<li>" link "</li>"))
                         (else
                          (disp indent "<li>")
                          (let ((indent (string-append indent "  ")))
                            (disp indent link)
                            (display-list indent (cdr item)))
                          (disp indent "</li>")))))
               items))
            (disp indent "</ul>")))))

    ;; Apply a list of (start end . replacement) edits to a string.
    ;; Edits are sorted back-to-front so earlier offsets stay valid.
    (define (apply-edits str edits)
      (let ((sorted (list-sort (lambda (a b) (> (car a) (car b)))
                               edits)))
        (fold (lambda (edit str)
                (string-append (substring str 0 (car edit))
                               (cddr edit)
                               (substring str (cadr edit)
                                          (string-length str))))
              str
              sorted)))

    ;; Build an edit that inserts id="slug" after the tag name.
    (define (id-insertion-edit h pos)
      (let ((slug (heading-slug h)))
        (and slug
             (cons* (+ pos 3) (+ pos 3)
                    (string-append " id=\"" slug "\"")))))

    ;; Build an edit that inserts or replaces the TOC section.
    (define (toc-section-edit raw positions index tree insert?)
      (let* ((pos (list-ref positions index))
             (indent (line-indent raw pos))
             (toc (generate-toc-html indent tree))
             (start (line-start raw pos))
             (end (if insert?
                      start
                      (line-start
                       raw
                       (if (< (+ index 1) (length positions))
                           (list-ref positions (+ index 1))
                           (string-length raw))))))
        (list (cons* start end (string-append toc "\n")))))

    (define (srfi-update-toc html-file)
      (let* ((raw (read-text-file html-file))
             (sxml (html->sxml raw))
             (plan (heading-plan sxml))
             (positions (heading-positions raw))
             (tree (headings->tree (plan->toc-headings plan))))
        (when (not (= (length plan) (length positions)))
          (error "Heading count mismatch between SXML and tokenizer"))
        (if (null? tree)
            (disp "No headings for table of contents.")
            (let* ((toc-index (find-toc-index plan))
                   (first-wanted (find-first-wanted-index plan))
                   (id-edits (filter-map id-insertion-edit plan positions))
                   (toc-edits
                    (cond (toc-index
                           (toc-section-edit raw
					     positions
					     toc-index
                                             tree
					     #f))
                          (first-wanted
                           (toc-section-edit raw
					     positions
					     first-wanted
                                             tree
					     #t))
                          (else '())))
                   (result (apply-edits raw (append id-edits toc-edits))))
              (write-text-file html-file result)))))

    (define-command (update-toc num)
      "Update table of contents and heading ids in SRFI <num>."
      (srfi-update-toc (srfi-html-file (parse-srfi-number num))))))
