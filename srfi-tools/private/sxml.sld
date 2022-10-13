(define-library (srfi-tools private sxml)
  (export sxml-for-each
          sxml-attributes
          sxml-body
          sxml-body-as-string
          find-html-tag
          tag-with-attrs?
          parse-tag
          make-tag
          remove-subtags
          remove-subtag-ids
          flatten-subtags)
  (import (scheme base)
          (scheme cxr)
          (srfi-tools private list))
  (begin

    (define (sxml-for-each proc elem)
      (let walk ((elem elem))
        (cond ((not (pair? elem)) '())
              ((equal? '@ (car elem)) '())
              (else (proc elem)
                    (for-each walk (cdr elem))))))

    (define (sxml-attributes elem)
      (if (and (pair? elem)
               (pair? (cdr elem))
               (pair? (cadr elem))
               (equal? '@ (caadr elem)))
          (cdadr elem)
          '()))

    (define (sxml-body elem)
      (cond ((not (pair? (cdr elem)))
             '())
            ((and (pair? (cadr elem))
                  (eqv? '@ (caadr elem)))
             (cddr elem))
            (else
             (cdr elem))))

    (define (sxml-body-as-string elem)
      (fold (lambda (part string)
              (string-append
               string
               (cond ((string? part)
                      part)
                     ((pair? part)
                      (sxml-body-as-string part))
                     (else
                      (error "Unrecognized SXML" part)))))
            ""
            (sxml-body elem)))

    ;;

    (define (find-html-tag sxml)
      (and (pair? sxml)
           (or (and (pair? (car sxml))
                    (eqv? 'html (caar sxml))
                    (car sxml))
               (find-html-tag (cdr sxml)))))

    (define (tag-with-attrs? elem)
      (and (pair? (cdr elem))
           (pair? (cadr elem))
           (eqv? '@ (caadr elem))))

    (define (parse-tag elem)
      (if (or (not (pair? elem))
              (not (symbol? (car elem)))
              (eqv? '@ (car elem)))
          (values #f #f #f)
          (let ((name (car elem)))
            (set! elem (cdr elem))
            (let ((attrs (and (pair? elem)
                              (pair? (car elem))
                              (eqv? '@ (caar elem))
                              (cdar elem))))
              (when attrs (set! elem (cdr elem)))
              (let ((body elem))
                (values name attrs body))))))

    (define (make-tag name attrs body)
      (if (and attrs (not (null? attrs)))
          (cons* name (cons '@ attrs) body)
          (cons name body)))

    (define (remove-subtags body . tag-names)
      (filter (lambda (elem)
                (let-values (((name attrs body) (parse-tag elem)))
                  (not (and name (member name tag-names)))))
              body))

    (define (remove-subtag-ids body . ids)
      (filter (lambda (elem)
                (let-values (((name attrs body) (parse-tag elem)))
                  (not (and attrs (let* ((pair (assoc 'id attrs))
                                         (value (and pair (cadr pair))))
                                    (and value (member value ids)))))))
              body))

    (define (flatten-subtags body . tag-names)
      (let loop ((oldbody body) (newbody '()))
        (if (null? oldbody)
            newbody
            (let-values (((name attrs elembody) (parse-tag (car oldbody))))
              (if (and name (member name tag-names))
                  (loop (append elembody (cdr oldbody))
                        (append newbody (list (make-tag name attrs '()))))
                  (loop (cdr oldbody)
                        (append newbody (list (car oldbody)))))))))))
