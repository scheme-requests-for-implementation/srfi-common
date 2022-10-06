(define-library (srfi-tools private sxml)
  (export sxml-for-each
          sxml-attributes
          sxml-body
          sxml-body-as-string)
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
            (sxml-body elem)))))
