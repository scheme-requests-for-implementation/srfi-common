(define-library (srfi-tools private html-parser)
  (export make-html-parser
          html->sxml
          html->sxml/srfi
          html-parser-position
          html-strip
          read-html-file
          read-html-token)
  (import (except (scheme base) read-char)
          (rename (only (scheme base) read-char)
                  (read-char %base-read-char))
          (scheme char)
          (scheme cxr)
          (scheme file)
          (scheme write))
  (include "chibi-html-parser.scm")
  (begin

    (define srfi-entities
      (append
       '(;; Dashes and dots
         ("bull"   . "\x2022;")
         ("hellip" . "\x2026;")
         ("mdash"  . "\x2014;")
         ("middot" . "\x00B7;")
         ("ndash"  . "\x2013;")
         ;; Quotation marks
         ("laquo"  . "\x00AB;")
         ("ldquo"  . "\x201C;")
         ("lsquo"  . "\x2018;")
         ("raquo"  . "\x00BB;")
         ("rdquo"  . "\x201D;")
         ("rsquo"  . "\x2019;")
         ;; Arrows
         ("larr"   . "\x2190;")
         ("rarr"   . "\x2192;")
         ;; Math
         ("divide" . "\x00F7;")
         ("minus"  . "\x2212;")
         ("plusmn" . "\x00B1;")
         ("times"  . "\x00D7;")
         ;; Symbols
         ("copy"   . "\x00A9;")
         ("deg"    . "\x00B0;")
         ("micro"  . "\x00B5;")
         ("para"   . "\x00B6;")
         ("reg"    . "\x00AE;")
         ("sect"   . "\x00A7;")
         ("trade"  . "\x2122;"))
       *default-entities*))

    ;; We define our own version of html->sxml because Chibi's version doesn't
    ;; recognize important entities.  We extend the list with the ones in
    ;; srfi-entities above.
    (define html->sxml/srfi
      (let ((parse
             (make-html-parser
              'start:  (lambda (tag attrs seed virtual?) '())
              'end:    (lambda (tag attrs parent-seed seed virtual?)
                         `((,tag ,@(if (pair? attrs)
                                       `((@ ,@attrs) ,@(reverse seed))
                                       (reverse seed)))
                           ,@parent-seed))
              'decl:    (lambda (tag attrs seed)
                          `((*DECL* ,tag ,@attrs) ,@seed))
              'process: (lambda (attrs seed) `((*PI* ,@attrs) ,@seed))
              'comment: (lambda (text seed) `((*COMMENT* ,text) ,@seed))
              'text:    (lambda (text seed) (cons text seed))
              'entities: srfi-entities)))
        (lambda o
          (cons '*TOP* (reverse (apply parse '() o))))))

    (define (read-html-file file)
      (with-input-from-file file html->sxml/srfi))))