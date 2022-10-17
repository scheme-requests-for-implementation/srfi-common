(define-library (srfi-tools private list)
  (export any
          append-map
          append-reverse
          cons*
          drop
          drop-right
          take
          take-right
          first second third
          filter
          filter-map
          find
          fold
          remove
          last

          list-sort

          rest
          pair<?
          reverse-pair<?
          sorted-insert-unique
          interpose)

  (import (scheme base)
          (only (srfi 1)
                any append-map append-reverse cons* drop drop-right
                take take-right
                first second third
                filter filter-map find fold remove last))

  (cond-expand
   ((or chibi cyclone gauche sagittarius)
    (import (srfi 132)))
   (chicken
    (import (chicken sort)))
   (kawa
    (import (srfi 95))))

  (cond-expand
   ((or chicken kawa)
    (begin
      (define (list-sort less? list) (sort list less?))))
   (else))

  (begin

    ;; From Common Lisp.
    (define rest cdr)

    ;; Sort by car. Where equal, sort by cdr.
    (define (pair<? car<? cdr<?)
      (lambda (a b)
        (or (car<? (car a) (car b))
            (and (not (car<? (car b) (car a)))
                 (cdr<? (cdr a) (cdr b))))))

    ;; Sort by cdr. Where equal, sort by car.
    (define (reverse-pair<? car<? cdr<?)
      (lambda (a b)
        (or (cdr<? (cdr a) (cdr b))
            (and (not (cdr<? (cdr b) (cdr a)))
                 (car<? (car a) (car b))))))

    (define (sorted-insert-unique < elem list)
      (let loop ((before '()) (tail list))
        (cond ((or (not (pair? tail))
                   (< elem (first tail)))
               (append-reverse before (cons elem tail)))
              ((< (first tail) elem)
               (loop (cons (first tail) before)
                     (rest tail)))
              (else
               list))))

    ;; From Clojure.
    (define (interpose delimiter list)
      (if (null? list)
          '()
          (let loop ((new '()) (list list))
            (if (null? (rest list))
                (reverse (cons (first list) new))
                (loop (cons* delimiter (first list) new)
                      (rest list))))))))
