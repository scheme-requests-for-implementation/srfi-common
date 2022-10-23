(define-library (srfi-tools private string-shim)

  ;; Scheme implementations do not agree on whether they should ship
  ;; SRFI 13 or SRFI 130. 13 is more widespread and has a simpler API,
  ;; so let's use it. The following code emulates 13 using 130 on
  ;; implementations where only 130 is available.

  (export string-null?
          string-every
          string-any
          string-join
          string-tabulate
          string-take
          string-take-right
          string-drop
          string-drop-right
          string-pad
          string-pad-right
          string-trim
          string-trim-right
          string-trim-both
          string-prefix-length
          string-suffix-length
          string-prefix?
          string-suffix?
          string-index
          string-index-right
          string-skip
          string-skip-right
          string-contains
          string-concatenate
          string-fold
          string-fold-right
          string-filter)

  (import (scheme base))

  (cond-expand
   ((and (library (srfi 130))
         (not (library (srfi 13))))
    (import (except (srfi 130)
                    string-contains
                    string-contains-right
                    string-index
                    string-index-right
                    string-skip
                    string-skip-right)
            (prefix (only (srfi 130)
                          string-contains
                          string-contains-right
                          string-index
                          string-index-right
                          string-skip
                          string-skip-right)
                    cursor:)))
   (else
    (import (srfi 13))))

  (cond-expand
   ((and (library (srfi 130))
         (not (library (srfi 13))))
    (begin

      (define (fail-false s curs)
        (and curs (string-cursor->index s curs)))

      (define (fail-end s curs)
        (and (not (string-cursor=? curs (string-cursor-end s)))
             (string-cursor->index s curs)))

      (define (successor s curs)
        (and (not (string-cursor=? curs (string-cursor-start s)))
             (- (string-cursor->index s curs) 1)))

      ;;

      (define (string-contains s1 s2 . opts)
        (fail-false s1 (apply cursor:string-contains s1 s2 opts)))

      (define (string-contains-right s1 s2 . opts)
        (fail-false s1 (apply cursor:string-contains s1 s2 opts)))

      (define (string-index s pred . opts)
        (fail-end s (apply cursor:string-index s pred opts)))

      (define (string-skip s pred . opts)
        (fail-end s (apply cursor:string-skip s pred opts)))

      (define (string-index-right s pred . opts)
        (successor s (apply cursor:string-index-right s pred opts)))

      (define (string-skip-right s pred . opts)
        (successor s (apply cursor:string-skip-right s pred opts)))))
   (else)))
