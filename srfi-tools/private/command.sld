(define-library (srfi-tools private command)
  (export
   define-command
   add-command!
   command-name
   command-apply
   command-min-args
   command-max-args
   command-by-name
   command-list)
  (import (scheme base)
          (srfi-tools private list)
          (srfi-tools private string))
  (begin

    (define-record-type <command>
      (make-command name min-args max-args proc)
      command?
      (name command-name)
      (min-args command-min-args)
      (max-args command-max-args)  ; #f means unlimited.
      (proc command-proc))

    (define (command-apply command args)
      (apply (command-proc command)
             args))

    (define (command<? a b)
      (string<? (command-name a)
                (command-name b)))

    (define commands '())

    (define (name-predicate name)
      (lambda (command)
        (equal? name (command-name command))))

    (define (try-command-by-name name)
      (find (name-predicate name) commands))

    (define (command-by-name name)
      (or (try-command-by-name name)
          (error "No such command" name)))

    (define (add-command! name min-args max-args proc)
      (let ((command (make-command name min-args max-args proc)))
        (set! commands
              (list-sort command<?
                         (cons command
                               (remove (name-predicate name)
                                       commands))))))

    (define-syntax define-command
      (syntax-rules ()
        ((define-command (name args ...) body ...)
         (let ((n-args (length '(args ...))))
           (add-command! (symbol->string 'name)
                         n-args
                         n-args
                         (lambda (args ...)
                           body ...))))))

    (define (command-list)
      (remove (lambda (command)
                (string-prefix? "-" (command-name command)))
              commands))))
