(define-library (srfi-tools help)
  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
	  (srfi-tools private command)
          (srfi-tools private list)
          (srfi-tools private string)
          (srfi-tools private port))
  (begin

    (define (indent-lines string)
      (let ((lines (string-split #\newline string)))
	(with-output-to-string
	  (lambda ()
	    (for-each (lambda (l)
			(write-string "  ")
			(write-string l)
			(newline))
		      lines)))))

    (define-command (commands)
      "Display a list of subommands, with help."
      (for-each (lambda (command)
                  (display (command-name command))
		  (for-each (lambda (a)
			      (display " ")
			      (display a))
			    (command-arg-names command))
		  (newline)
		  (display (indent-lines (command-help command))))
                (command-list)))

    (define (complete index args)
      (cond ((= index 1)
             (map command-name (command-list)))
            (else
             ;; We cannot yet complete subcommand arguments.
             '())))

    (define completion-scripts
      '(("bash"

         "_srfi_complete() {\n"
         " IFS=$'\\n' COMPREPLY=($(compgen"
         " -W"
         " \"$(srfi complete \"$COMP_CWORD\" \"${COMP_WORDS[@]}\")\""
         " --"
         " \"${COMP_WORDS[COMP_CWORD]}\""
         "))\n"
         "}\n"
         "complete -F _srfi_complete srfi\n")))

    (define (completion-script-for-shell shell)
      (let ((entry (assoc shell completion-scripts)))
        (and entry (string-join (rest entry) ""))))

    (add-command!
     "complete" '(name) "Display completions of a srfi subcommand." 1 #f
     (lambda (what . args)
       (cond ((string->number what)
              => (lambda (index)
                   (for-each write-line (complete index args))))
             ((completion-script-for-shell what)
              => (lambda (script)
                   (if (zero? (length args))
                       (write-string script)
                       (error "Too many args"))))
             (else
              (error "What to complete?" what)))))

    (define help-lines
      '("The `srfi` command presents a number of tools to help"
        "read and write Scheme Requests for Implementation."
        ""
        "Type `srfi commands` for a list of available commands."
        "Each tool can also be imported as a Scheme library."
        ""
        "Please direct comments about the tools to the"
        "srfi-discuss mailing list or to the SRFI Editor."
        ""
        "Mailing list instructions and archives can be found at"
        "<https://srfi.schemers.org/srfi-list-subscribe.html>."
        ""
        "The SRFI website is at <https://srfi.schemers.org/>."))

    (define help
      (case-lambda
       (()
        (for-each write-line help-lines))
       ((command)
        (write-line (command-help (command-by-name command))))))

    (add-command!
     "help"
     '(command)
     "Display general help, or help for <command>."
     0 1 help)

    ;; Emulate all the common help flags to be user friendly.

    (define-command (|--help|)
      "Display help."
      (help))

    (define-command (|-help|)
      "Display help."
      (help))

    (define-command (|-h|)
      "Display help."
      (help))))
