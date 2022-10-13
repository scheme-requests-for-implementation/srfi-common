(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scheme write)
        (prefix (chibi filesystem) chibi:)
        (prefix (chibi io) chibi:)
        (prefix (chibi process) chibi:)
        (prefix (only (chibi) port-fileno) chibi:)
        (srfi-tools private list)
        (srfi-tools private port))

(begin

  (define (rename-file oldpath newpath)
    (chibi:rename-file oldpath newpath))

  (define (create-directory path)
    (chibi:create-directory path))

  (define (ensure-directory path)
    (unless (file-exists? path)
      (chibi:create-directory path)))

  (define (directory-files path)
    (remove (lambda (name) (member name '("." "..")))
            (chibi:directory-files path)))

  ;; TODO: Copied from Chibi's <lib/chibi/process.scm>.
  (define (execute-returned cmd)
    ;; we only arrive here if execute fails
    (let ((err (current-error-port)))
      (cond
       ((output-port? err)
        (display "ERROR: couldn't execute: " (current-error-port))
        (write cmd (current-error-port))
        (newline (current-error-port))))
      (exit 1)))

  (define (run-program command+args)
    (chibi:system command+args)
    (values))

  (define (run-program/file-to-file command+args
                                    input-filename
                                    output-filename)
    (call-with-port (open-binary-input-file input-filename)
      (lambda (input-port)
        (call-with-port (open-binary-output-file output-filename)
          (lambda (output-port)
            (let ((ifd (chibi:port-fileno input-port))
                  (ofd (chibi:port-fileno output-port))
                  (pid (chibi:fork)))
              (cond
               ((not pid)
                (error "couldn't fork"))
               ((zero? pid)
                ;; child
                (chibi:duplicate-file-descriptor-to ifd 0)
                (chibi:duplicate-file-descriptor-to ofd 1)
                (chibi:close-file-descriptor ifd)
                (chibi:close-file-descriptor ofd)
                (chibi:execute (first command+args) command+args)
                (execute-returned command+args))
               (else
                ;; parent
                (let ((status (second (chibi:waitpid pid 0))))
                  (unless (eqv? status 0)
                    (error "Command failed")))))))))))

  (define (run-program/get-output-string command+args)
    (chibi:process->string command+args)))
