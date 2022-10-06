(define (rename-file oldpath newpath)
  (chicken:rename-file oldpath newpath #t))

(define (create-directory path)
  (chicken:create-directory path))

(define (ensure-directory path)
  (chicken:create-directory path #t))

(define (directory-files path)
  (chicken:directory path))

(define (handle-exit command+args thunk)
  (let-values (((number normal-exit? pid) (thunk)))
    (unless normal-exit?
      (error "Subprocess exited abnormally" command+args))
    (unless (equal? 0 number)
      (error "Subprocess exited with status" number command+args))))

(define (run-program command+args)
  (handle-exit
   command+args
   (lambda ()
     (scsh-process:run
      (,(car command+args) ,@(cdr command+args))))))

(define (run-program/get-output-string command+args)
  (let ((result
         (scsh-process:run/string
          (,(car command+args) ,@(cdr command+args)))))
    (if (string? result)
        result
        (error "Subprocess exited abnormally" command+args))))

(define (run-program/file-to-file command+args
                                  input-filename
                                  output-filename)
  (handle-exit
   command+args
   (lambda ()
     (scsh-process:run
      (,(car command+args) ,@(cdr command+args))
      (< ,input-filename)
      (> ,output-filename)))))
