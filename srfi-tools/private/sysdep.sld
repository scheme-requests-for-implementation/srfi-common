(define-library (srfi-tools private sysdep)
  (export rename-file
          create-directory
          ensure-directory
          directory-files
          with-current-directory
          run-program
          run-program/get-boolean
          run-program/get-output-string
          run-program/file-to-file)
  (cond-expand
   (chibi
    (include-library-declarations "sysdep.chibi.scm"))
   (chicken
    (import (scheme base)
            (prefix (only (chicken file)
                          create-directory
                          directory
                          rename-file)
                    chicken:)
            (prefix (only (chicken process-context)
                          change-directory
                          current-directory)
                    chicken:)
            (prefix (only (scsh-process)
                          run
                          run/port
                          run/string)
                    scsh-process:))
    (include "sysdep.chicken.scm"))
   (gauche
    (include-library-declarations "sysdep.gauche.scm"))))
