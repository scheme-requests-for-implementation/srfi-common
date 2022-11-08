(define-library (srfi-tools checklink)
  (import (scheme base)
          (scheme write)

          (srfi-tools private command)
          (srfi-tools private os)

          (srfi-tools path))
  (include "checklink.scm"))