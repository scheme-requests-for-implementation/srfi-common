(define-library (srfi-tools legal)
  (import (scheme base)
          (srfi-tools private string)
          (srfi-tools private command))
  (export srfi-license-lines
          srfi-license-string)
  (begin

    (define mit-license-boilerplate
      '(""
        "Permission is hereby granted, free of charge, to any person obtaining"
        "a copy of this software and associated documentation files (the"
        "\"Software\"), to deal in the Software without restriction, including"
        "without limitation the rights to use, copy, modify, merge, publish,"
        "distribute, sublicense, and/or sell copies of the Software, and to"
        "permit persons to whom the Software is furnished to do so, subject to"
        "the following conditions:"
        ""
        "The above copyright notice and this permission notice (including the"
	" next paragraph) shall be included in all copies or substantial"
	"portions of the Software."
        ""
        "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,"
        "EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF"
        "MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND"
        "NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE"
        "LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION"
        "OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION"
        "WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."))

    (define (srfi-license-lines who)
      (append (list (string-append "Copyright (C) " who "."))
              mit-license-boilerplate))

    (define (srfi-license-string who)
      (string-join (append (srfi-license-lines who) (list ""))
                   "\n"))

    (define-command (generate-license who)
      "Generate the standard MIT License and copyright message."
      (write-string (srfi-license-string who)))))
