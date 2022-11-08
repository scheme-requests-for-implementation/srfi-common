(define-library (srfi-tools private external)
  (export desktop-open
          edit-text-file
          browse-url-with
          browse-url
          render-web-page-as-plain-text
          run-pager-on-url
          download-url-into-file
          gzip-decompress-file)
  (import (scheme base)
          (scheme file)
          (scheme process-context)
          (srfi-tools private string)
          (srfi-tools private file)
          (srfi-tools private port)
          (srfi-tools private error)
          (srfi-tools private os))
  (begin

    (define (desktop-open filename)
      (assert-sane-filename filename)
      ;; TODO: If there is no "open" command, try "xdg-open".
      (run-program (list "open" filename)))

    (define (assert-sane-filename filename)
      (when (string-prefix? "-" filename)
        (error "Filename starts with '-'" filename)))

    (define (assert-sane-url url)
      (when (string-prefix? "-" url)
        (error "URL starts with '-'" url)))

    (define (edit-text-file filename)
      (assert-sane-filename filename)
      (let ((editor (get-environment-variable "EDITOR")))
        (unless editor
          (user-error "EDITOR environment variable not set."))
        (run-program (list editor filename))))

    (define (browse-url-with browser url)
      (assert-sane-url url)
      (run-program (list browser url)))

    (define (browse-url url)
      (let ((browser (get-environment-variable "BROWSER")))
        (unless browser
          (user-error "BROWSER environment variable not set."))
        (when (or (string-contains browser ":")
                  (string-contains browser "%"))
          ;; Weird features from <http://www.catb.org/esr/BROWSER/index.html>.
          ;; These features are not supported by PAGER or EDITOR.
          ;; It's doubtful anyone is using them anymore.
          (user-error "BROWSER environment variable contains ':' or '%'."))
        (browse-url-with browser url)))

    (define (render-web-page-as-plain-text url)
      (assert-sane-url url)
      (run-program/get-output-string (list "lynx" "-dump" url)))

    (define (run-pager-on-url url)
      (assert-sane-url url)
      (let ((pager (get-environment-variable "PAGER")))
        (unless pager
          (user-error "PAGER environment variable not set."))
        (let ((rendered (render-web-page-as-plain-text url))
              (temp-file (make-temp-file-name)))
          (write-text-file temp-file rendered)
          (run-program (list pager temp-file)))))

    (define (download-url-into-file url filename)
      (assert-sane-url url)
      (assert-sane-filename filename)
      (let ((newfilename (string-append filename ".new")))
        (disp "Downloading " url)
        (run-program (list "curl"
                           "--location"
                           "--fail"
                           "--show-error"
                           "--output" newfilename
                           "--"
                           url))
        (rename-file newfilename filename)))

    (define (gzip-decompress-file compressed-filename filename)
      (run-program/file-to-file (list "gunzip")
                                compressed-filename
                                filename))))
