(define-library (srfi-tools git)
  (export srfi-git-https-url
          srfi-git-ssh-url
          srfi-clone
          srfi-pull)
  (import (scheme base)
          (scheme file)
          (srfi-tools private command)
          (srfi-tools private format)
          (srfi-tools private port)
          (srfi-tools private os)
          (srfi-tools data)
          (srfi-tools path)
          (srfi-tools url)
          (srfi-tools github))
  (begin

    (define srfi-git-https-url srfi-github-https-url)

    (define-command (git-https-url num)
      "Display the Git HTTPS URL for SRFI <num>."
      (write-line-about-srfi srfi-git-https-url num))

    (define srfi-git-ssh-url srfi-github-ssh-url)

    (define-command (git-ssh-url num)
      "Display the Git SSH URL for SRFI <num>."
      (write-line-about-srfi srfi-git-ssh-url num))

    (define (in-git-dir?)
      (run-program/get-boolean '("git" "rev-parse" "--git-dir")))

    (define (srfi-clone num)
      (let ((dir (srfi-dir num)))
        (ensure-directory dir)
        (with-current-directory
         dir
         (lambda ()
           (when (in-git-dir?)
             (error "That SRFI is already under git version control."))
           (run-program '("git" "init"))
           (run-program `("git" "remote" "add" "origin"
                          ,(srfi-git-https-url num)))
           (run-program '("git" "add" "."))
           (run-program '("git" "pull"
                          "--autostash"
                          "--set-upstream"
                          "origin" "master"))
           (write-line dir)))))

    (define-command (clone num)
      "Pull SRFI <num> from its git version control repository."
      (srfi-clone (parse-srfi-number num)))

    (define (srfi-pull . numbers)
      (for-each
       (lambda (num)
         (let ((dir (srfi-dir num)))
           (disp dir)
           (with-current-directory
            dir
            (lambda ()
              (unless (in-git-dir?)
                (error "SRFI is not under git version control." num))
              (run-program '("git" "pull"))))
           (disp)))
       numbers))

    (add-command!
     "pull"
     '(number ...)
     "Run `git pull` for the given SRFI <number>s."
     1
     #f
     (lambda numbers
       (apply srfi-pull (map parse-srfi-number numbers))))))
