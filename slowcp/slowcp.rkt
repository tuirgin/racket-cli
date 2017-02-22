#!/usr/bin/env racket
#lang racket/base
(require racket/cmdline
         racket/file
         racket/path
         srfi/19)


(define copy-delay (make-parameter 5))
(define destination (make-parameter "."))

(define slow-copy
  (command-line
   #:program "slowcp"
   #:once-each
   [("-t") time "the duration of <time> in seconds between each copy; default: 5"
           (copy-delay (string->number time))]
   [("-d") dest "the directory to which <files> will be copied; default: ."
           (destination dest)]
   #:ps "\nPURPOSE: Copy <files> at a limited rate"
   #:args files
   files))

(define (main)
  (make-directory* (destination))

  (define (date-stamp d/t) (date->string d/t "~Y-~m-~d ~H:~M:~S"))
  (define success-count (make-parameter 0))
  (define fail-count (make-parameter 0))

  (for ([from-path slow-copy])
    (define to-path (build-path (destination) (file-name-from-path from-path)))
    (with-handlers
      ([exn:fail:filesystem?
        (λ (exn)
          (fprintf (current-error-port) "~a ERROR: Unable to copy ~a to ~a\n"
                   (date-stamp (current-date)) from-path to-path)
          (fail-count (add1 (fail-count))))])
      (copy-file from-path to-path)
      (printf "~a Copied ~a to ~a\n"
              (date-stamp (current-date)) from-path to-path)
      (success-count (add1 (success-count)))
      (sleep (copy-delay))))

  (printf "~a\n" (make-string 72 #\=))
  (printf "Copying completed: ~a file(s) copied to ~a.\n" (success-count) (destination))
  (printf "                   ~a file(s) failed to copy.\n" (fail-count)))

(main)
