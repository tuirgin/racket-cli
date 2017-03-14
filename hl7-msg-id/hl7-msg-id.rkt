#!/usr/bin/env racket
#lang racket/base

(require gregor
         racket/cmdline
         racket/file
         racket/format
         racket/string)

(module+ test
  (require rackunit))

#|

Modify HL7 MSH-10 (Message Control ID). All files provided at run time
will be given a unique message control id.

|#


;; ---------------------------------------------------------------------------------------------------
;; PARAMETERS

(define count-start (make-parameter 1))
(define count-length (make-parameter 4))
(define files (make-parameter #f))
(define id-length (make-parameter 20))
(define id-prefix (make-parameter ""))
(define id-suffix (make-parameter ""))


;; ---------------------------------------------------------------------------------------------------
;; COMMAND-LINE

;; TODO: Test-run, operate on copies, etc.

(module+ main
  (command-line
   #:program "hl7-msg-id"
   #:once-each
   [("--count-start" "-c")
    start
    ("Default: 1"
     "Specify the starting count.")
    (count-start (string->number start))]
   [("--count-width" "-d")
    count-width
    ("Default: 4"
     "Specify the character width of count; i.e. a width of 4 will yield 0001, 0002...")
    (count-length (string->number count-width))]
   [("--id-width" "-i")
    id-width
    ("Default: 20"
     "Specify the total character width of the id, inclusive of prefix, date/time
    stamp, count, and suffix.")
    (id-length (string->number id-width))]
   [("--prefix" "-p")
    prefix
    ("Specify the id prefix.")
    (id-prefix prefix)]
   [("--suffix" "-s")
    suffix
    ("Specify the id suffix.")
    (id-suffix suffix)]
   #:ps "
 !!! WARNING: This tool makes in-place file modifications. !!!

 PURPOSE: Incrementally change the message control id of specified HL7 files.

 NOTE: If the argument list is too long, use find and xargs; e.g.:
   $> find . -iname '*.hl7' -print0 | xargs -n 100 -0 hl7-msg-id"
   ""
   #:args args
   (files args))
  (process-files (files)))


;; ---------------------------------------------------------------------------------------------------
;; FUNCTIONS

;; Start processing of files
;; TODO: Migrate to one of the available for loops
(define (process-files files)
  (foldl process-file (count-start) files)
  (void))

;; FILE Number -> FILE
;; Change MSH-10 of ~file-name~ to ~id~. Return incremented ~id~.
;; TODO: Separate processing from writing and incriminating id
;;       See above, re: migrating to a for/[something] loop
(define (process-file file-name id)
  (lines->file
   (replace-msg-id file-name id)
   ;; For testing:
   ;; (string-append (regexp-replace #rx"\\.txt" file-name "-new.txt")))
   file-name)
  (printf " ~a: ~a\n" (~r id #:min-width 4) file-name)
  (add1 id))

;; FILE -> [Listof String]
;; Open and modify ~file-name~ to have an MSH-10 of ~id~.
(define (replace-msg-id file-name id)
  (define hl7 (file->lines file-name))
  (define msh-regx (pregexp "^((MSH\\|)([^|]*\\|){8})[^|]*(\\|.*)$"))
  (define msh-record (regexp-match msh-regx (car hl7)))
  (define new-msh (string-append (cadr msh-record)
                                 (msg-id id)
                                 (list-ref msh-record 4)))
  (cons new-msh (cdr hl7)))

(module+ test
  ;; TODO: Add tests for replace-msg-id
  )

;; [Listof X] -> FILE
;; Write list to file with <CR> line-endings.
(define (lines->file lines file-name)
  (display-lines-to-file lines
                         file-name
                         #:exists 'replace
                         #:separator #"\r"
                         #:mode 'text))

;; Number -> String
;; Create ID string with prefix, date/time stamp, count, suffix
(define (msg-id count)
  (define (time-string) (~t (now) "YYYYMMddHHmmssSSSSSSSSSSSSSSSSSSSSSSSSSS"))
  (~a (id-prefix)
      (substring (time-string) 0 (- (id-length)
                                    (string-length (id-prefix))
                                    (count-length)
                                    (string-length (id-suffix))))
      (~a (~r count #:min-width (count-length) #:pad-string "0")
          #:max-width (count-length)
          #:limit-prefix? #t
          #:limit-marker "")
      (id-suffix)))

(module+ test
  ;; TODO: Add more tests for boundaries, particularly where the id-length is
  ;;       less than the full length of all constituent parts.
  (test-equal? "ID width: 20" (string-length (msg-id 12345)) 20)
  (test-case
      "Date/time truncation"
    (id-length 10)
    (count-length 9)
    (check-equal? (msg-id 123456789) "2123456789")
    (check-equal? (msg-id 1) "2000000001"))
  (test-case
      "Prefix/Suffix"
    (id-length 20)
    (count-length 4)
    (id-prefix "AAA")
    (id-suffix "ZZZ")
    (check-equal? (substring (msg-id 1) 0 3) "AAA")
    (check-equal? (substring (msg-id 1) 17) "ZZZ")))
