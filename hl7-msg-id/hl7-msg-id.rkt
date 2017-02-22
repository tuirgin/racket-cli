#!/usr/bin/env racket
#lang racket/base

(provide main)

(require racket/cmdline
         racket/file
         racket/format)

#|

Modify HL7 MSH-10 (Message Control ID). All files provided at run time
will be given a unique message control id.

|#

;; ---------------------------------------------------------------------------------------------------
;; COMMAND-LINE

(define files
  (command-line
   #:program "hl7-msg-id"
   #:ps "\nPURPOSE: Incrementally change the message control id of specified HL7 files.
   NOTE: If the argument list is too long, use find and xargs; e.g.:
     $> find . -iname '*.hl7' -print0 | xargs -n 100 -0 hl7-msg-id"
   #:args files
   files))

;; ---------------------------------------------------------------------------------------------------
;; FUNCTIONS

;; FILE Number -> FILE
;; Change MSH-10 of ~file-name~ to ~id~. Return incremented ~id~.
(define (process-file file-name id)
  (lines->file
   (replace-msg-id file-name id)
   ;; For testing:
   ;; (string-append (regexp-replace #rx"\\.txt" file-name "-new.txt")))
   ;; TODO: cli switch for in-place vs rename
   file-name)
  (printf "- ~a: ~a\n" (~r id #:min-width 4) file-name)
  (add1 id))

;; FILE -> [Listof String]
;; Open and modify ~file-name~ to have an MSH-10 of ~id~.
(define (replace-msg-id file-name id)
  (define hl7 (file->lines file-name))
  (define msh-regx (pregexp "^((MSH\\|)([^|]*\\|){8})[^|]*(\\|.*)$"))
  (define msh-record (regexp-match msh-regx (car hl7)))
  (define new-msh (string-append (cadr msh-record)
                                 (number->string id)
                                 (list-ref msh-record 4)))
  (cons new-msh (cdr hl7)))

;; [Listof X] -> FILE
;; Write list to file with <CR> line-endings.
(define (lines->file lines file-name)
  (display-lines-to-file lines
                         file-name
                         #:exists 'replace
                         #:separator #"\r"
                         #:mode 'text))

;; ---------------------------------------------------------------------------------------------------
;; MAIN

(define main
  (foldl process-file 1 files))

