#!/usr/bin/env racket
#lang racket/base

#|

Read a JResultNet TxLog and output data in usable formats: Plain Text, CSV,
Racket-readable data.

|#

(require openssl/sha1
         racket/bytes
         racket/cmdline
         racket/file
         racket/list
         racket/pretty
         racket/string)

(provide format-date
         load-txlog
         print-txlog
         read-txlog
         string-repeat
         txlog->list)

(module+ test
  (require rackunit))

#|
;; DATA DEFINITIONS
|#

#|
;; TxLog is a line-by-line representation of JResultNet communications consisting of:
;;
;; - I/O field, one of:
;;   * XX: Comm status message indicator
;;   * Rx: Receive
;;   * Tx: Transmit
;; - Date/Time Stamp
;; - Message, one of:
;;   * Plain text interface status message
;;   * Up to 16 space-separated hexadecimal octets representing a portion of the
;;     actual data which crossed the JRN device interface.
;; - A plain text rendering of the hexadecimal data from the previous field,
;;   where control characters have been rendered as "."
;;
;; NOTE: There is *no* way of determining how to group lines into records based
;; on the I/O and Date/Time Stamp alone as the time stamp is dependent upon the
;; device implementation. Some devices may yield one time stamp per record,
;; while others may have multiple time stamps per record, or multiple records
;; per time stamp.
;;
;; Example:
;; XX 20170110180250650   Made Connection
;; Rx 20170110191008843   0b 4d 53 48 7c 5e 7e 5c 26 7c 53 65 6e 64 69 6e  .MSH|^~\&|Sendin
;; Rx 20170110191008843   67 20 41 70 70 6c 69 63 61 74 69 6f 6e 7c 53 65  g Application|Se
;; Rx 20170110191008843   6e 64 69 6e 67 20 46 61 63 69 6c 69 74 79 7c 4a  nding Facility|J
;; Rx 20170110191008843   52 65 73 75 6c 74 4e 65 74 7c 52 2d 48 4f 53 54  ResultNet|R-HOST
;; Rx 20170110191008843   2d 32 7c 32 30 31 37 30 31 31 30 31 39 31 35 32  -2|2017011019152
;; Rx 20170110191008843   30 2e 36 31 36 7c 7c 41 43 4b 5e 52 33 33 5e 41  0.616||ACK^R33^A
;; Rx 20170110191008843   43 4b 2d 52 33 33 7c 31 39 34 37 7c 50 7c 32 2e  CK-R33|1947|P|2.
;; Rx 20170110191008843   33 0d 4d 53 41 7c 41 41 7c 31 39 34 37 7c 50 61  3.MSA|AA|1947|Pa
;; Rx 20170110191008843   74 69 65 6e 74 49 44 2d 46 30 31 2d 30 34 7c 7c  tientID-F01-04||
;; Rx 20170110191008843   7c 41 43 43 45 50 54 20 53 49 4d 55 4c 41 54 49  |ACCEPT SIMULATI
;; Rx 20170110191008843   4f 4e 0d 1c 0d                                   ON...
;; Tx 20170110191009350   0b 4d 53 48 7c 5e 7e 5c 26 7c 4a 52 65 73 75 6c  .MSH|^~\&|JResul
;; Tx 20170110191009350   74 4e 65 74 7c 7c 7c 7c 32 30 31 37 30 31 31 30  tNet||||20170110
;; Tx 20170110191009350   31 39 31 30 30 38 7c 7c 41 43 4b 7c 37 39 32 7c  191008||ACK|792|
;; Tx 20170110191009350   50 7c 32 2e 33 0d 4d 53 41 7c 43 41 7c 31 39 34  P|2.3.MSA|CA|194
;; Tx 20170110191009350   31 7c 50 61 74 69 65 6e 74 49 44 2d 46 30 31 2d  1|PatientID-F01-
;; Tx 20170110191009350   30 31 0d 1c 0d                                   01...
|#

#|
;; io-string is one of:
;; - "XX"
;; - "Rx"
;; - "Tx"
|#

#|
;; time-string is 17-character string:
;; e.g.: 20170102030405006
;;
;; An example of parsing and reformatting with the srfi-19 library:
;;
;; (date->string (string->date "20170102030405006" "~Y~m~d~H~M~S~N") "~5.~N")
;; -> "2017-01-02T03:04:05.006000000"
|#

#|
;; tx-msg-string is one of:
;; - String
;; - Hex-string, consisting of 16 space-separated hexadecimal octets
;;
;; Examples:
;; - "Made Connection"
;; - "0b 4d 53 48 7c 5e 7e 5c 26 7c 53 65 6e 64 69 6e"
;;
;; The plain text string is an interface status message. The hex-string
;; represents the actual i/o data recorded by the JRN interface device.
|#

#|
;; tx-line is a Listof time-string io-string tx-msg-string
|#

;; Default Parameters for Output
;; --------------------------------------------------------------------------------------------------

(define csv (make-parameter #f))
(define json (make-parameter #f))
(define pretty (make-parameter #t))
(define sep (make-parameter "="))
(define width (make-parameter 80))


;; Command Line
;; --------------------------------------------------------------------------------------------------

(module+ main
  (define txlog
    (command-line
     #:program "cat-txlog"
     #:once-each
     [("-s" "--separator")
      str
      "Specify the separator to use between records."
      (sep str)]
     [("-w" "--width")
      int
      "Specify the width in repetitions of the separator."
      (width (string->number int))]
     #:once-any
     [("-p" "--pretty") "Pretty printed output. Default" (pretty #t)]
     [("-u" "--ugly")
      "Disable pretty printing. Output will be in a parenthetical list."
      (pretty #f)]
     [("-c" "--csv") "CSV output." (csv #t)]
     #:args (filename)
     filename))

  (print-txlog (txlog->list (read-txlog txlog))))


;; Process Input: read input, convert hex to strings, and create data structure
;; --------------------------------------------------------------------------------------------------

;; read-txlog : TxLog -> Listof tx-line
(define (read-txlog file)
  (call-with-input-file file (λ (in) (reconstruct-txlog in))))

(define (txlog->list lst)
  (cond [(null? lst) '()]
        [(null? (cdr lst)) (list (first lst))]
        [(msg-match? (first lst) (second lst))
         (txlog->list (cons (merge-lines (first lst) (second lst)) (cddr lst)))]
        [else (cons (first lst) (txlog->list (cdr lst)))]))

(define (load-txlog txlog)
  (txlog->list (read-txlog txlog)))

(define (msg-match? l1 l2)
  (and (= (first l1) (first l2))
       (string=? (second l1) (second l2))
       (not (string=? (second l1) "XX"))))

(define (merge-lines l1 l2)
  (cond [(msg-match? l1 l2)
         (list (first l1)
               (second l1)
               (string-append (third l1) (third l2)))]))

(module+ test
  (define rx1 '(20170110180259198 "Rx" "MSH|^~\\&|Abbott"))
  (define rx2 '(20170110180259198 "Rx" " Point of Care|A"))
  (define rx3 '(20170110180257979 "Rx" "MSH|^~\\&|Abbott"))
  (define tx1 '(20170110180257979 "Tx" "MSH|^~\\&|JResul"))
  (define tx2 '(20170110180257979 "Tx" "tNet|JResultNet|"))
  (define xx1 '(20170110180244302 "XX" "Listening for Connection"))
  (define xx2 '(20170110180250650 "XX" "Made Connection"))
  (define xx3 '(20170110180244302 "XX" "Made Connection"))
  (test-equal? "Empty" (txlog->list '()) '())
  (test-equal? "Single Line" (txlog->list (list rx1)) (list rx1))
  (test-equal? "Matching"
               (txlog->list (list rx1 rx2))
               '((20170110180259198 "Rx" "MSH|^~\\&|Abbott Point of Care|A")))
  (test-equal? "XX + Tx"
               (txlog->list (list xx1 tx1))
               '((20170110180244302 "XX" "Listening for Connection")
                 (20170110180257979 "Tx" "MSH|^~\\&|JResul")))
  (test-equal? "Rx + Tx"
               (txlog->list (list rx1 tx1))
               '((20170110180259198 "Rx" "MSH|^~\\&|Abbott")
                 (20170110180257979 "Tx" "MSH|^~\\&|JResul")))
  (test-equal? "Rx Non-matching Timestamp"
               (txlog->list (list rx3 rx2))
               '((20170110180257979 "Rx" "MSH|^~\\&|Abbott")
                 (20170110180259198 "Rx" " Point of Care|A")))
  (test-equal? "XX with Matching Timestamp"
               (txlog->list (list xx1 xx3))
               '((20170110180244302 "XX" "Listening for Connection")
                 (20170110180244302 "XX" "Made Connection"))))

;; reconstruct-txlog : TxLog -> Listof String
(define (reconstruct-txlog in)
  (for/list ([line (in-lines in)])
    (define line-lst (regexp-split #px"\\s{2,}" line))
    (define time+io-string (string-split (first line-lst)))
    (define io-string (first time+io-string))
    (define time-string (string->number (second time+io-string)))
    (define tx-msg-string (cond [(string=? io-string "XX") (second line-lst)]
                                [else (hexstr->string (second line-lst))]))
    (list time-string io-string tx-msg-string)))

(module+ test
  (define line-xx "XX 20170110093143816   Listening for Connection")
  (define line-start-msg
    "Rx 20170110180257640   0b 4d 53 48 7c 5e 7e 5c 26 7c 41 62 62 6f 74 74  .MSH|^~\x5c&|Abbott")
  (define line-new-record
    "Rx 20170110180257640   0d 4f 52 43 7c 4e 57 0d 4f 42 52 7c 31 7c 7c 7c  .ORC|NW.OBR|1|||")
  (define line-end-msg
    "Tx 20170110180257979   30 31 0d 1c 0d                                   01...")
  (test-equal? "Non-hex Line"
               (reconstruct-txlog (open-input-string line-xx))
               '((20170110093143816 "XX" "Listening for Connection")))
  (test-equal? "Message Start"
               (reconstruct-txlog (open-input-string line-start-msg))
               '((20170110180257640 "Rx" "MSH|^~\\&|Abbott")))
  (test-equal? "New Record"
               (reconstruct-txlog (open-input-string line-new-record))
               '((20170110180257640 "Rx" "\nORC|NW\nOBR|1|||")))
  (test-equal? "Message End"
               (reconstruct-txlog (open-input-string line-end-msg))
               '((20170110180257979 "Tx" "01\n"))))

(define (hexstr->string str)
  (define (hs->str hs)
    (bytes->string/utf-8 (bytes-join (map hex-string->bytes (string-split hs)) #"")))
  (define (r->n str) (regexp-replace* #px"\x0d{1,}" str "\n"))
  (define (del-cntrl str) (regexp-replace* #px"[\x0b\x1c]" str ""))
  (r->n (del-cntrl (hs->str str))))

(module+ test
  (define hx-new-msg "0b 4d 53 48 7c 5e 7e 5c 26 7c 41 62 62 6f 74 74")
  (define hx-new-record "0d 4f 52 43 7c 4e 57 0d 4f 42 52 7c 31 7c 7c 7c")
  (test-equal? "Empty String" (hexstr->string "") "")
  (test-equal? "Multiple Segment Separators" (hexstr->string "0d 0d 0d 0d") "\n")
  (test-equal? "End of Message" (hexstr->string "0d 1c 0d") "\n")
  (test-equal? "Start of Message" (hexstr->string hx-new-msg) "MSH|^~\\&|Abbott")
  (test-equal? "New Records" (hexstr->string hx-new-record) "\nORC|NW\nOBR|1|||"))


;; Output
;; --------------------------------------------------------------------------------------------------

(define (print-txlog lst)
  ;; TODO: Would there be any use for JSON output?
  (cond [(csv) (print/csv lst)]
        [(pretty) (print/pretty lst)]
        [else (printf "~s~n" lst)]))

(define (print/pretty lst)
  (for ([i lst])
    (printf "~a (~a):\n\n~a\n~a\n\n"
            (format-date (first i))
            (second i)
            (third i)
            (string-repeat (width) (sep)))))

(define (print/csv lst)
  (printf "\"yyyy-mm-dd hh:mm:ss.000\",\"IO\",\"Data\"\n")
  (for ([i lst])
    (printf "~a,\"~a\",\"~a\"\n"
            (format-date (first i))
            (second i)
            (third i))))

(define (format-date int)
  (define dt-str (number->string int))
  (cond [(equal? (string-length dt-str) 17)
         (format-date/datetime dt-str)]
        [else dt-str]))

(define (format-date/datetime dts)
  (string-append (substring dts 0 4)
                 "-"
                 (substring dts 4 6)
                 "-"
                 (substring dts 6 8)
                 " "
                 (substring dts 8 10)
                 ":"
                 (substring dts 10 12)
                 ":"
                 (substring dts 12 14)
                 "."
                 (substring dts 14)))

(define (string-repeat cnt str)
  (string-append* (make-list cnt str)))

(module+ test
  (test-equal? "Date Format" (format-date 20160102030405678) "2016-01-02 03:04:05.678")
  (test-equal? "Date Format: Unknown" (format-date 20160102) "20160102"))

