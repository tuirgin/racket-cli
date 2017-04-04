#lang racket

;; (require "cat-txlog.rkt")

(require (for-syntax racket/syntax)
         openssl/sha1
         threading)

(module+ test
  (require racket/pretty
           rackunit
           rackunit/text-ui))

;; An detail struct is:
;; -  dt: Date/Time Stamp
;; -  io: One of Rx, Tx, XX
;; -  id: Message Control ID
;; - val: List of values from HL7. It may be one or more element values, or an
;;        entire segment.
(struct detail (dt io id val) #:transparent)

;;;; Import TxLog
;; --------------------------------------------------------------------------------------------------

;; No input verification. TxLog or else...

;; read-txlog : TxLog -> (Listof String)
(define (read-txlog file-path)
  (~> (file->lines file-path)
      split-txlines
      txlines->txlist
      txlist->records
      hl7->list))

;; JRN doesn't always split HL7 messages in the TxLog. Make sure new MSH
;; messages don't start in the middle of the line.

;; split-txlines : (Listof String) -> (Listof String)
(define (split-txlines txlog)
  (cond [(empty? txlog) '()]
        [(regexp-match #px"[^ ] 0b" (first txlog))
         (cons (part-a (first txlog))
               (cons (part-b (first txlog))
                     (split-txlines (rest txlog))))]
        [else (cons (first txlog) (split-txlines (rest txlog)))]))

;; Assume no more than one \x0b beyond beginning of line
(define pattern (pregexp "^(.*[^ ]) (0b.*)"))

;; part-x : String -> String
(define (part-x str match#)
  (~a (substring str 0 23)
      (regexp-replace pattern (substring str 23 70) match#)))

;; part-a : String -> String
(define (part-a str) (part-x str "\\1"))

;; part-b : String -> String
(define (part-b str) (part-x str "\\2"))

(module+ test
  (define split-tx0
    (list
     "Rx 20170331222104659   54 45 7c 32 7c 7c 7c 44 4f 42 3d 31 30 37 31 39  TE|2|||DOB=10719"
     "Rx 20170331222104659   35 37 7c 7c 32 30 31 32 30 39 31 38 31 33 35 38  57||201209181358"
     "Rx 20170331222104659   31 37 2d 30 34 3a 30 30 0d 4e 54 45 7c 33 7c 7c  17-04:00.NTE|3||"
     "Rx 20170331222104659   7c 44 72 3d 42 69 67 20 54 6f 65 7c 7c 32 30 31  |Dr=Big Toe||201"
     "Rx 20170331222104659   32 30 39 31 38 31 33 35 38 31 37 2d 30 34 3a 30  20918135817-04:0"
     "Rx 20170331222104659   30 0d 4e 54 45 7c 34 7c 7c 7c 53 65 78 3d 4f 74  0.NTE|4|||Sex=Ot"
     "Rx 20170331222104659   68 65 72 7c 7c 32 30 31 32 30 39 31 38 31 33 35  her||20120918135"))
  (define split-tx1
    (list
     "Rx 20170331222055460   30 34 3a 30 30 0d 4e 54 45 7c 35 7c 7c 7c 44 53  04:00.NTE|5|||DS"
     "Rx 20170331222055460   4e 3d 43 30 32 0d 1c 0d 0b 4d 53 48 7c 5e 7e 5c  N=C02....MSH|^~\\"
     "Rx 20170331222055460   26 7c 41 62 62 6f 74 74 20 50 6f 69 6e 74 20 6f  &|Abbott Point o"))
  (test-equal? "Unmixed lines" (split-txlines split-tx0) split-tx0)
  (test-equal? "part-a" (part-a (second split-tx1)) "Rx 20170331222055460   4e 3d 43 30 32 0d 1c 0d")
  (test-equal? "part-b" (part-b (second split-tx1)) "Rx 20170331222055460   0b 4d 53 48 7c 5e 7e 5c")
  (test-equal?
   "Mixed lines"
   (split-txlines split-tx1)
   '("Rx 20170331222055460   30 34 3a 30 30 0d 4e 54 45 7c 35 7c 7c 7c 44 53  04:00.NTE|5|||DS"
     "Rx 20170331222055460   4e 3d 43 30 32 0d 1c 0d"
     "Rx 20170331222055460   0b 4d 53 48 7c 5e 7e 5c"
     "Rx 20170331222055460   26 7c 41 62 62 6f 74 74 20 50 6f 69 6e 74 20 6f  &|Abbott Point o")))

;; txlines->txlist : (Listof String) -> (Listof (List String))
(define (txlines->txlist txlog)
  (for/list ([line txlog])
    (define line-lst (regexp-split #px"\\s{2,}" line))
    (define time+io-string (string-split (first line-lst)))
    (define io-string (first time+io-string))
    (define time-string (string->number (second time+io-string)))
    (define tx-msg-string (cond [(string=? io-string "XX") (second line-lst)]
                                [else (hexstr->string (second line-lst))]))
    (list time-string io-string tx-msg-string)))

(module+ test
  (define line-xx '("XX 20170110093143816   Listening for Connection"))
  (define line-start-msg
    '("Rx 20170110180257640   0b 4d 53 48 7c 5e 7e 5c 26 7c 41 62 62 6f 74 74  .MSH|^~\x5c&|Abbott"))
  (define line-new-record
    '("Rx 20170110180257640   0d 4f 52 43 7c 4e 57 0d 4f 42 52 7c 31 7c 7c 7c  .ORC|NW.OBR|1|||"))
  (define line-end-msg
    '("Tx 20170110180257979   30 31 0d 1c 0d                                   01..."))
  (test-equal? "Non-hex Line"
               (txlines->txlist line-xx)
               '((20170110093143816 "XX" "Listening for Connection")))
  (test-equal? "Message Start"
               (txlines->txlist line-start-msg)
               '((20170110180257640 "Rx" "MSH|^~\\&|Abbott")))
  (test-equal? "New Record"
               (txlines->txlist line-new-record)
               '((20170110180257640 "Rx" "\nORC|NW\nOBR|1|||")))
  (test-equal? "Message End"
               (txlines->txlist line-end-msg)
               '((20170110180257979 "Tx" "01\n"))))

(define (hexstr->string str)
  (define (hs->str hs)
    (bytes->string/utf-8 (bytes-join (map hex-string->bytes (string-split hs)) #"")))
  (define (r->n str) (regexp-replace* #px"\x0d{1,}" str "\n"))
  (define (del-cntrl str) (regexp-replace* #px"[\x0b\x1c]" str ""))
  (define (ns->n str) (regexp-replace* #px"[\n\r]{2,}" str "\n"))
  (~> (hs->str str) r->n del-cntrl ns->n))

(module+ test
  (define hx-new-msg "0b 4d 53 48 7c 5e 7e 5c 26 7c 41 62 62 6f 74 74")
  (define hx-new-record "0d 4f 52 43 7c 4e 57 0d 4f 42 52 7c 31 7c 7c 7c")
  (test-equal? "Empty String" (hexstr->string "") "")
  (test-equal? "Multiple Segment Separators" (hexstr->string "0d 0d 0d 0d") "\n")
  (test-equal? "End of Message" (hexstr->string "0d 1c 0d") "\n")
  (test-equal? "Start of Message" (hexstr->string hx-new-msg) "MSH|^~\\&|Abbott")
  (test-equal? "New Records" (hexstr->string hx-new-record) "\nORC|NW\nOBR|1|||"))

(define (txlist->records lst)
  (cond [(null? lst) '()]
        [(null? (cdr lst)) (list (first lst))]
        [(msg-match? (first lst) (second lst))
         (txlist->records (cons (merge-lines (first lst) (second lst)) (cddr lst)))]
        [else (cons (first lst) (txlist->records (cdr lst)))]))

(define (merge-records txlog)
  (cond [(null? txlog) '()]
        [(null? (cdr txlog)) (list (first txlog))]
        [(msg-match? (first txlog) (second txlog))
         (txlist->records (cons (merge-lines (first txlog) (second txlog)) (cddr txlog)))]
        [else (cons (first txlog) (txlist->records (cdr txlog)))]))

(define (msg-match? l1 l2)
  (and (= (first l1) (first l2))
       (string=? (second l1) (second l2))
       (not (string=? (second l1) "XX"))
       (not (string-prefix? (third l2) "MSH"))))

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
  (test-equal? "Empty" (txlist->records '()) '())
  (test-equal? "Single Line" (txlist->records (list rx1)) (list rx1))
  (test-equal? "Matching"
               (txlist->records (list rx1 rx2))
               '((20170110180259198 "Rx" "MSH|^~\\&|Abbott Point of Care|A")))
  (test-equal? "XX + Tx"
               (txlist->records (list xx1 tx1))
               '((20170110180244302 "XX" "Listening for Connection")
                 (20170110180257979 "Tx" "MSH|^~\\&|JResul")))
  (test-equal? "Rx + Tx"
               (txlist->records (list rx1 tx1))
               '((20170110180259198 "Rx" "MSH|^~\\&|Abbott")
                 (20170110180257979 "Tx" "MSH|^~\\&|JResul")))
  (test-equal? "Rx Non-matching Timestamp"
               (txlist->records (list rx3 rx2))
               '((20170110180257979 "Rx" "MSH|^~\\&|Abbott")
                 (20170110180259198 "Rx" " Point of Care|A")))
  (test-equal? "XX with Matching Timestamp"
               (txlist->records (list xx1 xx3))
               '((20170110180244302 "XX" "Listening for Connection")
                 (20170110180244302 "XX" "Made Connection"))))

;;;; Structure Data
;; --------------------------------------------------------------------------------------------------

(define (hl7->list lst)
  (for/list ([m (in-list lst)])
    (list (first m) (second m) (msg->list (third m)))))

(define (msg->list msg)
  (for/list ([r (in-list (msg-split msg))])
    (seg-split r)))

(define (msg-split msg)
  (string-split msg "\n"))

(define (seg-split seg)
  (string-split seg "|"))

(module+ test
  (define txlog '((20170110180250650 "XX" "Made Connection") (20170110180257640 "Rx" "MSH|^~\\&|||||||ORU^R30^ORU-R30|1\nPID|1||PatientID-A01-01\nORC|NW\nOBR|1|||E3+\nOBX|1||^NA||143\nOBX|2||^K||4.2\nOBX|3||^HCT||60|%PCV\n") (20170110180257979 "Tx" "MSH|^~\\&|||||20170110180257||ACK|5676\nMSA|CA|1|PatientID-A01-01\n") (20170110180259198 "Rx" "MSH|^~\\&|||||||ORU^R30^ORU-R30|2||\nPID|1||PatientID-A01-02\nORC|NW\nOBR|1|||E3+\nOBX|1||^NA||143|mmol/L\nOBX|2||^K||4.2|mmol/L\nOBX|3||^HCT||60|%PCV\n") (20170110180259539 "Tx" "MSH|^~\\&|||||20170110180259||ACK|5677\nMSA|CA|2|PatientID-A01-02\n")))
  (define msg1 "MSH|^~\\&|||||||ORU^R30^ORU-R30|1||\nPID|1||PatientID-A01-01||||||||||||||||\nORC|NW\nOBR|1|||E3+\nOBX|1||^NA||143||\nOBX|2||^K||4.2|||||||||||||||\n")
  (define rec1 "MSH|^~\\&|||||||ORU^R30^ORU-R30|1||")
  (test-equal? "Record to List"
               (seg-split  rec1)
               '("MSH" "^~\\&" "" "" "" "" "" "" "ORU^R30^ORU-R30" "1" ""))
  (test-equal? "HL7 to List"
               (msg->list msg1)
               '(("MSH" "^~\\&" "" "" "" "" "" "" "ORU^R30^ORU-R30" "1" "") ("PID" "1" "" "PatientID-A01-01" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "") ("ORC" "NW") ("OBR" "1" "" "" "E3+") ("OBX" "1" "" "^NA" "" "143" "") ("OBX" "2" "" "^K" "" "4.2" "" "" "" "" "" "" "" "" "" "" "" "" "" "")))
  (test-equal? "TxLog: HL7 String to HL7 List"
               (hl7->list txlog)
               '((20170110180250650 "XX" (("Made Connection"))) (20170110180257640 "Rx" (("MSH" "^~\\&" "" "" "" "" "" "" "ORU^R30^ORU-R30" "1") ("PID" "1" "" "PatientID-A01-01") ("ORC" "NW") ("OBR" "1" "" "" "E3+") ("OBX" "1" "" "^NA" "" "143") ("OBX" "2" "" "^K" "" "4.2") ("OBX" "3" "" "^HCT" "" "60" "%PCV"))) (20170110180257979 "Tx" (("MSH" "^~\\&" "" "" "" "" "20170110180257" "" "ACK" "5676") ("MSA" "CA" "1" "PatientID-A01-01"))) (20170110180259198 "Rx" (("MSH" "^~\\&" "" "" "" "" "" "" "ORU^R30^ORU-R30" "2" "") ("PID" "1" "" "PatientID-A01-02") ("ORC" "NW") ("OBR" "1" "" "" "E3+") ("OBX" "1" "" "^NA" "" "143" "mmol/L") ("OBX" "2" "" "^K" "" "4.2" "mmol/L") ("OBX" "3" "" "^HCT" "" "60" "%PCV"))) (20170110180259539 "Tx" (("MSH" "^~\\&" "" "" "" "" "20170110180259" "" "ACK" "5677") ("MSA" "CA" "2" "PatientID-A01-02"))))))

;;;; Query Data
;; --------------------------------------------------------------------------------------------------

;; Filter on segment and sequence number.
;;
;; NOTE: MSH segments require a pos value that is 1 less than the sequence
;; number; i.e. the Message Control ID is at sequence 10, but pos 9.
(define (hl7-ref txlog seg pos)
  (filter detail?                       ; TODO: Refactor to make use of (filter-not void? ...)
          (for*/list ([m (in-list txlog)]
                      [s (in-list (third m))])
            (when (and (not (null? s))
                       (string=? (first s) (sym->str seg)))
              (create-detail m (list (list-ref s pos) (get-msg-type m)))))))

;; Return segments by segment id; i.e. "MSA", "OBR", "PID"
(define (seg-assoc seg txlog)
  (filter detail?                       ; TODO: Refactor to make use of (filter-not void? ...)
          (for*/list ([m (in-list txlog)]
                      [s (in-list (third m))])
            (when (and (not (null? s))
                       (string=? (first s) (sym->str seg)))
              (create-detail m s)))))

;; Filter on segment id plus a value match at the specified element.
;;
;; E.g. To return details with MSA segments with a msg ctrl id of "3":
;; (seg-ref=? txlog 'msa 2 "3")
(define (seg-ref=? txlog seg pos str)
  (filter (λ (lst) (string=? (list-ref (detail-val lst) pos) str))
          (seg-assoc seg txlog)))

;; Find messages by element value plus corresponding MSA responses
(define (track-msg txlog seg pos str)
  (define msgs (track-msg/ref=? txlog seg pos str))
  (sort (append msgs (track-msg/msa txlog msgs)) datetime<))

(define (track-msg/ref=? txlog seg pos str)
  (filter detail?                       ; TODO: Refactor to make use of (filter-not void? ...)
          (for*/list ([m (in-list txlog)]
                      [s (in-list (third m))])
            (when (and (not (null? s))
                       (string=? (first s) (sym->str seg))
                       (string=? (list-ref s pos) str))
              (create-detail m (list (list-ref s pos) (get-msg-type m)))))))

(define (track-msg/msa txlog lst)
  (foldl (λ (v l) (append (seg-ref=? txlog 'msa 2 (detail-id v)) l)) '() lst))

(module+ test

  (define tx0
    '((20170110180250650 "XX" (("Made Connection")))
      (20170110180257640 "Rx" (("MSH" "^~\\&" "Abbott Point of Care" "Abbott Point of Care" "" "" "20120327153803-04:00" "" "ORU^R30^ORU-R30" "1" "P" "2.6")
                               ("PID" "1" "" "PatientID-A01-01" "" "LastName-A01-01^FirstName^MiddleName" "" "19000101" "U" "" "" "" "" "" "" "" "" "" "PatientAccount-A01-01" "SSN")
                               ("ORC" "NW")
                               ("OBR" "1" "" "" "E3+")
                               ("OBX" "1" "" "495412f2-2a1b-4b8b-b600-7e0ab70421a6^NA" "" "143" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "2" "" "8c3e9ddf-9a86-473e-8c32-6797c491c809^K" "" "4.2" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "3" "" "7ab86ff9-313b-4bea-a13e-a416a8f5e249^HCT" "" "60" "%PCV" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "4" "" "c135d903-889f-4fd1-add9-6460eaf37dbd^HB" "" "20.4" "g/dL" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("NTE" "1" "" "" "Allen's Test=Fail" "" "20120327153803-04:00")
                               ("NTE" "2" "" "" "PtTemp=98.6 F" "" "20120327153803-04:00")
                               ("NTE" "3" "" "" "DelSys=Bagging" "" "20120327153803-04:00")
                               ("NTE" "4" "" "" "CPB=No" "" "20120327153803-04:00")
                               ("NTE" "5" "" "" "Site=L Femoral" "" "20120327153803-04:00")
                               ("NTE" "6" "" "" "Sample Type=MIX" "" "20120327153803-04:00")
                               ("NTE" "7" "" "" "DSN=A01" "" "20120327153803-04:00")))
      (20160110180257979 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180257" "" "ACK" "5676" "P" "2.6")
                               ("MSA" "CA" "1" "PatientID-A01-01")))
      (20150110180259198 "Rx" (("MSH" "^~\\&" "Abbott Point of Care" "Abbott Point of Care" "" "" "20120327153803-04:00" "" "ORU^R30^ORU-R30" "2" "P" "2.6")
                               ("PID" "1" "" "PatientID-A01-01" "" "LastName-A01-02^FirstName^MiddleName" "" "19000101" "U" "" "" "" "" "" "" "" "" "" "PatientAccount-A01-02" "SSN")
                               ("ORC" "NW")
                               ("OBR" "1" "" "" "E3+")
                               ("OBX" "1" "" "495412f2-2a1b-4b8b-b600-7e0ab70421a6^NA" "" "143" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "2" "" "8c3e9ddf-9a86-473e-8c32-6797c491c809^K" "" "4.2" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "3" "" "7ab86ff9-313b-4bea-a13e-a416a8f5e249^HCT" "" "60" "%PCV" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "4" "" "c135d903-889f-4fd1-add9-6460eaf37dbd^HB" "" "20.4" "g/dL" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("NTE" "1" "" "" "Allen's Test=Fail" "" "20120327153803-04:00")
                               ("NTE" "2" "" "" "PtTemp=98.6 F" "" "20120327153803-04:00")
                               ("NTE" "3" "" "" "DelSys=Bagging" "" "20120327153803-04:00")
                               ("NTE" "4" "" "" "CPB=No" "" "20120327153803-04:00")
                               ("NTE" "5" "" "" "Site=L Femoral" "" "20120327153803-04:00")
                               ("NTE" "6" "" "" "Sample Type=MIX" "" "20120327153803-04:00")
                               ("NTE" "7" "" "" "DSN=A01" "" "20120327153803-04:00")))
      (20170110180259539 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180259" "" "ACK" "5677" "P" "2.6")
                               ("MSA" "CA" "2" "PatientID-A01-01")))
      (20170110180301499 "Rx" (("MSH" "^~\\&" "Abbott Point of Care" "Abbott Point of Care" "" "" "20120327153803-04:00" "" "ORU^R30^ORU-R30" "3" "P" "2.6")
                               ("PID" "1" "" "PatientID-A01-03" "" "LastName-A01-03^FirstName^MiddleName" "" "19000101" "U" "" "" "" "" "" "" "" "" "" "PatientAccount-A01-03" "SSN")
                               ("ORC" "NW")
                               ("OBR" "1" "" "" "E3+")
                               ("OBX" "1" "" "495412f2-2a1b-4b8b-b600-7e0ab70421a6^NA" "" "143" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "2" "" "8c3e9ddf-9a86-473e-8c32-6797c491c809^K" "" "4.2" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "3" "" "7ab86ff9-313b-4bea-a13e-a416a8f5e249^HCT" "" "60" "%PCV" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "4" "" "c135d903-889f-4fd1-add9-6460eaf37dbd^HB" "" "20.4" "g/dL" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("NTE" "1" "" "" "Allen's Test=Fail" "" "20120327153803-04:00")
                               ("NTE" "2" "" "" "PtTemp=98.6 F" "" "20120327153803-04:00")
                               ("NTE" "3" "" "" "DelSys=Bagging" "" "20120327153803-04:00")
                               ("NTE" "4" "" "" "CPB=No" "" "20120327153803-04:00")
                               ("NTE" "5" "" "" "Site=L Femoral" "" "20120327153803-04:00")
                               ("NTE" "6" "" "" "Sample Type=MIX" "" "20120327153803-04:00")
                               ("NTE" "7" "" "" "DSN=A01" "" "20120327153803-04:00")))
      (20170110180301835 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180301" "" "ACK" "5678" "P" "2.6")
                               ("MSA" "CA" "3" "PatientID-A01-03")))
      (20170110180303781 "Rx" (("MSH" "^~\\&" "Abbott Point of Care" "Abbott Point of Care" "" "" "20120327153803-04:00" "" "ORU^R30^ORU-R30" "4" "P" "2.6")
                               ("PID" "1" "" "PatientID-A01-04" "" "LastName-A01-04^FirstName^MiddleName" "" "19000101" "U" "" "" "" "" "" "" "" "" "" "PatientAccount-A01-04" "SSN")
                               ("ORC" "NW")
                               ("OBR" "1" "" "" "E3+")
                               ("OBX" "1" "" "495412f2-2a1b-4b8b-b600-7e0ab70421a6^NA" "" "143" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "2" "" "8c3e9ddf-9a86-473e-8c32-6797c491c809^K" "" "4.2" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "3" "" "7ab86ff9-313b-4bea-a13e-a416a8f5e249^HCT" "" "60" "%PCV" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "4" "" "c135d903-889f-4fd1-add9-6460eaf37dbd^HB" "" "20.4" "g/dL" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("NTE" "1" "" "" "Allen's Test=Fail" "" "20120327153803-04:00")
                               ("NTE" "2" "" "" "PtTemp=98.6 F" "" "20120327153803-04:00")
                               ("NTE" "3" "" "" "DelSys=Bagging" "" "20120327153803-04:00")
                               ("NTE" "4" "" "" "CPB=No" "" "20120327153803-04:00")
                               ("NTE" "5" "" "" "Site=L Femoral" "" "20120327153803-04:00")
                               ("NTE" "6" "" "" "Sample Type=MIX" "" "20120327153803-04:00")
                               ("NTE" "7" "" "" "DSN=A01" "" "20120327153803-04:00")))
      (20170110180304096 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180303" "" "ACK" "5679" "P" "2.6")
                               ("MSA" "CA" "4" "PatientID-A01-04")))
      (20170110180305403 "Rx" (("MSH" "^~\\&" "Abbott Point of Care" "Abbott Point of Care" "" "" "20120327153803-04:00" "" "ORU^R30^ORU-R30" "5" "P" "2.6")
                               ("PID" "1" "" "PatientID-A01-05" "" "LastName-A01-05^FirstName^MiddleName" "" "19000101" "U" "" "" "" "" "" "" "" "" "" "PatientAccount-A01-05" "SSN")
                               ("ORC" "NW")
                               ("OBR" "1" "" "" "E3+")
                               ("OBX" "1" "" "495412f2-2a1b-4b8b-b600-7e0ab70421a6^NA" "" "143" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "2" "" "8c3e9ddf-9a86-473e-8c32-6797c491c809^K" "" "4.2" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "3" "" "7ab86ff9-313b-4bea-a13e-a416a8f5e249^HCT" "" "60" "%PCV" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "4" "" "c135d903-889f-4fd1-add9-6460eaf37dbd^HB" "" "20.4" "g/dL" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("NTE" "1" "" "" "Allen's Test=Fail" "" "20120327153803-04:00")
                               ("NTE" "2" "" "" "PtTemp=98.6 F" "" "20120327153803-04:00")
                               ("NTE" "3" "" "" "DelSys=Bagging" "" "20120327153803-04:00")
                               ("NTE" "4" "" "" "CPB=No" "" "20120327153803-04:00")
                               ("NTE" "5" "" "" "Site=L Femoral" "" "20120327153803-04:00")
                               ("NTE" "6" "" "" "Sample Type=MIX" "" "20120327153803-04:00")
                               ("NTE" "7" "" "" "DSN=A01" "" "20120327153803-04:00")))
      (20170110180305796 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180305" "" "ACK" "5680" "P" "2.6")
                               ("MSA" "CA" "5" "PatientID-A01-05")))
      (20170110180307026 "Rx" (("MSH" "^~\\&" "Abbott Point of Care" "Abbott Point of Care" "" "" "20120327153803-04:00" "" "ORU^R30^ORU-R30" "6" "P" "2.6")
                               ("PID" "1" "" "PatientID-A01-06" "" "LastName-A01-06^FirstName^MiddleName" "" "19000101" "U" "" "" "" "" "" "" "" "" "" "PatientAccount-A01-06" "SSN")
                               ("ORC" "NW")
                               ("OBR" "1" "" "" "E3+")
                               ("OBX" "1" "" "495412f2-2a1b-4b8b-b600-7e0ab70421a6^NA" "" "143" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "2" "" "8c3e9ddf-9a86-473e-8c32-6797c491c809^K" "" "4.2" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "3" "" "7ab86ff9-313b-4bea-a13e-a416a8f5e249^HCT" "" "60" "%PCV" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("OBX" "4" "" "c135d903-889f-4fd1-add9-6460eaf37dbd^HB" "" "20.4" "g/dL" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                               ("NTE" "1" "" "" "Allen's Test=Fail" "" "20120327153803-04:00")
                               ("NTE" "2" "" "" "PtTemp=98.6 F" "" "20120327153803-04:00")
                               ("NTE" "3" "" "" "DelSys=Bagging" "" "20120327153803-04:00")
                               ("NTE" "4" "" "" "CPB=No" "" "20120327153803-04:00")
                               ("NTE" "5" "" "" "Site=L Femoral" "" "20120327153803-04:00")
                               ("NTE" "6" "" "" "Sample Type=MIX" "" "20120327153803-04:00")
                               ("NTE" "7" "" "" "DSN=A01" "" "20120327153803-04:00")))
      (20140110180307135 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180307" "" "ACK^R33^ACK-R33" "5681" "P" "2.6" "" "" "" "" "AL")
                               ("MSA" "AA" "1" "SpecimenID-A01-01")))
      (20170110180307471 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180307" "" "ACK" "5682" "P" "2.6")
                               ("MSA" "CA" "6" "PatientID-A01-06")))
      (20141210180308692 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180308" "" "ACK^R33^ACK-R33" "5683" "P" "2.6" "" "" "" "" "AL")
                               ("MSA" "AA" "2" "SpecimenID-A01-01")))))

  (test-equal? "Element Reference: PID"
               (hl7-ref tx0 'pid 3)
               (list
                (detail 20170110180257640 "Rx" "1" '("PatientID-A01-01" "ORU^R30^ORU-R30"))
                (detail 20150110180259198 "Rx" "2" '("PatientID-A01-01" "ORU^R30^ORU-R30"))
                (detail 20170110180301499 "Rx" "3" '("PatientID-A01-03" "ORU^R30^ORU-R30"))
                (detail 20170110180303781 "Rx" "4" '("PatientID-A01-04" "ORU^R30^ORU-R30"))
                (detail 20170110180305403 "Rx" "5" '("PatientID-A01-05" "ORU^R30^ORU-R30"))
                (detail 20170110180307026 "Rx" "6" '("PatientID-A01-06" "ORU^R30^ORU-R30"))))

  (test-equal? "Segment Filter: \"mSa\""
               (seg-assoc "mSa" tx0)
               (list
                (detail 20160110180257979 "Tx" "5676" '("MSA" "CA" "1" "PatientID-A01-01"))
                (detail 20170110180259539 "Tx" "5677" '("MSA" "CA" "2" "PatientID-A01-01"))
                (detail 20170110180301835 "Tx" "5678" '("MSA" "CA" "3" "PatientID-A01-03"))
                (detail 20170110180304096 "Tx" "5679" '("MSA" "CA" "4" "PatientID-A01-04"))
                (detail 20170110180305796 "Tx" "5680" '("MSA" "CA" "5" "PatientID-A01-05"))
                (detail 20140110180307135 "Tx" "5681" '("MSA" "AA" "1" "SpecimenID-A01-01"))
                (detail 20170110180307471 "Tx" "5682" '("MSA" "CA" "6" "PatientID-A01-06"))
                (detail 20141210180308692 "Tx" "5683" '("MSA" "AA" "2" "SpecimenID-A01-01"))))

  (test-equal? "Segment Filter: 'msa"
               (seg-assoc 'msa tx0)
               (list
                (detail 20160110180257979 "Tx" "5676" '("MSA" "CA" "1" "PatientID-A01-01"))
                (detail 20170110180259539 "Tx" "5677" '("MSA" "CA" "2" "PatientID-A01-01"))
                (detail 20170110180301835 "Tx" "5678" '("MSA" "CA" "3" "PatientID-A01-03"))
                (detail 20170110180304096 "Tx" "5679" '("MSA" "CA" "4" "PatientID-A01-04"))
                (detail 20170110180305796 "Tx" "5680" '("MSA" "CA" "5" "PatientID-A01-05"))
                (detail 20140110180307135 "Tx" "5681" '("MSA" "AA" "1" "SpecimenID-A01-01"))
                (detail 20170110180307471 "Tx" "5682" '("MSA" "CA" "6" "PatientID-A01-06"))
                (detail 20141210180308692 "Tx" "5683" '("MSA" "AA" "2" "SpecimenID-A01-01"))))

  (test-equal? "Display Segment where Element Equals X"
               (seg-ref=? tx0 'msa 2 "1")
               (list
                (detail 20160110180257979 "Tx" "5676" '("MSA" "CA" "1" "PatientID-A01-01"))
                (detail 20140110180307135 "Tx" "5681" '("MSA" "AA" "1" "SpecimenID-A01-01"))))

  (test-equal? "Track ADT/ORM/ORU for PatientID-A01-01 in PID-3"
               (track-msg/ref=? tx0 'pid 3 "PatientID-A01-01")
               (list
                (detail 20170110180257640 "Rx" "1" '("PatientID-A01-01" "ORU^R30^ORU-R30"))
                (detail 20150110180259198 "Rx" "2" '("PatientID-A01-01" "ORU^R30^ORU-R30"))))

  (define track-d
    (list (detail 20170110180257640 "Rx" "1" '("PatientID-A01-01" "ORU^R30^ORU-R30"))
          (detail 20150110180259198 "Rx" "2" '("PatientID-A01-01" "ORU^R30^ORU-R30"))))

  (test-equal? "Track matching MSA for detail"
               (track-msg/msa tx0 track-d)
               (list
                (detail 20170110180259539 "Tx" "5677" '("MSA" "CA" "2" "PatientID-A01-01"))
                (detail 20141210180308692 "Tx" "5683" '("MSA" "AA" "2" "SpecimenID-A01-01"))
                (detail 20160110180257979 "Tx" "5676" '("MSA" "CA" "1" "PatientID-A01-01"))
                (detail 20140110180307135 "Tx" "5681" '("MSA" "AA" "1" "SpecimenID-A01-01"))))

  (test-equal? "Track ADT/ORM/ORU with MSA"
               (track-msg tx0 'pid 3 "PatientID-A01-01")
               (list
                (detail 20140110180307135 "Tx" "5681" '("MSA" "AA" "1" "SpecimenID-A01-01"))
                (detail 20141210180308692 "Tx" "5683" '("MSA" "AA" "2" "SpecimenID-A01-01"))
                (detail 20150110180259198 "Rx" "2" '("PatientID-A01-01" "ORU^R30^ORU-R30"))
                (detail 20160110180257979 "Tx" "5676" '("MSA" "CA" "1" "PatientID-A01-01"))
                (detail 20170110180257640 "Rx" "1" '("PatientID-A01-01" "ORU^R30^ORU-R30"))
                (detail 20170110180259539 "Tx" "5677" '("MSA" "CA" "2" "PatientID-A01-01")))))

;;;; Reports
;; -------------------------------------------------------------------------------------------------

;; All reports to include:
;;  - List of matches; i.e. PID
;;  - Count of matches
;;  - List of dups; TODO: define duplicates
;;  - Count of dups

;; Report stats based on PID
;; Include:
;;  - TODO: Placer Order Number (ORM): OBR-2 and/or ORC-2
;;  - TODO: Filler Order Number (ORU): OBR-3 and/or ORC-3
(define (report txlog)
  (define pids (hl7-ref txlog 'pid 3))
  (define separator (string-append "\n\n" (string-repeat 133 "=") "\n\n\n" ))
  (~a (format/pids pids)
      (format/pid-track txlog (pids/detail->list pids))
      (format-msa/aa (report-msa/aa txlog))
      (format-msa/ar (report-msa/ar txlog))
      (format-msa/ae (report-msa/ae txlog))
      (format-msa/ca (report-msa/ca txlog))
      (format-msa/ce (report-msa/ce txlog))
      (format-msa/cr (report-msa/cr txlog))
      #:separator separator))

(define (format/pid-track txlog pids)
  (define (format-detail dt io id vals)
    (~a (~a dt #:width 23 #:align 'center)
        (~a io #:width 2 #:align 'left)
        (~a id #:width 38 #:align 'right)
        (~a vals #:align 'left)
        #:separator "  "))
  (define (format-val lst)
    (cond [(string=? (first lst) "MSA")
           (~a "─ " (string-join lst "|") #:align 'left)]
          [(= (length lst) 2)
           (~a (~a (first lst) #:width 20) (~a (second lst) #:width 20) #:separator "  ")]
          [else (string-join lst "  ")]))
  (define header (~a (format-detail "" "" "" (~a (~a "Patient ID" #:width 20)
                                                 (~a "Message Type" #:width 20)
                                                 #:separator "  "))
                     (format-detail "Date/Time"
                                    "IO"
                                    "Message Control ID"
                                    "─ MSA Segment")
                     (format-detail (string-repeat 23 "-")
                                    (string-repeat 2 "-")
                                    (string-repeat 38 "-")
                                    (string-repeat 64 "-"))
                     #:separator "\n"))
  (~a "Patient ID Message Tracking"
      (for/fold ([str ""]) ([pid (in-list pids)]) ; Per PID
        (~a str
            (~a "\n\n» " pid "\n" header)
            (for/fold ([str ""])        ; Per matching record
                      ([i (in-list (map (λ (d) (format-detail
                                        (format-date (detail-dt d))
                                        (detail-io d)
                                        (detail-id d)
                                        (format-val (detail-val d))))
                                (track-msg txlog 'pid 3 pid)))])
              (~a str i #:separator "\n"))))
      #:separator "\n"))

(define (pids/detail->list pids)
  (for/list ([d (in-list pids)])
    (first (detail-val d))))

(define (format/pids pids)
  (define pid-count (length pids))
  (define sep (string-repeat 3 " "))
  (define (format-detail dt io id pid type)
    (~a (~a dt #:width 23 #:align 'center)
        (~a io #:width 2 #:align 'left)
        (~a id #:width 38 #:align 'right)
        (~a pid #:width 20 #:align 'left)
        (~a type #:width 20 #:align 'left)
        #:separator "  "))
  (define header (~a (format-detail "Date/Time"
                                    "IO"
                                    "Message Control ID"
                                    "Patient ID"
                                    "Message Type")
                     (format-detail (string-repeat 23 "-")
                                    (string-repeat 2 "-")
                                    (string-repeat 38 "-")
                                    (string-repeat 20 "-")
                                    (string-repeat 20 "-"))
                     #:separator "\n"))
  (~a "Patient IDs\n\n"
      (~a "Total PIDs: " pid-count "\n\n")
      header
      (for/fold ([str ""]) ([d (in-list pids)])
        (~a str
            (format-detail (format-date (detail-dt d))
                           (detail-io d)
                           (detail-id d)
                           (first (detail-val d))
                           (second (detail-val d)))
            #:separator "\n"))))

;; The define--report-msa macro creates two functions per argument; e.g.
;; (define--report-msa xx yy) produces the following:
;;
;; - report-msa/xx
;; - report-msa/yy
;; - format-msa/xx
;; - format-msa/yy
;;
;; Usage: (report-msa/xx txlog)
;; Usage: (format-msa/xx (report-msa/xx txlog))
(define-syntax (define--report-msa stx)
  (syntax-case stx ()
    [(_ arg ...)
     (with-syntax ()
       #`(begin
           #,@(for/list ([ack (syntax->list #'(arg ...))])
                (with-syntax ([report-name (format-id stx "report-msa/~a" ack)]
                              [format-name (format-id stx "format-msa/~a" ack)]
                              [ack-type ack])

                  #`(begin
                      ;; Provide report-msa/xx
                      (define (report-name txlog)
                        (seg-ref=? txlog 'msa 1 (sym->str 'ack-type)))

                      ;; Provide format-msa/xx
                      (define (format-name msa)
                        (define (format-detail dt io id ack ref-id text)
                          (~a (~a dt #:width 23 #:align 'center)
                              (~a io #:width 2 #:align 'left)
                              (~a id #:width 38 #:align 'right)
                              (~a ack #:width 2 #:align 'left)
                              (~a ref-id #:width 38 #:align 'right)
                              (~a text  #:align 'left)
                              #:separator "  "))
                        (define msa-type (sym->str 'ack-type))
                        (define msa-count (length msa))
                        (define sep (string-repeat 3 " "))
                        (define header (~a (format-detail "Date/Time"
                                                          "IO"
                                                          "Message Control ID"
                                                          "AK"
                                                          "Original Msg Ctrl ID"
                                                          "Text Message")
                                           (format-detail (string-repeat 23 "-")
                                                          (string-repeat 2 "-")
                                                          (string-repeat 38 "-")
                                                          (string-repeat 2 "-")
                                                          (string-repeat 38 "-")
                                                          (string-repeat 20 "-"))
                                           #:separator "\n"))
                        (~a (~a "MSA" msa-type "Responses\n\n" #:separator " ")
                            (~a "Total " msa-type ": " msa-count "\n\n")
                            header
                            (for/fold ([str ""]) ([d (in-list msa)])
                              (~a str
                                  (format-detail (format-date (detail-dt d))
                                                 (detail-io d)
                                                 (detail-id d)
                                                 (second (detail-val d))
                                                 (third (detail-val d))
                                                 (fourth (detail-val d)))
                                  #:separator "\n")))))))))]))
(define--report-msa aa ae ar ca ce cr)

(module+ test
  ;; TODO: Formatting and report tests
  )

;;;; Auxiliary Functions
;; -------------------------------------------------------------------------------------------------

;; Get MSH element. Will fail if (third lst) does not contain an HL7 message.
;; NOTE: MSH pos is 1 less than the sequence number.
;; TODO: Handle error
(define (get-msh-elem lst pos)
  (list-ref (first (third lst)) pos))

(define (get-msg-id lst) (get-msh-elem lst 9))

(define (get-msg-type lst) (get-msh-elem lst 8))

;; Create detail using default value sources for dt, io, id. Will fail if lst
;; does not contain an HL7 message.
;; TODO: Handle error
(define (create-detail lst proc)
  (detail (first lst) (second lst) (get-msg-id lst) proc))

(define (datetime< d1 d2)
  (< (detail-dt d1) (detail-dt d2)))

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

;; Convert lowercase symbols and strings to uppercase strings. This allows "MSA"
;; to be referred to as 'msa or "msa".
(define (sym->str s)
  (cond [(symbol? s) (string-upcase (symbol->string s))]
        [(string? s) (string-upcase s)]
        [else s]))

(module+ test
  (test-equal? "Date Format" (format-date 20160102030405678) "2016-01-02 03:04:05.678")
  (test-equal? "Date Format: Unknown" (format-date 20160102) "20160102")
  (test-equal? "Get MsgCtrl ID" (get-msg-id (second tx0)) "1")

  ;; (test-equal? "Get MsgCtrl ID: Invalid" (get-msg-id (first tx0)) ...)

  (test-equal? "Create Detail" (create-detail (second tx0) (first (third (second tx0))))
               (detail (first (second tx0))  ; dt
                       (second (second tx0)) ; io
                       (get-msg-id (second tx0)) ; id
                       (first (third (second tx0))))) ; val
  #;(test-equal? "Create Detail: Invalid" (create-detail (first tx0) (third (first tx0)))
                 ...)

  (test-equal? "Symbol to String: lowercase" (sym->str 'ab-cde) "AB-CDE")
  (test-equal? "Symbol to String: uppercase" (sym->str 'AB-CDE) "AB-CDE")
  (test-equal? "String to String: lowercase" (sym->str "ab-cde") "AB-CDE")
  (test-equal? "String to String: uppercase" (sym->str "AB-CDE") "AB-CDE")

  (define d1 (detail 20140110180307135 "" "" '()))
  (define d2 (detail 20141210180308692 "" "" '()))
  (test-false "datetime< False" (datetime< d2 d1))
  (test-true "datetime< True" (datetime< d1 d2)))

;; TODO: Remove before release
(define tx0
  '((20170110180250650 "XX" (("Made Connection")))
    (20170110180257640 "Rx" (("MSH" "^~\\&" "Abbott Point of Care" "Abbott Point of Care" "" "" "20120327153803-04:00" "" "ORU^R30^ORU-R30" "1" "P" "2.6")
                             ("PID" "1" "" "PatientID-A01-01" "" "LastName-A01-01^FirstName^MiddleName" "" "19000101" "U" "" "" "" "" "" "" "" "" "" "PatientAccount-A01-01" "SSN")
                             ("ORC" "NW")
                             ("OBR" "1" "" "" "E3+")
                             ("OBX" "1" "" "495412f2-2a1b-4b8b-b600-7e0ab70421a6^NA" "" "143" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "2" "" "8c3e9ddf-9a86-473e-8c32-6797c491c809^K" "" "4.2" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "3" "" "7ab86ff9-313b-4bea-a13e-a416a8f5e249^HCT" "" "60" "%PCV" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "4" "" "c135d903-889f-4fd1-add9-6460eaf37dbd^HB" "" "20.4" "g/dL" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("NTE" "1" "" "" "Allen's Test=Fail" "" "20120327153803-04:00")
                             ("NTE" "2" "" "" "PtTemp=98.6 F" "" "20120327153803-04:00")
                             ("NTE" "3" "" "" "DelSys=Bagging" "" "20120327153803-04:00")
                             ("NTE" "4" "" "" "CPB=No" "" "20120327153803-04:00")
                             ("NTE" "5" "" "" "Site=L Femoral" "" "20120327153803-04:00")
                             ("NTE" "6" "" "" "Sample Type=MIX" "" "20120327153803-04:00")
                             ("NTE" "7" "" "" "DSN=A01" "" "20120327153803-04:00")))
    (20160110180257979 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180257" "" "ACK" "5676" "P" "2.6")
                             ("MSA" "CA" "1" "PatientID-A01-01 %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%% %%%")))
    (20150110180259198 "Rx" (("MSH" "^~\\&" "Abbott Point of Care" "Abbott Point of Care" "" "" "20120327153803-04:00" "" "ORU^R30^ORU-R30" "2" "P" "2.6")
                             ("PID" "1" "" "PatientID-A01-01" "" "LastName-A01-02^FirstName^MiddleName" "" "19000101" "U" "" "" "" "" "" "" "" "" "" "PatientAccount-A01-02" "SSN")
                             ("ORC" "NW")
                             ("OBR" "1" "" "" "E3+")
                             ("OBX" "1" "" "495412f2-2a1b-4b8b-b600-7e0ab70421a6^NA" "" "143" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "2" "" "8c3e9ddf-9a86-473e-8c32-6797c491c809^K" "" "4.2" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "3" "" "7ab86ff9-313b-4bea-a13e-a416a8f5e249^HCT" "" "60" "%PCV" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "4" "" "c135d903-889f-4fd1-add9-6460eaf37dbd^HB" "" "20.4" "g/dL" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("NTE" "1" "" "" "Allen's Test=Fail" "" "20120327153803-04:00")
                             ("NTE" "2" "" "" "PtTemp=98.6 F" "" "20120327153803-04:00")
                             ("NTE" "3" "" "" "DelSys=Bagging" "" "20120327153803-04:00")
                             ("NTE" "4" "" "" "CPB=No" "" "20120327153803-04:00")
                             ("NTE" "5" "" "" "Site=L Femoral" "" "20120327153803-04:00")
                             ("NTE" "6" "" "" "Sample Type=MIX" "" "20120327153803-04:00")
                             ("NTE" "7" "" "" "DSN=A01" "" "20120327153803-04:00")))
    (20170110180259539 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180259" "" "ACK" "5677" "P" "2.6")
                             ("MSA" "CA" "2" "PatientID-A01-01")))
    (20170110180301499 "Rx" (("MSH" "^~\\&" "Abbott Point of Care" "Abbott Point of Care" "" "" "20120327153803-04:00" "" "ORU^R30^ORU-R30" "3" "P" "2.6")
                             ("PID" "1" "" "PatientID-A01-03" "" "LastName-A01-03^FirstName^MiddleName" "" "19000101" "U" "" "" "" "" "" "" "" "" "" "PatientAccount-A01-03" "SSN")
                             ("ORC" "NW")
                             ("OBR" "1" "" "" "E3+")
                             ("OBX" "1" "" "495412f2-2a1b-4b8b-b600-7e0ab70421a6^NA" "" "143" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "2" "" "8c3e9ddf-9a86-473e-8c32-6797c491c809^K" "" "4.2" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "3" "" "7ab86ff9-313b-4bea-a13e-a416a8f5e249^HCT" "" "60" "%PCV" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "4" "" "c135d903-889f-4fd1-add9-6460eaf37dbd^HB" "" "20.4" "g/dL" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("NTE" "1" "" "" "Allen's Test=Fail" "" "20120327153803-04:00")
                             ("NTE" "2" "" "" "PtTemp=98.6 F" "" "20120327153803-04:00")
                             ("NTE" "3" "" "" "DelSys=Bagging" "" "20120327153803-04:00")
                             ("NTE" "4" "" "" "CPB=No" "" "20120327153803-04:00")
                             ("NTE" "5" "" "" "Site=L Femoral" "" "20120327153803-04:00")
                             ("NTE" "6" "" "" "Sample Type=MIX" "" "20120327153803-04:00")
                             ("NTE" "7" "" "" "DSN=A01" "" "20120327153803-04:00")))
    (20170110180301835 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180301" "" "ACK" "5678" "P" "2.6")
                             ("MSA" "CA" "3" "PatientID-A01-03")))
    (20170110180303781 "Rx" (("MSH" "^~\\&" "Abbott Point of Care" "Abbott Point of Care" "" "" "20120327153803-04:00" "" "ORU^R30^ORU-R30" "4" "P" "2.6")
                             ("PID" "1" "" "PatientID-A01-04" "" "LastName-A01-04^FirstName^MiddleName" "" "19000101" "U" "" "" "" "" "" "" "" "" "" "PatientAccount-A01-04" "SSN")
                             ("ORC" "NW")
                             ("OBR" "1" "" "" "E3+")
                             ("OBX" "1" "" "495412f2-2a1b-4b8b-b600-7e0ab70421a6^NA" "" "143" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "2" "" "8c3e9ddf-9a86-473e-8c32-6797c491c809^K" "" "4.2" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "3" "" "7ab86ff9-313b-4bea-a13e-a416a8f5e249^HCT" "" "60" "%PCV" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "4" "" "c135d903-889f-4fd1-add9-6460eaf37dbd^HB" "" "20.4" "g/dL" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("NTE" "1" "" "" "Allen's Test=Fail" "" "20120327153803-04:00")
                             ("NTE" "2" "" "" "PtTemp=98.6 F" "" "20120327153803-04:00")
                             ("NTE" "3" "" "" "DelSys=Bagging" "" "20120327153803-04:00")
                             ("NTE" "4" "" "" "CPB=No" "" "20120327153803-04:00")
                             ("NTE" "5" "" "" "Site=L Femoral" "" "20120327153803-04:00")
                             ("NTE" "6" "" "" "Sample Type=MIX" "" "20120327153803-04:00")
                             ("NTE" "7" "" "" "DSN=A01" "" "20120327153803-04:00")))
    (20170110180304096 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180303" "" "ACK" "5679" "P" "2.6")
                             ("MSA" "CA" "4" "PatientID-A01-04")))
    (20170110180305403 "Rx" (("MSH" "^~\\&" "Abbott Point of Care" "Abbott Point of Care" "" "" "20120327153803-04:00" "" "ORU^R30^ORU-R30" "5" "P" "2.6")
                             ("PID" "1" "" "PatientID-A01-05" "" "LastName-A01-05^FirstName^MiddleName" "" "19000101" "U" "" "" "" "" "" "" "" "" "" "PatientAccount-A01-05" "SSN")
                             ("ORC" "NW")
                             ("OBR" "1" "" "" "E3+")
                             ("OBX" "1" "" "495412f2-2a1b-4b8b-b600-7e0ab70421a6^NA" "" "143" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "2" "" "8c3e9ddf-9a86-473e-8c32-6797c491c809^K" "" "4.2" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "3" "" "7ab86ff9-313b-4bea-a13e-a416a8f5e249^HCT" "" "60" "%PCV" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "4" "" "c135d903-889f-4fd1-add9-6460eaf37dbd^HB" "" "20.4" "g/dL" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("NTE" "1" "" "" "Allen's Test=Fail" "" "20120327153803-04:00")
                             ("NTE" "2" "" "" "PtTemp=98.6 F" "" "20120327153803-04:00")
                             ("NTE" "3" "" "" "DelSys=Bagging" "" "20120327153803-04:00")
                             ("NTE" "4" "" "" "CPB=No" "" "20120327153803-04:00")
                             ("NTE" "5" "" "" "Site=L Femoral" "" "20120327153803-04:00")
                             ("NTE" "6" "" "" "Sample Type=MIX" "" "20120327153803-04:00")
                             ("NTE" "7" "" "" "DSN=A01" "" "20120327153803-04:00")))
    (20170110180305796 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180305" "" "ACK" "5680" "P" "2.6")
                             ("MSA" "CA" "5" "PatientID-A01-05")))
    (20170110180307026 "Rx" (("MSH" "^~\\&" "Abbott Point of Care" "Abbott Point of Care" "" "" "20120327153803-04:00" "" "ORU^R30^ORU-R30" "6" "P" "2.6")
                             ("PID" "1" "" "PatientID-A01-06" "" "LastName-A01-06^FirstName^MiddleName" "" "19000101" "U" "" "" "" "" "" "" "" "" "" "PatientAccount-A01-06" "SSN")
                             ("ORC" "NW")
                             ("OBR" "1" "" "" "E3+")
                             ("OBX" "1" "" "495412f2-2a1b-4b8b-b600-7e0ab70421a6^NA" "" "143" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "2" "" "8c3e9ddf-9a86-473e-8c32-6797c491c809^K" "" "4.2" "mmol/L" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "3" "" "7ab86ff9-313b-4bea-a13e-a416a8f5e249^HCT" "" "60" "%PCV" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("OBX" "4" "" "c135d903-889f-4fd1-add9-6460eaf37dbd^HB" "" "20.4" "g/dL" "" "" "" "" "F" "" "" "" "" "" "" "" "20120327153803-04:00" "MIX")
                             ("NTE" "1" "" "" "Allen's Test=Fail" "" "20120327153803-04:00")
                             ("NTE" "2" "" "" "PtTemp=98.6 F" "" "20120327153803-04:00")
                             ("NTE" "3" "" "" "DelSys=Bagging" "" "20120327153803-04:00")
                             ("NTE" "4" "" "" "CPB=No" "" "20120327153803-04:00")
                             ("NTE" "5" "" "" "Site=L Femoral" "" "20120327153803-04:00")
                             ("NTE" "6" "" "" "Sample Type=MIX" "" "20120327153803-04:00")
                             ("NTE" "7" "" "" "DSN=A01" "" "20120327153803-04:00")))
    (20140110180307135 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180307" "" "ACK^R33^ACK-R33" "5681" "P" "2.6" "" "" "" "" "AL")
                             ("MSA" "AA" "1" "SpecimenID-A01-01")))
    (20170110180307471 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180307" "" "ACK" "5682" "P" "2.6")
                             ("MSA" "CA" "6" "PatientID-A01-06")))
    (20141210180308692 "Tx" (("MSH" "^~\\&" "JResultNet" "JResultNet" "Abbott Point of Care" "Abbott Point of Care" "20170110180308" "" "ACK^R33^ACK-R33" "5683" "P" "2.6" "" "" "" "" "AL")
                             ("MSA" "AA" "2" "SpecimenID-A01-01")))))

