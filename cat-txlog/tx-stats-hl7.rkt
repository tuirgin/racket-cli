#lang racket

(require "cat-txlog.rkt")

(require (for-syntax racket/syntax))

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
  (define separator (string-append "\n\n" (string-repeat 100 "=") "\n\n\n" ))
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
        (~a id #:width 20 #:align 'right)
        (~a vals #:align 'left)
        #:separator "  "))
  (define (format-val lst)
    (cond [(string=? (first lst) "MSA")
           (~a "─ " (string-join lst "|") #:max-width 42 #:limit-marker "..." #:align 'left)]
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
                                    (string-repeat 20 "-")
                                    (string-repeat 42 "-"))
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
        (~a id #:width 20 #:align 'right)
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
                                    (string-repeat 20 "-")
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
                              (~a id #:width 20 #:align 'right)
                              (~a ack #:width 2 #:align 'left)
                              (~a ref-id #:width 20 #:align 'right)
                              (~a text #:max-width 20 #:limit-marker "..." #:align 'left)
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
                                                          (string-repeat 20 "-")
                                                          (string-repeat 2 "-")
                                                          (string-repeat 20 "-")
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

;; Convert lowercase symbols and strings to uppercase strings. This allows "MSA"
;; to be referred to as 'msa or "msa".
(define (sym->str s)
  (cond [(symbol? s) (string-upcase (symbol->string s))]
        [(string? s) (string-upcase s)]
        [else s]))

(define (datetime< d1 d2)
  (< (detail-dt d1) (detail-dt d2)))

(module+ test

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

