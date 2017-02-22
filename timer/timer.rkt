#!/bin/sh
#|
exec racket -tm "$0" ${1+"$@"}
|#
#lang racket/base
(require racket/list
         racket/system)
(module+ test (require rackunit))
(provide main)

;; TODO: Move all the OS-specific pieces into a configuration file.

;; PARAMETERS & VARIABLES

;; Time Parsing
(define HRS          (make-parameter #f))
(define MIN          (make-parameter #f))
(define SEC          (make-parameter #f))
(define TIME         (make-parameter #f))

;; OS
(define OS           (system-type   'os))

;; Visual Notification
(define NOTIFY-TITLE "TIMER DONE!")
(define NOTIFY-APP   (make-parameter #f))
(define NOTIFY-MSG   (make-parameter #f))
(define NOTIFY-CMD   (make-parameter #f))

;; Sound Notification
(define SND-APP      (make-parameter #f))
(define SND-FILE     (make-parameter #f))
(define SND-CMD      (make-parameter #f))


;; OS SETUP

(define (set-os-params os)
  (cond [(eq? os 'unix)    (unix)]
        [(eq? os 'windows) (windows)]
        [(eq? os 'macosx)  (mac)]))

(define (unix)
  (NOTIFY-APP (read-line (first (process "which notify-send"))))
  (NOTIFY-MSG (string-append "A timer for " (times->string) " has finished."))
  (NOTIFY-CMD (string-append (NOTIFY-APP) " -u critical '" NOTIFY-TITLE "' '" (NOTIFY-MSG) "'"))
  (SND-APP    (read-line (first (process "which play"))))
  (SND-FILE   "~/Dropbox/sounds/click.wav")
  (SND-CMD    (string-append (SND-APP) " " (SND-FILE))))

;; TODO: Add sound notification
(define (windows)
  (NOTIFY-APP "c:/bin/notifu/notifu.exe")
  (NOTIFY-MSG (string-append "A timer for " (times->string) " has finished."))
  (NOTIFY-CMD (string-append (NOTIFY-APP) " /t warn /p \"" NOTIFY-TITLE "\" /m \"" (NOTIFY-MSG) "\"" )))

;; TODO
(define (mac) empty)

(define (times->string)
  (string-append (if (HRS) (string-append (HRS) "h") "")
                 (if (MIN) (string-append (MIN) "m") "")
                 (if (SEC) (string-append (SEC) "s")  "")))


;; PARSE TIME

(define (get-time str)
  ;; hrs (?:([[:digit:]]+)\\s*h)?\\s*
  ;; min (?:([[:digit:]]+)\\s*m)?\\s*
  ;; sec (?:([[:digit:]]+)\\s*s?)?\\s*$
  (define time-rx
    (pregexp
     "^\\s*(?:([[:digit:]]+)\\s*h)?\\s*(?:([[:digit:]]+)\\s*m)?\\s*(?:([[:digit:]]+)\\s*s?)?\\s*$"))
  (define time-list (regexp-match time-rx str))
  (HRS (second time-list))
  (MIN (third  time-list))
  (SEC (fourth time-list))
  (define (hours/str->seconds   hours)   (if hours   (* (string->number hours)  3600) 0))
  (define (minutes/str->seconds minutes) (if minutes (* (string->number minutes)  60) 0))
  (define (seconds/str->seconds seconds) (if seconds    (string->number seconds)      0))
  (+ (hours/str->seconds (HRS)) (minutes/str->seconds (MIN)) (seconds/str->seconds (SEC))))

(module+ test
  (check-equal? (get-time "") 0)
  (check-equal? (get-time "300") 300)
  (check-equal? (get-time " 3s")   3)
  (check-equal? (get-time " 1m ") 60)
  (check-equal? (get-time " 1h ") (* 60 60 1))
  (check-equal? (get-time "30m 20") (+ (* 30 60) 20))
  (check-equal? (get-time "1h1s") (+ (* 60 60 1) 1))
  (check-equal? (get-time "9 h30 m6 s") (+ (* 60 60 9) (* 30 60) 6)))


;; MAIN

(define main
  (case-lambda
    [(time)
     (TIME (get-time time))
     (set-os-params OS)
     (sleep (TIME))
     (system (NOTIFY-CMD))
     (repeat (λ () (system (SND-CMD))) 3)]
    [() (printf " USAGE: timer [0h][0m][0[s]]\n")
        (printf "   e.g. timer 30\n")
        (printf "        timer 5m20\n")
        (printf "        timer 1h30m26s\n")]))

(module+ test
  ;; TODO: Learn how to test command line arguments and system calls
  )

(define (repeat f count) (for ([i (in-range count)]) (f)))

;; Local Variables:
;; mode: racket
;; End:
