#!/usr/bin/env racket
#lang racket/base

(require gregor
         racket/cmdline)

(module+ test
  (require rackunit))

(define DATE-FORMAT "yyyy-MM-dd")

(define mod-years (make-parameter 0))
(define mod-months (make-parameter 0))
(define mod-days (make-parameter 0))
(define base-date (make-parameter (today)))

(module+ main
  (command-line
   #:program "get-date"
   #:once-each
   [("--start-date" "-s")
    date
    ("Default: (today)"
     "Date from which to calculate the relative date in ISO 8601 format;"
     "i.e. \"yyyy-mm-dd\"")
    (base-date (iso8601->date date))]
   [("--years" "-y")
    years
    ("Default: 0" "Years from current date")
    (mod-years (string->number years))]
   [("--months" "-m")
    months
    ("Default: 0" "Months from current date")
    (mod-months (string->number months))]
   [("--days" "-d")
    days
    ("Default: 0" "Days from current date")
    (mod-days (string->number days))]
   )
  (date->iso8601 (get-date #:date (base-date)
                           #:years (mod-years)
                           #:months (mod-months)
                           #:days (mod-days))))

(define (get-date #:date [start-date (base-date)]
                  #:years [years (mod-years)]
                  #:months [months (mod-months)]
                  #:days [days (mod-days)])
  (+days (+months (+years start-date years) months) days))

(module+ test
  (check-equal? (get-date) (today))
  (check-equal? (get-date #:years -1)
                (-years (today) 1))
  (check-equal? (get-date #:months 1)
                (+months (today) 1))
  (check-equal? (get-date #:days -1)
                (-days (today) 1))
  (check-equal? (get-date #:years -3 #:months 5 #:days -90)
                (-years (+months (-days (today) 90) 5) 3))
  (check-equal? (get-date #:date (iso8601->date "2018-06-26")
                          #:years -44)
                (iso8601->date "1974-06-26"))
  (check-equal? (get-date #:date (iso8601->date "1974-06-26")
                          #:years 44
                          #:months 1
                          #:days 15)
                (iso8601->date "2018-08-10"))
  )

(define (date->iso8601 d)
  (~t d DATE-FORMAT))

(module+ test
  (check-equal? (date->iso8601 (iso8601->date "2018-06-26"))
                "2018-06-26"))

