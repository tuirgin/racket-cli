#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline
         racket/format
         srfi/13)

(module+ test
  (require rackunit))

(define length (make-parameter 256))
(define marker (make-parameter "*"))

(module+ main
  (command-line
   #:program "counter-string"
   #:once-each
   [("--length" "-l")
    num
    ("Default: 256" "Specify the length of the counter-string.")
    (define n (string->number num))
    (if (exact-positive-integer? n)
        (length n)
        (raise-user-error "The length must be specified as a positive integer.\n<num>:" num))]
   [("--marker" "-m")
    char
    ("Default: *" "Specify the marker character.")
    (if (>= (string-length char) 1)
        (marker char)
        (raise-user-error "The marker symbol must be a string of one or more characters.\n<char>:"
                          char))]
   #:ps "
Creates a counter-string where the last integer indicates the character position
of the final marker; e.g. \"*3*5*7*9*12*15*18*21*24*\".\n")
  (printf "~a~n" (counter-string (length) "")))

(define (counter-string count acc)
  (cond [(almost-full? count acc) (finish-counter-string acc)]
        [else (append-count+mark count acc)]))

(module+ test
  (length 0)
  (check-equal? (counter-string 0 "") "")
  (length 1)
  (check-equal? (counter-string 1 "") "*")
  (length 2)
  (check-equal? (counter-string 2 "") "2*")
  (length 3)
  (check-equal? (counter-string 3 "") "*3*")
  (length 10)
  (marker ">")
  (check-equal? (counter-string 10 "") ">3>5>7>10>")
  (marker "@#$%")
  (length 2)
  (check-equal? (counter-string 2 "") "@@")
  (length 10)
  (check-equal? (counter-string 10 "") "@@@@10%$#@"))

(define (finish-counter-string acc)
  (string-reverse
   (~a acc (make-string
            (- (length) (string-length acc))
            (string-ref (marker) 0)))))

(define (append-count+mark count acc)
  (define count-string (number->string count))
  (counter-string
   (- count
      (string-length count-string)
      (string-length (marker)))
   (string-append acc (marker) (string-reverse count-string))))

(define (almost-full? count str)
  (define length-string+count+mark (+ (string-length str)
                                      (string-length (number->string count))
                                      (string-length (marker))))
  (> length-string+count+mark (length)))

(module+ test
  (marker "*")
  (length 1)
  (check-equal? (almost-full? 1 "") #t)
  (length 2)
  (check-equal? (almost-full? 2 "") #f)
  (check-equal? (almost-full? 0 "*2") #t)
  (length 5)
  (check-equal? (almost-full? 3 "*5") #f)
  (check-equal? (almost-full? 1 "*5*3") #t))
