#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline
         racket/format
         srfi/13)

(module+ test
  (require rackunit))

(define cs-length
  (make-parameter
   256
   (lambda (l)
     (cond [(and (string? l) (exact-positive-integer? (string->number l))) (string->number l)]
           [(exact-positive-integer? l) l]
           [else
            (raise-user-error "The length must be specified as a positive integer.\n<length>:" l)]))))

(define cs-marker
  (make-parameter
   "*"
   (lambda (m)
     (cond [(>= (string-length m) 1) m]
           [else
            (raise-user-error
             "The marker symbol must be a string of one or more characters.\n<marker>:"
             m)]))))

(module+ main
  (command-line
   #:program "counter-string"
   #:once-each
   [("--length" "-l")
    length
    ("Default: 256" "Specify the length of the counter-string.")
    (cs-length length)]
   [("--marker" "-m")
    marker
    ("Default: *" "Specify the marker character.")
    (cs-marker marker)]
   #:ps "
Creates a counter-string where the last integer indicates the character position
of the final marker; e.g. \"*3*5*7*9*12*15*18*21*24*\".\n")
  (printf "~a~n" (counter-string)))

(define (counter-string [l (cs-length)] #:mark [m (cs-marker)])
  (unless (equal? l (cs-length)) (cs-length l))
  (unless (equal? m (cs-marker)) (cs-marker m))
  (build-counter-string))

(define (build-counter-string [count (cs-length)] [acc ""])
  (cond [(almost-full? count acc) (finish-counter-string acc)]
        [else (append-count+mark count acc)]))

(module+ test
  (check-exn exn:fail:user? (lambda () (counter-string 0)))
  (check-exn exn:fail:user? (lambda () (counter-string "0")))
  (check-exn exn:fail:user? (lambda () (counter-string "foo")))
  (check-equal? (counter-string 1) "*")
  (check-equal? (counter-string 2) "2*")
  (check-equal? (counter-string 3) "*3*")
  (check-equal? (counter-string 10 #:mark ">") ">3>5>7>10>")
  (check-equal? (counter-string 2 #:mark "@#$%") "@@")
  (check-equal? (counter-string 10) "@@@@10%$#@"))

(define (finish-counter-string acc)
  (string-reverse
   (~a acc (make-string
            (- (cs-length) (string-length acc))
            (string-ref (cs-marker) 0)))))

(define (append-count+mark count acc)
  (define count-string (number->string count))
  (build-counter-string
   (- count
      (string-length count-string)
      (string-length (cs-marker)))
   (string-append acc (cs-marker) (string-reverse count-string))))

(define (almost-full? count str)
  (define string+count+mark-length (+ (string-length str)
                                      (string-length (number->string count))
                                      (string-length (cs-marker))))
  (> string+count+mark-length (cs-length)))

(module+ test
  (cs-marker "*")
  (cs-length 1)
  (check-equal? (almost-full? 1 "") #t)
  (cs-length 2)
  (check-equal? (almost-full? 2 "") #f)
  (check-equal? (almost-full? 0 "*2") #t)
  (cs-length 5)
  (check-equal? (almost-full? 3 "*5") #f)
  (check-equal? (almost-full? 1 "*5*3") #t))
