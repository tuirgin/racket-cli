#!/usr/bin/env racket
#lang racket/base

(require gregor
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/path
         racket/string
         threading)

(module+ test
  (require rackunit))

(define count# (make-parameter 10))
(define template-path (make-parameter #f))
(define output-path (make-parameter "./filegen-output"))
(define time (make-parameter (abs (current-milliseconds))))
(define time-string (make-parameter (~t (now) "YYYYMMddHHmmssSSSSSSSSSSSSSSSSSSSSSSSSSS")))
(define time+num-length (make-parameter 20))

;; TODO: Add option to start incrementing from a specified number.

(module+ main
  (command-line
   #:program "filegen"
   #:once-each
   [("--count" "-c")
    file-count
    ("Default: 10"
     "Specify the number of files needed.")
    (count# (string->number file-count))]
   [("--width" "-w")
    width
    ("Default: 20"
     "Specify the character width of the @|time+num| value.")
    (time+num-length (string->number width))]
   [("--output" "-o")
    output-directory
    ("Default: filegen-output"
     "Specify the output directory.")
    (output-path output-directory)]
   #:ps "
 The template is a plain text file containing arbitrary text with optional
 replacement patterns.

 The following replacement patterns are available:

 Pattern          Description
 ---------------  ---------------------------------------------------------
 @|num|           Insert incremental number starting with \"1\".

 @|time+num|      Insert incremental number based on the current time.
                  This allows for substitutions which will remain unique
                  across multiple runs of filegen."
   ""
   #:args (template)
   (template-path template))
  (filegen))

(define (filegen)
  (render (file->string (template-path))))

(define (render str)
  (define count#-width (string-length (number->string (count#))))
  (for ([x (in-range (count#))])
    (define x-padded (~r (add1 x) #:min-width count#-width #:pad-string "0"))
    (render/output (munge str x-padded) x-padded)))

(module+ test
  (test-case
      "Render: 1-wide"
    (define in "MSH|^~\\&|SendApp@|num||SendFac|RcvApp|RcvFac|20141103123456||ADT^A01^ADT_A01|@|time+num||P|2.6PID|1||PID-@|num|||LN-@|num|^FN@|num|^MN@|num|||19010101|")
    (define out5 "MSH|^~\\&|SendApp5|SendFac|RcvApp|RcvFac|20141103123456||ADT^A01^ADT_A01|2017031315|P|2.6PID|1||PID-5||LN-5^FN5^MN5||19010101|")
    (time-string "2017031314065476781396500000000000000000")
    (time+num-length 10)
    (output-path (~a "tmp-test-dir-render-" (current-seconds)))
    (template-path (~a "tmp-test-template-render-" (current-seconds) ".txt"))
    (count# 5)
    (define file-path
      (simplify-path
       (build-path (output-path) (~a (path->string (path-replace-extension (template-path) #""))
                                     "-5.txt"))))
    (after
     (render in)
     (directory-exists? (output-path))
     (file-exists? file-path)
     (check-equal? (file->string file-path) out5)
     (delete-directory/files (path-only file-path))))
  (test-case
      "Render: 2-wide"
    (time 1487698079510)
    (define in "MSH|^~\\&|SendApp@|num||SendFac|RcvApp|RcvFac|20141103123456||ADT^A01^ADT_A01|@|time+num||P|2.6PID|1||PID-@|num|||LN-@|num|^FN@|num|^MN@|num|||19010101|")
    (define out05 "MSH|^~\\&|SendApp05|SendFac|RcvApp|RcvFac|20141103123456||ADT^A01^ADT_A01|201703131406547678139650000005|P|2.6PID|1||PID-05||LN-05^FN05^MN05||19010101|")
    (time-string "2017031314065476781396500000000000000000")
    (time+num-length 30)
    (output-path (~a "tmp-test-dir-render-" (current-seconds)))
    (template-path (~a "tmp-test-template-render-" (current-seconds) ".txt"))
    (count# 10)
    (define file-path
      (simplify-path
       (build-path (output-path) (~a (path->string (path-replace-extension (template-path) #""))
                                   "-05.txt"))))
    (after
     (render in)
     (directory-exists? (output-path))
     (file-exists? file-path)
     (check-equal? (file->string file-path) out05)
     (delete-directory/files (path-only file-path)))))

(define (munge str count-string)
  (define (@num s)
    (string-replace s "@|num|" count-string))
  (define (@time+num s)
    (string-replace s "@|time+num|"
                    (~a (substring (time-string) 0 (- (time+num-length)
                                                      (string-length count-string)))
                        count-string)))
  (~> str @time+num @num))

(module+ test
  (test-equal? "Process template: empty" (munge "" "10") "")
  (test-equal? "Process template: multiple match"
               (munge "^@|num|\r^^@|num|^^\b@|num|^" "1000")
               "^1000\r^^1000^^\b1000^")
  (time-string "201703131406547678139650000000000000000")
  (time+num-length 25)
  (define time+5 "2017031314065476781390005")
  (test-equal? "Process template: time+count"
               (munge "@|num||@|time+num|" "0005")
               (~a "0005|" time+5)))

(define (render/output str count-string)
  (define file-name
    (~a (path->string (path-replace-extension (template-path) #""))
        "-"
        count-string
        (let ([ext (path-get-extension (template-path))])
          (if ext ext ""))))
  (define out-path (simplify-path (build-path (output-path) file-name) #f))
  (define out-dir (path-only out-path))
  (make-parent-directory* out-path)
  (display-to-file str out-path))

(module+ test
  (test-case
      "Create files with suffix"
    (output-path (~a "tmp-test-dir-output-" (current-seconds)))
    (template-path (~a "tmp-test-template-output-" (current-seconds) ".txt"))
    (define file-path
      (simplify-path
       (build-path (output-path) (~a (path->string (path-replace-extension (template-path) #""))
                                     "-10.txt"))))
    (after
     (render/output "write-output-test" 10)
     (test-true "Create files with suffix: directory" (directory-exists? (output-path)))
     (test-true "Create files with suffix: file" (file-exists? file-path))
     (delete-directory/files (path-only file-path))))

  (test-case
      "Create files without suffix"
    (template-path (~a "tmp-test-template-output-" (current-seconds)))
    (define file-path
      (simplify-path
       (build-path (output-path) (~a (path->string (path-replace-extension (template-path) #""))
                                     "-10"))))
    (after
     (render/output "write-output-test" 10)
     (test-true "Create files without suffix: directory" (directory-exists? (output-path)))
     (test-true "Create files without suffix: file" (file-exists? file-path))
     (delete-directory/files (path-only file-path)))))
