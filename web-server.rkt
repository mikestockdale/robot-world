#lang racket

(require web-server/servlet web-server/servlet-env)
(require "command.rkt" "setup.rkt")

(define world (setup-world))

(define-values (dispatch url)
  (dispatch-rules
   (("execs" (string-arg)) execs)
   (("draw") draw)
   (("hello") hello)
   (else error)))

(define (execs request list)
  (response/xexpr (execute-command-list world list)))

(define (hello request)
  (response/xexpr (execute-hello world)))

(define (draw request)
  (response/xexpr (execute-draw world)))

(define (error request) (response/xexpr "error"))

(define (start request) (dispatch request))

(setup-blocks world)
(serve/servlet start
               #:port 8080
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:launch-browser? #f)
