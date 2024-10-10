#lang racket

(require web-server/servlet web-server/servlet-env)
(require "command.rkt" "entity.rkt" "location.rkt" "world.rkt")

(define world (make-world 50))

(define (populate-world)
  (for ([x 5])
    (for ([y 5])
      (add-entity! world type-block
                   (location (+ 5 (* x 10)) (+ 5 (* y 10))))))
  #t)

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

(populate-world)
(serve/servlet start
               #:port 8080
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:launch-browser? #f)
