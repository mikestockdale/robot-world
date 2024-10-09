#lang racket

(require web-server/servlet web-server/servlet-env)
(require "entity.rkt" "location.rkt" "world.rkt" "remote-world.rkt")

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
   (("hello") connect)
   (else error)))

(define (execs request list)
  (response/xexpr (remote-execs world list)))

(define (connect request)
  (response/xexpr (remote-connect world)))

(define (draw request)
  (response/xexpr (remote-draw world)))

(define (error request) (response/xexpr "error"))

(define (start request) (dispatch request))

(populate-world)
(serve/servlet start
               #:port 8080
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:launch-browser? #f)
