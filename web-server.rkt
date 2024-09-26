#lang racket

(require web-server/servlet web-server/servlet-env)
(require "entity.rkt" "location.rkt" "world.rkt" "remote-world.rkt")

(define world (make-world 50))

(define (populate-world)
  (add-entity! world type-block (location 25 25))
  (add-entity! world type-block (location 35 25))
  (add-entity! world type-block (location 25 35))
  (add-entity! world type-block (location 45 25))
  (add-entity! world type-block (location 25 45))
  (add-entity! world type-block (location 15 25))
  (add-entity! world type-block (location 25 15))
  (add-entity! world type-block (location 5 25))
  (add-entity! world type-block (location 25 5))
  #t)

(define-values (dispatch url)
  (dispatch-rules
   (("add" (integer-arg) (integer-arg) (integer-arg)) add)
   (("exec" (integer-arg) (integer-arg) (integer-arg)) exec)
   (("execs" (string-arg)) execs)
   (("draw") draw)
   (else error)))

(define (execs request list)
  (response/xexpr (remote-execs world list)))

(define (add request type x y)
  (response/xexpr (remote-add world type x y)))

(define (exec request procedure id other)
  (response/xexpr (remote-exec world procedure id other)))

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
