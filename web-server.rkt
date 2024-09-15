#lang racket

(require web-server/servlet web-server/servlet-env)
(require "remote-world.rkt")

(define-values (dispatch url)
  (dispatch-rules
   (("add" (integer-arg) (integer-arg) (integer-arg)) add)
   (("move" (integer-arg) (integer-arg)) move)
   (("take" (integer-arg) (integer-arg)) take)
   (("drop" (integer-arg) (integer-arg)) drop)
   (("draw") draw)
   (else error)))

(define (add request type x y)
  (response/xexpr (remote-add type x y)))

(define (drop request id direction)
  (response/xexpr (remote-drop id direction)))

(define (move request id direction)
  (response/xexpr (remote-move id direction)))

(define (take request bot-id block-id)
  (response/xexpr (remote-take bot-id block-id)))
  
(define (draw request)
  (response/xexpr (remote-draw)))

(define (error request) (response/xexpr "error"))

(define (start request) (dispatch request))

(populate-world)
(serve/servlet start
               #:port 8080
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:launch-browser? #f)
