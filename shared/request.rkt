#lang racket/base

(provide (struct-out request) request-draw request-hello
         request-drop request-move request-take request-transfer)

;@title{Request}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/shared/request.rkt" "request.rkt"]}
;A request from a player client to the server contains a type, entity id and parameter.

(struct request (type id parameter) #:prefab)

;The request types are these:

(define request-drop 0)
(define request-move 1)
(define request-take 2)
(define request-transfer 3)

; Requests may also be just a request name.

(define request-draw "draw")
(define request-hello "hello")