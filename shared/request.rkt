#lang racket

(provide (struct-out request)
         request-drop request-move request-take)

(struct request (type id parameter) #:prefab)

(define request-drop 0)
(define request-move 1)
(define request-take 2)
