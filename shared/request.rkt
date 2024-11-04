#lang racket

(provide request-execute-list request-draw request-hello
         drop-command move-command take-command)

(define request-execute-list 0)
(define request-draw 1)
(define request-hello 2)

(define drop-command 0)
(define move-command 1)
(define take-command 2)
