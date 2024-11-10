#lang racket

(provide (struct-out request) (struct-out command)
         request-execute-commands request-draw request-hello
         drop-command move-command take-command)

(struct request (type commands) #:prefab)
(struct command (type id parameter) #:prefab)

(define request-execute-commands 0)
(define request-draw 1)
(define request-hello 2)

(define drop-command 0)
(define move-command 1)
(define take-command 2)
