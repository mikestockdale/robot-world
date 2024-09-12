#lang racket

(provide gen:server add-bot! drop-block! move-bot! take-block!)
(require racket/generic)

(define-generics server
  (add-bot! server location)
  (drop-block! server bot-id block-id)
  (move-bot! server bot-id location)
  (take-block! server bot-id direction))
