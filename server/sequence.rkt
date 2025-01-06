#lang racket

(provide make-sequence)
(require "shared.rkt")
(module+ test (require rackunit))

;@title{Sequence}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/server/sequence.rkt" "sequence.rkt"]}
;The @bold{sequence} is a generator for integers.

(define (make-sequence)
  (let-values ([(_ get-next) (sequence-generate (in-naturals 101))])
    get-next))

;The sequence supplies new ids.

(test-case:
 "generates ids"
 (let ([sequence (make-sequence)])
   (check-equal? (sequence) 101)
   (check-equal? (sequence) 102)
   (check-equal? (sequence) 103)))
