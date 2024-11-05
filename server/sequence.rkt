#lang racket

(provide make-sequence new-id)

(struct sequence (get-next))
(define (make-sequence)
  (let-values ([(_ get-next) (sequence-generate (in-naturals 101))])
    (sequence get-next)))

(define (new-id sequence)
  ((sequence-get-next sequence)))

(module+ test
  (require rackunit)

  (test-case
   "generates ids"
   (let ([sequence (make-sequence)])
     (check-equal? (new-id sequence) 101)
     (check-equal? (new-id sequence) 102)
     (check-equal? (new-id sequence) 103)))

  )