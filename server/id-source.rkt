#lang racket

(provide make-id-source new-id!)

(struct id-source ([next-id #:mutable]))
(define (make-id-source) (id-source 101))

(define (new-id! id-source)
  (let ([new-id (id-source-next-id id-source)])
    (set-id-source-next-id! id-source (add1 (id-source-next-id id-source)))
    new-id))

(module+ test
  (require rackunit)

  (test-case
   "generates ids"
   (let ([source (make-id-source)])
     (check-equal? (new-id! source) 101)
     (check-equal? (new-id! source) 102)
     (check-equal? (new-id! source) 103)))

  )