#lang racket

(provide make-cargos cargo-ref load-cargo! unload-cargo!)

(define (make-cargos) (make-hash))

(define (cargo-ref cargos id) (hash-ref cargos id #f))

(define (load-cargo! cargos id entity)
  (hash-set! cargos id entity))

(define (unload-cargo! cargos id)
  (let ([entity (cargo-ref cargos id)])
    (when entity
      (hash-remove! cargos id))
    entity))

(module+ test
  (require rackunit "shared.rkt")

  (test-case
   "load"
   (let ([cargos (make-cargos)]
         [block (entity 102 type-block (location 1 2))])
     (load-cargo! cargos 101 block )
     (check-equal? (cargo-ref cargos 101) block)))

  (test-case
   "unload"
   (let ([cargos (make-cargos)]
         [block (entity 102 type-block (location 1 2))])
     (load-cargo! cargos 101 block )
     (check-equal? (unload-cargo! cargos 101) block)
     (check-false (cargo-ref cargos 101)))))
