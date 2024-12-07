#lang racket

(provide make-cargos cargo-for-bot load-cargo unload-cargo)

(struct cargos (hash))
(define (make-cargos) (cargos (make-hash)))

(define (cargo-for-bot cargos id) (hash-ref (cargos-hash cargos) id #f))

(define (load-cargo cargos id entity)
  (hash-set! (cargos-hash cargos) id entity))

(define (unload-cargo cargos id)
  (let ([entity (cargo-for-bot cargos id)])
    (when entity
      (hash-remove! (cargos-hash cargos) id))
    entity))

(module+ test
  (require rackunit "shared.rkt")

  (test-case
   "load and retrieve"
   (let ([cargos (make-cargos)]
         [block (entity 102 type-block)])
     (load-cargo cargos 101 block )
     (check-equal? (cargo-for-bot cargos 101) block)))

  (test-case
   "unload"
   (let ([cargos (make-cargos)]
         [block (entity 102 type-block)])
     (load-cargo cargos 101 block )
     (check-equal? (unload-cargo cargos 101) block)
     (check-false (cargo-for-bot cargos 101)))))
