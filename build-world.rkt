#lang racket

(provide build-world)
(require "entity.rkt" "location.rkt" "server.rkt" "world.rkt")

(define (build-world spec procedure)
  (let* ([world (make-world (first spec))]
         [server (connect-server world)]
         [entities
          (for/hash ([entity-spec (rest spec)])
            (values
             (first entity-spec)
             (add-entity! world
                          (if (string-prefix? (first entity-spec) "bot") type-bot type-block)
                          (location (second entity-spec) (third entity-spec)))))])
    (define (entity-ref key) (hash-ref entities key))
    (procedure world server entity-ref)))
