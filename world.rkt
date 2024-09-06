#lang racket

(provide make-world add-bot! add-entity! move-bot! locate-bot draw-world
         type-block type-bot
         bot-id)

(require threading)
(require "location.rkt")

(module+ test (require rackunit))

(struct bot (id location))
(struct entity (id type location))
(struct world (size [next-id #:mutable] entities))

(define type-bot 0)
(define type-block 1)
(define type-symbols #(#\O #\B))

(define (make-world size) (world size 101 (make-hash)))

(define (place-entity! world entity)
  (hash-set! (world-entities world) (entity-id entity) entity))
(define (locate-bot world id) (entity-location (entity-ref world id)))
(define (entity-ref world id) (hash-ref (world-entities world) id))

(define (entity-at world location)
  (~>> world world-entities hash-values
       (findf (λ (entity) (equal? (entity-location entity) location)))))

(define (add-entity! world type location)
  (if (is-valid-location? location (world-size world))
      (let ([new-id (world-next-id world)])
        (place-entity! world (entity new-id type location))
        (set-world-next-id! world (+ 1 (world-next-id world)))
        new-id)
      #f))

(define (add-bot! world location)
  (let ([new-id (add-entity! world type-bot location)])
    (if new-id (bot new-id location) #f)))

(define (move-bot! world id direction)
  (let*
      ([old-location (entity-location (entity-ref world id))]
       [new-location (move-location old-location direction)])
    (if (is-valid-location? new-location (world-size world))
        (begin (place-entity! world (entity id type-bot new-location))
               new-location)
        old-location)))

(define (draw-world world)
  (let* ([size (world-size world)]
         [lines (for/vector ([_ size]) (make-string size #\space))])
    
    (define (draw-entity id entity)
      (let ([location (entity-location entity)])
      (string-set!
       (vector-ref lines (- size 1 (location-y location)))
       (location-x location)
       (vector-ref type-symbols (entity-type entity)))))
    
    (hash-for-each (world-entities world) draw-entity)
    lines))

(module+ test
  (test-case
   "bot is created at requested location"
   (let* ([somewhere (location 1 2)]
          [new-bot (add-bot! (make-world 10) somewhere)])
     (check-equal? (bot-location new-bot) somewhere)))

  (test-case
   "block is created at requested location"
   (let* ([world (make-world 10)]
          [somewhere (location 1 2)]
          [id (add-entity! world type-block somewhere)]
          [block (entity-at world somewhere)])
     (check-equal? (entity-id block) id)  
     (check-equal? (entity-type block) type-block)  
     (check-equal? (entity-location block) somewhere)
     (check-equal? (entity-ref world id) block)))  

  (test-case
   "bot is not created at invalid location"
   (let* ([somewhere (location -1 2)]
          [new-bot (add-bot! (make-world 10) somewhere)])
     (check-false new-bot)))

  (test-case
   "bot is created with new id"
   (let* ([world (make-world 10)]
          [first-bot (add-bot! world (location 3 4))]
          [second-bot (add-bot! world (location 5 6))]          )
     (check-not-equal? (bot-id first-bot) (bot-id second-bot))))

  (test-case
   "move bot changes location"
   (let* ([world (make-world 10)]
          [new-bot (add-bot! world (location 5 6))])
     (check-equal? (move-bot! world (bot-id new-bot) direction-north) (location 5 7))
     (check-equal? (move-bot! world (bot-id new-bot) direction-east) (location 6 7))
     (check-equal? (move-bot! world (bot-id new-bot) direction-south) (location 6 6))
     (check-equal? (move-bot! world (bot-id new-bot) direction-west) (location 5 6))))

  (test-case
   "invalid move leaves bot location unchanged"
   (let* ([world (make-world 10)]
          [new-bot (add-bot! world (location 9 9))])
     (check-equal? (move-bot! world (bot-id new-bot) direction-north) (location 9 9)))) 

  (test-case
   "world is drawn as strings"
   (let ([world (make-world 3)])
     (add-entity! world type-bot (location 0 2))
     (add-entity! world type-bot(location 1 1))
     (add-entity! world type-block(location 2 1))
     (check-equal? (draw-world world) #("O  " " OB" "   "))))) 
