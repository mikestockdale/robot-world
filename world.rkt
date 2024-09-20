#lang racket

(provide make-world  entity-ref neighbors draw-entities world-size
         add-entity! move-entity! take-entity! drop-entity!)

(require threading)
(require "direction.rkt" "entity.rkt" "location.rkt")

(module+ test (require rackunit))

(struct world (size [next-id #:mutable] entities))

(define (make-world size) (world size 101 (make-hash)))

(define (place-entity! world entity)
  (hash-set! (world-entities world) (entity-id entity) entity))

(define (entity-ref world id) (hash-ref (world-entities world) id #f))

(define (remove-entity! world id)
  (hash-remove! (world-entities world) id))

(define (entity-at world location)
  (~>> world world-entities hash-values
       (findf (λ (entity) (equal? (entity-location entity) location)))))

(define (add-entity! world type location)
  (if (is-valid-location? location (world-size world))
      (let* ([new-id (world-next-id world)]
             [new-entity (make-entity new-id type location)])
        (place-entity! world new-entity)
        (set-world-next-id! world (+ 1 (world-next-id world)))
        new-entity)
      #f))

(define (move-entity! world id direction)
  (let*
      ([old-entity (entity-ref world id)]
       [new-location (move-direction direction (entity-location old-entity))])
    (if (and (is-valid-location? new-location (world-size world))
             (not (entity-at world new-location)))
        (let ([new-entity (struct-copy entity old-entity [location new-location])])
          (place-entity! world new-entity)
          #t)
        #f)))

(define (take-entity! world id cargo-id)
  (let ([new-entity (struct-copy entity (entity-ref world id) [cargo (entity-ref world cargo-id)])])
    (place-entity! world new-entity)
    (remove-entity! world cargo-id)
    new-entity))

(define (drop-entity! world id direction)
  (let* ([bot (entity-ref world id)]
         [drop-location (move-direction direction (entity-location bot))])
    (place-entity! world
                   (struct-copy entity (entity-cargo bot) [location drop-location]))
    (let ([new-bot (struct-copy entity (entity-ref world id) [cargo #f])])
      (place-entity! world new-bot)
      new-bot)))

(define (neighbors world entity)
  (~>> world world-entities hash-values
       (filter (λ (other) (= (distance (entity-location entity) (entity-location other) ) 1)))))

(define (draw-entities world procedure)

  (define (draw-entity id entity)
    (let ([location (entity-location entity)])
      (procedure (entity-symbol entity)
                 (location-x location)
                 (- (world-size world) 1 (location-y location)))))
  
  (hash-for-each (world-entities world) draw-entity))

(module+ test
  (test-case
   "bot is created at requested location"
   (let* ([world (make-world 10)]
          [somewhere (location 1 2)]
          [new-bot (add-entity! world type-bot somewhere)])
     (check-equal? (entity-location new-bot) somewhere)))

  (test-case
   "block is created at requested location"
   (let* ([world (make-world 10)]
          [somewhere (location 1 2)]
          [block (add-entity! world type-block somewhere)])
     (check-equal? (entity-type block) type-block)  
     (check-equal? (entity-location block) somewhere)
     (check-equal? (entity-ref world (entity-id block)) block)))  

  (test-case
   "bot is not created at invalid location"
   (let* ([somewhere (location -1 2)]
          [new-id (add-entity! (make-world 10) type-bot somewhere)])
     (check-false new-id)))

  (test-case
   "bot is created with new id"
   (let* ([world (make-world 10)]
          [first-id (add-entity! world type-bot (location 3 4))]
          [second-id (add-entity! world type-bot (location 5 6))]          )
     (check-not-equal? first-id second-id)))

  (test-case
   "move bot changes location"
   (let* ([world (make-world 10)]
          [bot (add-entity! world type-bot (location 5 6))]
          [id (entity-id bot)])
     (move-entity! world id direction-north)
     (check-equal? (entity-location (entity-ref world id)) (location 5 7))
     (move-entity! world id direction-east)
     (check-equal? (entity-location (entity-ref world id)) (location 6 7))
     (move-entity! world id direction-south)
     (check-equal? (entity-location (entity-ref world id)) (location 6 6))
     (move-entity! world id direction-west)
     (check-equal? (entity-location (entity-ref world id)) (location 5 6))))

  (test-case
   "invalid move leaves bot location unchanged"
   (let* ([world (make-world 10)]
          [bot (add-entity! world type-bot (location 9 9))])
     (check-false (move-entity! world (entity-id bot) direction-north))
     (check-equal? (entity-location bot) (location 9 9)))) 

  (test-case
   "can not move to occupied location"
   (let* ([world (make-world 3)]
          [bot (add-entity! world type-bot (location 1 1))])
     (add-entity! world type-block (location 1 2))
     (check-false (move-entity! world (entity-id bot) direction-north))
     (check-equal? (entity-location bot) (location 1 1)))) 

  (test-case
   "entities are drawn"
   (let* ([world (make-world 3)]
          [bot (string (entity-symbol (add-entity! world type-bot (location 0 2))))]
          [block (string (entity-symbol (add-entity! world type-block(location 2 1))))])
     (add-entity! world type-bot(location 1 1))
     (define result "")
     (define (draw symbol x y)
       (set! result (string-join (list result (string symbol) (number->string x) (number->string y)))))
     (draw-entities world draw)
     (check-equal? result (string-append " " bot " 0 0 " block " 2 1 " bot " 1 1"))))

  (test-case
   "neighbors are nearby"
   (let* ([world (make-world 3)]
          [subject (add-entity! world type-bot (location 1 1))])
     (add-entity! world type-block (location 1 2))
     (add-entity! world type-block (location 0 0))
     (let ([nearby (neighbors world subject)])
       (check-equal? (length nearby) 1)
       (check-equal? (entity-location (first nearby)) (location 1 2)))))

  (test-case
   "block is loaded"
   (let* ([world (make-world 3)]
          [bot (add-entity! world type-bot (location 1 1))]
          [block (add-entity! world type-block (location 2 1))]
          [new-bot (take-entity! world (entity-id bot) (entity-id block))])
     (check-equal? (entity-cargo new-bot) block)
     (check-false (entity-ref world (entity-id block)))))

  (test-case
   "block is dropped"
   (let* ([world (make-world 3)]
          [block (add-entity! world type-block (location 2 1))]
          [bot (add-entity! world type-bot (location 1 1))])
     (take-entity! world (entity-id bot) (entity-id block))
     (drop-entity! world (entity-id bot) direction-north)
     (check-false (entity-cargo (entity-ref world (entity-id bot))))
     (check-equal? (entity-location (entity-ref world (entity-id block))) (location 1 2)))))
