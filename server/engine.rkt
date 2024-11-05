#lang racket

(provide make-engine make-bot draw-entities
         add-entity! move-entity! take-entity! drop-entity!)

(require threading)
(require "shared.rkt" "cargos.rkt" "grid.rkt" "sequence.rkt")

(struct engine (sequence grid cargos))
(define (make-engine size) (engine (make-sequence) (make-grid size) (make-cargos)))
 
(define (make-bot engine entity-id)
  (let ([entity (entity-by-id (engine-grid engine) entity-id)])
    (bot entity
         (cargo-for-bot (engine-cargos engine) entity-id)
         (neighbors (engine-grid engine) (entity-location entity)))))

(define (add-entity! engine type location)
  (if (is-available? (engine-grid engine) location)
      (let ([new-entity (entity (new-id (engine-sequence engine)) type location)])
        (place-entity (engine-grid engine) new-entity)
        new-entity)
      #f))

(define (move-entity! engine id direction)
  (let*
      ([old-entity (entity-by-id (engine-grid engine) id)]
       [new-location (move-direction direction (entity-location old-entity))])
    (if (is-available? (engine-grid engine) new-location)
        (begin
          (place-entity (engine-grid engine) (change-entity-location old-entity new-location))
          #t)
        #f)))

(define (take-entity! engine id cargo-id)
  (let ([cargo (entity-by-id (engine-grid engine) cargo-id)])
    (if cargo
        (begin
          (load-cargo (engine-cargos engine) id cargo)
          (remove-entity (engine-grid engine) cargo-id)
          #t)
        #f)))

(define (drop-entity! engine id direction)
  (let* ([bot (entity-by-id (engine-grid engine) id)]
         [drop-location (move-direction direction (entity-location bot))])
    (if (is-available? (engine-grid engine) drop-location)
        (begin
          (place-entity (engine-grid engine)
                        (change-entity-location
                         (unload-cargo (engine-cargos engine) id) drop-location))
          #t)
        #f)))

(define (draw-entities engine draw)
  (define (draw-entity entity x y)
    (let ([cargo (cargo-for-bot (engine-cargos engine) (entity-id entity))])
      (draw (entity-symbol entity cargo) x y)))
  (draw-each-entity (engine-grid engine) draw-entity))

(module+ test
  (require rackunit)
  (test-case
   "bot is created at requested location"
   (let* ([engine (make-engine 10)]
          [somewhere (location 1 2)]
          [new-bot (add-entity! engine type-bot somewhere)])
     (check-equal? (entity-location new-bot) somewhere)))

  (test-case
   "block is created at requested location"
   (let* ([engine (make-engine 10)]
          [somewhere (location 1 2)]
          [block (add-entity! engine type-block somewhere)])
     (check-equal? (entity-type block) type-block)  
     (check-equal? (entity-location block) somewhere)
     (check-equal? (entity-by-id (engine-grid engine) (entity-id block)) block)))  

  (test-case
   "bot is not created at invalid location"
   (let* ([somewhere (location -1 2)]
          [new-id (add-entity! (make-engine 10) type-bot somewhere)])
     (check-false new-id)))

  (test-case
   "bot is created with new id"
   (let* ([engine (make-engine 10)]
          [first-id (add-entity! engine type-bot (location 3 4))]
          [second-id (add-entity! engine type-bot (location 5 6))]          )
     (check-not-equal? first-id second-id)))

  (test-case
   "move bot changes location"
   (let* ([engine (make-engine 10)]
          [bot (add-entity! engine type-bot (location 5 6))]
          [id (entity-id bot)])
     (move-entity! engine id direction-north)
     (check-equal? (entity-location (entity-by-id (engine-grid engine) id)) (location 5 7))
     (move-entity! engine id direction-east)
     (check-equal? (entity-location (entity-by-id (engine-grid engine) id)) (location 6 7))
     (move-entity! engine id direction-south)
     (check-equal? (entity-location (entity-by-id (engine-grid engine) id)) (location 6 6))
     (move-entity! engine id direction-west)
     (check-equal? (entity-location (entity-by-id (engine-grid engine) id)) (location 5 6))))

  (test-case
   "invalid move leaves bot location unchanged"
   (let* ([engine (make-engine 10)]
          [bot (add-entity! engine type-bot (location 9 9))])
     (check-false (move-entity! engine (entity-id bot) direction-north))
     (check-equal? (entity-location bot) (location 9 9)))) 

  (test-case
   "can not move to occupied location"
   (let* ([engine (make-engine 3)]
          [bot (add-entity! engine type-bot (location 1 1))])
     (add-entity! engine type-block (location 1 2))
     (check-false (move-entity! engine (entity-id bot) direction-north))
     (check-equal? (entity-location bot) (location 1 1)))) 

  (test-case
   "entities are drawn"
   (let* ([engine (make-engine 3)]
          [bot (string (entity-symbol (add-entity! engine type-bot (location 0 2)) #f))]
          [block (string (entity-symbol (add-entity! engine type-block (location 2 1)) #f))])
     (add-entity! engine type-bot(location 1 1))
     (define result "")
     (define (draw symbol x y)
       (set! result (string-join (list result (string symbol) (number->string x) (number->string y)))))
     (draw-entities engine draw)
     (check-equal? result (string-append " " bot " 0 0 " block " 2 1 " bot " 1 1"))))
  
  (test-case
   "block is taken"
   (let* ([engine (make-engine 3)]
          [bot (add-entity! engine type-bot (location 1 1))]
          [block (add-entity! engine type-block (location 2 1))])
     (check-true (take-entity! engine (entity-id bot) (entity-id block)))
     (check-equal? (cargo-for-bot (engine-cargos engine) (entity-id bot)) block)
     (check-false (entity-by-id (engine-grid engine) (entity-id block)))))

  (test-case
   "can not take if block is removed"
   (let* ([engine (make-engine 3)]
          [bot (add-entity! engine type-bot (location 1 1))]
          [block (add-entity! engine type-block (location 2 1))])
     (remove-entity (engine-grid engine) (entity-id block))
     (check-false (take-entity! engine (entity-id bot) (entity-id block)))
     (check-false (cargo-for-bot (engine-cargos engine) (entity-id bot)))))

  (test-case
   "block is dropped"
   (let* ([engine (make-engine 3)]
          [block (add-entity! engine type-block (location 2 1))]
          [bot (add-entity! engine type-bot (location 1 1))])
     (take-entity! engine (entity-id bot) (entity-id block))
     (check-true (drop-entity! engine (entity-id bot) direction-north))
     (check-false (cargo-for-bot (engine-cargos engine) (entity-id bot)))
     (check-equal? (entity-location (entity-by-id (engine-grid engine) (entity-id block))) (location 1 2))))

  (test-case
   "can not drop in occupied location"
   (let* ([engine (make-engine 3)]
          [block (add-entity! engine type-block (location 2 1))]
          [bot (add-entity! engine type-bot (location 1 1))])
     (take-entity! engine (entity-id bot) (entity-id block))
     (add-entity! engine type-block (location 0 1))
     (check-false (drop-entity! engine (entity-id bot) direction-west))
     (check-equal? (cargo-for-bot (engine-cargos engine) (entity-id bot)) block)))

  )