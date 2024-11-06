#lang racket

(provide make-engine make-bot draw-entities
         add-entity move-entity take-entity drop-entity)

(require threading)
(require "shared.rkt" "cargos.rkt" "grid.rkt" "sequence.rkt")

(struct engine (sequence grid cargos))
(define (make-engine size) (engine (make-sequence) (make-grid size) (make-cargos)))

(define (add-entity engine type location)
  (if (is-available? (engine-grid engine) location)
      (let ([new-entity (entity (new-id (engine-sequence engine)) type location)])
        (place-entity (engine-grid engine) new-entity)
        new-entity)
      #f))

(define (move-entity engine id direction)
  (let*
      ([old-entity (entity-by-id (engine-grid engine) id)]
       [new-location (move-direction direction (entity-location old-entity))])
    (if (is-available? (engine-grid engine) new-location)
        (begin
          (place-entity (engine-grid engine) (change-entity-location old-entity new-location))
          #t)
        #f)))

(define (take-entity engine id cargo-id)
  (let ([cargo (entity-by-id (engine-grid engine) cargo-id)])
    (if cargo
        (begin
          (load-cargo (engine-cargos engine) id cargo)
          (remove-entity (engine-grid engine) cargo-id)
          #t)
        #f)))

(define (drop-entity engine id direction)
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
 
(define (make-bot engine entity-id)
  (let ([entity (entity-by-id (engine-grid engine) entity-id)])
    (bot entity
         (cargo-for-bot (engine-cargos engine) entity-id)
         (neighbors (engine-grid engine) (entity-location entity)))))

(module+ test
  (require rackunit)

  (test-case
   "entity is added at location"
   (let* ([engine (make-engine 10)]
          [block (add-entity engine type-block (location 1 2))])
     (check-equal? (entity-type block) type-block)  
     (check-equal? (entity-location block) (location 1 2))
     (check-equal? (entity-by-id (engine-grid engine) (entity-id block)) block)))  

  (test-case
   "entity is not added at invalid location"
   (check-false (add-entity (make-engine 10) type-bot (location -1 2))))

  (test-case
   "entity is created with new id"
   (let ([engine (make-engine 10)])
     (check-not-equal? (entity-id (add-entity engine type-bot (location 3 4)))
                       (entity-id (add-entity engine type-bot (location 5 6))))))

  (test-case
   "move bot changes location"
   (let* ([engine (make-engine 10)]
          [bot (add-entity engine type-bot (location 5 6))]
          [id (entity-id bot)])
     (move-entity engine id direction-north)
     (check-equal? (entity-location (entity-by-id (engine-grid engine) id)) (location 5 7))))

   (test-case
    "invalid move leaves bot location unchanged"
    (let* ([engine (make-engine 10)]
           [bot (add-entity engine type-bot (location 9 9))])
      (check-false (move-entity engine (entity-id bot) direction-north))
      (check-equal? (entity-location bot) (location 9 9)))) 

   (test-case
    "can not move to occupied location"
    (let* ([engine (make-engine 3)]
           [bot (add-entity engine type-bot (location 1 1))])
      (add-entity engine type-block (location 1 2))
      (check-false (move-entity engine (entity-id bot) direction-north))
      (check-equal? (entity-location bot) (location 1 1)))) 
  
   (test-case
    "block is taken"
    (let* ([engine (make-engine 3)]
           [bot (add-entity engine type-bot (location 1 1))]
           [block (add-entity engine type-block (location 2 1))])
      (check-true (take-entity engine (entity-id bot) (entity-id block)))
      (check-equal? (cargo-for-bot (engine-cargos engine) (entity-id bot)) block)
      (check-false (entity-by-id (engine-grid engine) (entity-id block)))))

   (test-case
    "can not take if block is removed"
    (let* ([engine (make-engine 3)]
           [bot (add-entity engine type-bot (location 1 1))]
           [block (add-entity engine type-block (location 2 1))])
      (remove-entity (engine-grid engine) (entity-id block))
      (check-false (take-entity engine (entity-id bot) (entity-id block)))
      (check-false (cargo-for-bot (engine-cargos engine) (entity-id bot)))))

   (test-case
    "block is dropped"
    (let* ([engine (make-engine 3)]
           [block (add-entity engine type-block (location 2 1))]
           [bot (add-entity engine type-bot (location 1 1))])
      (take-entity engine (entity-id bot) (entity-id block))
      (check-true (drop-entity engine (entity-id bot) direction-north))
      (check-false (cargo-for-bot (engine-cargos engine) (entity-id bot)))
      (check-equal? (entity-location (entity-by-id (engine-grid engine) (entity-id block))) (location 1 2))))

   (test-case
    "can not drop in occupied location"
    (let* ([engine (make-engine 3)]
           [block (add-entity engine type-block (location 2 1))]
           [bot (add-entity engine type-bot (location 1 1))])
      (take-entity engine (entity-id bot) (entity-id block))
      (add-entity engine type-block (location 0 1))
      (check-false (drop-entity engine (entity-id bot) direction-west))
      (check-equal? (cargo-for-bot (engine-cargos engine) (entity-id bot)) block)))

   (test-case
    "entities are drawn"
    (let* ([engine (make-engine 3)]
           [bot (string (entity-symbol (add-entity engine type-bot (location 0 2)) #f))]
           [block (string (entity-symbol (add-entity engine type-block (location 2 1)) #f))])
      (add-entity engine type-bot(location 1 1))
      (define result "")
      (define (draw symbol x y)
        (set! result (string-join (list result (string symbol) (number->string x) (number->string y)))))
      (draw-entities engine draw)
      (check-equal? result (string-append " " bot " 0 0 " block " 2 1 " bot " 1 1"))))

   (test-case
    "make bot"
    (let* ([engine (make-engine 4)]
           [bot1 (add-entity engine type-bot (location 2 2))]
           [cargo (add-entity engine type-block (location 2 3))]
           [block (add-entity engine type-block (location 1 1))])
      (take-entity engine (entity-id bot1) (entity-id cargo))
      (check-equal? (make-bot engine (entity-id bot1))
                    (bot bot1 cargo (list block)))))

   )