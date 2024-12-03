#lang racket

(provide test-engine)

(require (for-syntax racket))

(define-for-syntax (render-spec spec)
  (define (id-name item)
    (string->symbol (string-append (symbol->string (syntax-e item)) "-id")))
  (define (add-entity type items)
    (list
       (list (first items)
             (list 'add-entity 'engine type
                   (list 'location (second items) (third items))))
       (list (id-name (first items))
             (list 'entity-id (first items)))))
  (let* ([items (syntax->list spec)]
         [keyword (syntax-e (first items))])
    (cond
      [(equal? keyword 'size)
       (list (list 'engine (list 'make-engine (second items))))]
      [(string-contains? (symbol->string keyword) "bot")
       (add-entity 'type-bot items)]
      [(string-contains? (symbol->string keyword) "block")
       (add-entity 'type-block items)]
      [(string-contains? (symbol->string keyword) "base")
       (add-entity 'type-base items)]
      [else '()])))

(define-syntax (test-engine input)
  (let* ([body (rest (syntax->list input))]
         [specs (syntax->list (first body))])
    (datum->syntax
     input
     (append
      (list 'let* (apply append (map render-spec specs)))
      (rest body)))))
