#lang racket/base

(provide test-case:)
(require rackunit)

(define-syntax-rule (test-case: items ...)
  (module+ test (test-case items ...)))
