#lang racket

(require "command.rkt" "setup.rkt")

(define engine (setup-engine))

(define (process-request in out)
  (let ([request (read in)])
    (unless (equal? request eof)
      (sleep .1)
      (dispatch-request
       engine request
       (λ (reply)
         (write reply out)
         (flush-output out)))
      (process-request in out))))

(define (run-server)
  (let ([listener (tcp-listen 8080 4 #f "localhost")])

    (define (listen)
      (let-values ([(in out) (tcp-accept listener)])
        (file-stream-buffer-mode in 'none)
        (thread (λ () (process-request in out)))
        (listen)))
    
    (listen)))

(setup-blocks engine)
(run-server)
