#lang racket

(require web-server/servlet web-server/servlet-env)
(require "command.rkt" "setup.rkt")

(define world (setup-world))

(define (process-request in out)
  (let ([request (read in)])
    (unless (equal? request eof)
      (write (dispatch-request world request) out)
      (flush-output out)
      (process-request in out))))

(define (run-server)
  (let ([listener (tcp-listen 8080 4 #f "localhost")])

    (define (listen)
      (let-values ([(in out) (tcp-accept listener)])
        (file-stream-buffer-mode in 'none)
        (thread (λ () (process-request in out)))
        (listen)))
    
    (listen)))

(setup-blocks world)
(run-server)