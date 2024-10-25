#lang racket

(require web-server/servlet web-server/servlet-env)
(require "command.rkt" "setup.rkt")

(define world (setup-world))

(define (process-request in out)
  (let ([request (read in)])
    (unless (equal? request eof)
      (write (dispatch-request world request) out)
      (process-request in out))))

(define (run-server)
  (let ([listener (tcp-listen 8080 4 #f "127.0.0.1")])

    (define (listen)
      (let-values ([(in out) (tcp-accept listener)])
        (file-stream-buffer-mode in 'none)
        (file-stream-buffer-mode out 'none)
        (thread (λ () (process-request in out)))
        (listen)))
    
    (listen)))

(setup-blocks world)
(run-server)
