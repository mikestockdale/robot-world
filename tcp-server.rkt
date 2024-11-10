#lang racket

(require "command.rkt" "setup.rkt" "server/agent.rkt")

(define engine (setup-engine))

(define (process-requests in out)
  (let ([agent (agent 0)])
    (define (process-request)
      (let ([request (read in)])
        (unless (equal? request eof)
          (sleep (delay! agent))
          (write (dispatch-request engine request) out)
          (flush-output out)
          (process-request))))
    (process-request))
  (close-input-port in)
  (close-output-port out))

(define (run-server)
  (let ([listener (tcp-listen 8080 4 #f "localhost")])

    (define (listen)
      (let-values ([(in out) (tcp-accept listener)])
        (file-stream-buffer-mode in 'none)
        (thread (λ () (process-requests in out)))
        (listen)))
    
    (listen)))

(setup-blocks engine)
(run-server)
