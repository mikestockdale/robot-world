#lang racket

(require "setup.rkt" "server/dispatcher.rkt")

(define engine (setup-engine))

(define (process-client-request in out dispatcher)
  (let ([request (read in)])
    (unless (equal? request eof)
      (write (dispatch-request dispatcher request) out)
      (flush-output out)
      (process-client-request in out dispatcher))))

(define (run-server)
  (let ([listener (tcp-listen 8080 4 #f "localhost")])

    (define (listen)
      (let-values ([(in out) (tcp-accept listener)])
        (file-stream-buffer-mode in 'none)
        (thread
         (λ ()
           (process-client-request in out (make-dispatcher engine))
           (close-input-port in)
           (close-output-port out)))
        (listen)))
    
    (listen)))

(setup-blocks engine)
(run-server)
