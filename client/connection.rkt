#lang racket

(provide connect-remote)

(define (connect-remote host port) (remote-call host port))

(define (remote-call host port)
  (let-values ([(in out) (tcp-connect host port)])
    (file-stream-buffer-mode in 'none)
    (λ (request)
      (write request out)
      (flush-output out)
      (read in))))
