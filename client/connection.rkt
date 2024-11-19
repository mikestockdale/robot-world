#lang racket

(provide connect-remote)

;@title{Connection}
;@margin-note{Source code at @hyperlink["https://github.com/mikestockdale/robot-world/blob/main/client/connection.rkt" "connection.rkt"]}
;A connection is a function that sends a request to the server and receives a reply.

(define (connect-remote host port)
  (let-values ([(in out) (tcp-connect host port)])
    (file-stream-buffer-mode in 'none)
    (λ (request)
      (write request out)
      (flush-output out)
      (read in))))
