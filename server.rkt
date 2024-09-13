#lang racket

(provide make-server connect-server connect-remote add-bot! drop-block! move-bot! take-block!)
(require "gen-server.rkt" "local-server.rkt" "remote-server.rkt")
