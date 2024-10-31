#lang racket

(require "shared/bot.rkt"
         "shared/direction.rkt"
         "shared/entity.rkt"
         "shared/location.rkt")

(provide (struct-out bot) bot-id bot-location
         all-directions move-direction change-direction
         direction-north direction-east direction-south direction-west
         (struct-out entity) make-edge entity-symbol change-entity-location
         type-block type-bot type-edge
         (struct-out location) adjacent? nearby? location-offset)
