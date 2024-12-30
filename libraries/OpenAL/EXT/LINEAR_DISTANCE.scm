(define-library (OpenAL EXT LINEAR_DISTANCE)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_LINEAR_DISTANCE

   AL_LINEAR_DISTANCE
   AL_LINEAR_DISTANCE_CLAMPED
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_LINEAR_DISTANCE (al:QueryExtension "AL_EXT_LINEAR_DISTANCE"))

   (define AL_LINEAR_DISTANCE                      #xD003)
   (define AL_LINEAR_DISTANCE_CLAMPED              #xD004)

))
