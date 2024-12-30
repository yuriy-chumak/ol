(define-library (OpenAL EXT EXPONENT_DISTANCE)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_EXPONENT_DISTANCE

   AL_EXPONENT_DISTANCE
   AL_EXPONENT_DISTANCE_CLAMPED
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_EXPONENT_DISTANCE (al:QueryExtension "AL_EXT_EXPONENT_DISTANCE"))

   (define AL_EXPONENT_DISTANCE               #xD005)
   (define AL_EXPONENT_DISTANCE_CLAMPED       #xD006)

))
