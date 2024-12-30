(define-library (OpenAL EXT SOURCE_RADIUS)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_SOURCE_RADIUS

   AL_SOURCE_RADIUS
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_SOURCE_RADIUS (al:QueryExtension "AL_EXT_SOURCE_RADIUS"))

   (define AL_SOURCE_RADIUS                          #x1031)

))
