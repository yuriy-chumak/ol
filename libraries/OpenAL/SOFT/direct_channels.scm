(define-library (OpenAL SOFT direct_channels)

(import (scheme core)
   (OpenAL platform))

(export AL_SOFT_direct_channels

   AL_DIRECT_CHANNELS
)

; ---------------------------------------------------------------------------
(begin
   (define AL_SOFT_direct_channels (al:QueryExtension "AL_SOFT_direct_channels"))

   (define AL_DIRECT_CHANNELS                  #x1033)
))
