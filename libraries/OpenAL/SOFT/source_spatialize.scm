(define-library (OpenAL SOFT source_spatialize)

(import (scheme core)
   (OpenAL platform))

(export AL_SOFT_source_spatialize

   AL_SOURCE_SPATIALIZE
   AL_AUTO
)

; ---------------------------------------------------------------------------
(begin
   (define AL_SOFT_source_spatialize (al:QueryExtension "AL_SOFT_source_spatialize"))

   (define AL_SOURCE_SPATIALIZE                #x1214)
   (define AL_AUTO                             #x0002)
))
