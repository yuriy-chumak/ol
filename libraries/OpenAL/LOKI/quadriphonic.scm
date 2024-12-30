(define-library (OpenAL LOKI quadriphonic)

(import (scheme core)
   (OpenAL platform))

(export AL_LOKI_quadriphonic

   AL_FORMAT_QUAD8
   AL_FORMAT_QUAD16
)

; ---------------------------------------------------------------------------
(begin
   (define AL_LOKI_quadriphonic (al:QueryExtension "AL_LOKI_quadriphonic"))

   (define AL_FORMAT_QUAD8                      #x10004)
   (define AL_FORMAT_QUAD16                     #x10005)

))
