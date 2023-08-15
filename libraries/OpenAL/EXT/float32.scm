(define-library (OpenAL EXT float32)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_float32

   AL_FORMAT_MONO_FLOAT32
   AL_FORMAT_STEREO_FLOAT32
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_float32 (al:QueryExtension "AL_EXT_FLOAT32"))

   (define AL_FORMAT_MONO_FLOAT32               #x10010)
   (define AL_FORMAT_STEREO_FLOAT32             #x10011)

))
