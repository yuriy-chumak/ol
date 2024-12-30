(define-library (OpenAL EXT double)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_double

   AL_FORMAT_MONO_DOUBLE
   AL_FORMAT_STEREO_DOUBLE
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_double (al:QueryExtension "AL_EXT_DOUBLE"))

   (define AL_FORMAT_MONO_DOUBLE               #x10010)
   (define AL_FORMAT_STEREO_DOUBLE             #x10011)

))
