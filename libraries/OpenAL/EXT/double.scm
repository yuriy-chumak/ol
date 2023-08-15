(define-library (OpenAL EXT double)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_double

   AL_FORMAT_MONO_DOUBLE_EXT
   AL_FORMAT_STEREO_DOUBLE_EXT
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_double (al:QueryExtension "AL_EXT_DOUBLE"))

   (define AL_FORMAT_MONO_DOUBLE_EXT               #x10010)
   (define AL_FORMAT_STEREO_DOUBLE_EXT             #x10011)

))
