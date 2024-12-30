(define-library (OpenAL EXT MULAW)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_MULAW

   AL_FORMAT_MONO_MULAW
   AL_FORMAT_STEREO_MULAW
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_MULAW (al:QueryExtension "AL_EXT_MULAW"))

   (define AL_FORMAT_MONO_MULAW               #x10014)
   (define AL_FORMAT_STEREO_MULAW             #x10015)

))
