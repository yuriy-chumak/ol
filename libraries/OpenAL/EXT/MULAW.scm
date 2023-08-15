(define-library (OpenAL EXT MULAW)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_MULAW

   AL_FORMAT_MONO_MULAW_EXT
   AL_FORMAT_STEREO_MULAW_EXT
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_MULAW (al:QueryExtension "AL_EXT_MULAW"))

   (define AL_FORMAT_MONO_MULAW_EXT               #x10014)
   (define AL_FORMAT_STEREO_MULAW_EXT             #x10015)

))
