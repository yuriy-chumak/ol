(define-library (OpenAL EXT ALAW)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_ALAW

   AL_FORMAT_MONO_ALAW_EXT
   AL_FORMAT_STEREO_ALAW_EXT
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_ALAW (al:QueryExtension "AL_EXT_ALAW"))

   (define AL_FORMAT_MONO_ALAW_EXT               #x10016)
   (define AL_FORMAT_STEREO_ALAW_EXT             #x10017)

))
