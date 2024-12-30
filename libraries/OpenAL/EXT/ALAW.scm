(define-library (OpenAL EXT ALAW)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_ALAW

   AL_FORMAT_MONO_ALAW
   AL_FORMAT_STEREO_ALAW
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_ALAW (al:QueryExtension "AL_EXT_ALAW"))

   (define AL_FORMAT_MONO_ALAW                   #x10016)
   (define AL_FORMAT_STEREO_ALAW                 #x10017)

))
