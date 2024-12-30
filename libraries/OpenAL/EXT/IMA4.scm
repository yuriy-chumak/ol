(define-library (OpenAL EXT IMA4)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_IMA4

   AL_FORMAT_MONO_IMA4
   AL_FORMAT_STEREO_IMA4
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_IMA4 (al:QueryExtension "AL_EXT_IMA4"))

   (define AL_FORMAT_MONO_IMA4                       #x1300)
   (define AL_FORMAT_STEREO_IMA4                     #x1301)

))
