(define-library (OpenAL SOFT MSADPCM)

(import (scheme core)
   (OpenAL platform))

(export AL_SOFT_MSADPCM

   AL_FORMAT_MONO_MSADPCM
   AL_FORMAT_STEREO_MSADPCM
)

; ---------------------------------------------------------------------------
(begin
   (define AL_SOFT_MSADPCM (al:QueryExtension "AL_SOFT_MSADPCM"))

   (define AL_FORMAT_MONO_MSADPCM              #x1302)
   (define AL_FORMAT_STEREO_MSADPCM            #x1303)
))
