(define-library (OpenAL EXT STEREO_ANGLES)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_STEREO_ANGLES

   AL_STEREO_ANGLES
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_STEREO_ANGLES (al:QueryExtension "AL_EXT_STEREO_ANGLES"))

   (define AL_STEREO_ANGLES                          #x1030)

))
