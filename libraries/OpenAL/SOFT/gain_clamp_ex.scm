(define-library (OpenAL SOFT gain_clamp_ex)

(import (scheme core)
   (OpenAL platform))

(export AL_SOFT_gain_clamp_ex

   AL_GAIN_LIMIT
)

; ---------------------------------------------------------------------------
(begin
   (define AL_SOFT_gain_clamp_ex (al:QueryExtension "AL_SOFT_gain_clamp_ex"))

   (define AL_GAIN_LIMIT                       #x200E)
))
