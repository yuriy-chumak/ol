(define-library (OpenAL SOFT deferred_updates)

(import (scheme core)
   (OpenAL platform))

(export AL_SOFT_deferred_updates

   alDeferUpdates
   alProcessUpdates

   AL_DEFERRED_UPDATES
)

; ---------------------------------------------------------------------------
(begin
   (define AL_SOFT_deferred_updates (al:QueryExtension "AL_SOFT_deferred_updates"))

   (setq AL al:GetProcAddress)
   (define alDeferUpdates (AL ALvoid "alDeferUpdatesSOFT"))
   (define alProcessUpdates (AL ALvoid "alProcessUpdatesSOFT"))

   (define AL_DEFERRED_UPDATES                 #xC002)
))
