(define-library (OpenAL SOFT source_resampler)

(import (scheme core)
   (OpenAL platform))

(export AL_SOFT_source_resampler

   AL_NUM_RESAMPLERS
   AL_DEFAULT_RESAMPLER
   AL_SOURCE_RESAMPLER
   AL_RESAMPLER_NAME
)

; ---------------------------------------------------------------------------
(begin
   (define AL_SOFT_source_resampler (al:QueryExtension "AL_SOFT_source_resampler"))

   (define AL_NUM_RESAMPLERS                   #x1210)
   (define AL_DEFAULT_RESAMPLER                #x1211)
   (define AL_SOURCE_RESAMPLER                 #x1212)
   (define AL_RESAMPLER_NAME                   #x1213)
))
