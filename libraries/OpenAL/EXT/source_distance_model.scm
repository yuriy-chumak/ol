(define-library (OpenAL EXT source_distance_model)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_source_distance_model

   AL_SOURCE_DISTANCE_MODEL
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_source_distance_model (al:QueryExtension "AL_EXT_source_distance_model"))

   (define AL_SOURCE_DISTANCE_MODEL                 #x200)

))
