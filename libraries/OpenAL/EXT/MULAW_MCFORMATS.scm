(define-library (OpenAL EXT MULAW_MCFORMATS)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_MULAW_MCFORMATS

   AL_FORMAT_MONO_MULAW
   AL_FORMAT_STEREO_MULAW
   AL_FORMAT_QUAD_MULAW
   AL_FORMAT_REAR_MULAW
   AL_FORMAT_51CHN_MULAW
   AL_FORMAT_61CHN_MULAW
   AL_FORMAT_71CHN_MULAW
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_MULAW_MCFORMATS (al:QueryExtension "AL_EXT_MULAW_MCFORMATS"))

   (define AL_FORMAT_MONO_MULAW                      #x10014)
   (define AL_FORMAT_STEREO_MULAW                    #x10015)
   (define AL_FORMAT_QUAD_MULAW                      #x10021)
   (define AL_FORMAT_REAR_MULAW                      #x10022)
   (define AL_FORMAT_51CHN_MULAW                     #x10023)
   (define AL_FORMAT_61CHN_MULAW                     #x10024)
   (define AL_FORMAT_71CHN_MULAW                     #x10025)

))
