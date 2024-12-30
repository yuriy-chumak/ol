(define-library (OpenAL EXT MULAW_BFORMAT)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_MULAW_BFORMAT

   AL_FORMAT_BFORMAT2D_MULAW
   AL_FORMAT_BFORMAT3D_MULAW
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_MULAW_BFORMAT (al:QueryExtension "AL_EXT_MULAW_BFORMAT"))

   (define AL_FORMAT_BFORMAT2D_MULAW                 #x10031)
   (define AL_FORMAT_BFORMAT3D_MULAW                 #x10032)

))
