(define-library (OpenAL EXT MCFORMATS)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_MCFORMATS

   AL_FORMAT_QUAD8
   AL_FORMAT_QUAD16
   AL_FORMAT_QUAD32
   AL_FORMAT_REAR8
   AL_FORMAT_REAR16
   AL_FORMAT_REAR32
   AL_FORMAT_51CHN8
   AL_FORMAT_51CHN16
   AL_FORMAT_51CHN32
   AL_FORMAT_61CHN8
   AL_FORMAT_61CHN16
   AL_FORMAT_61CHN32
   AL_FORMAT_71CHN8
   AL_FORMAT_71CHN16
   AL_FORMAT_71CHN32
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_MCFORMATS (al:QueryExtension "AL_EXT_MCFORMATS"))

   (define AL_FORMAT_QUAD8                          #x1204)
   (define AL_FORMAT_QUAD16                         #x1205)
   (define AL_FORMAT_QUAD32                         #x1206)
   (define AL_FORMAT_REAR8                          #x1207)
   (define AL_FORMAT_REAR16                         #x1208)
   (define AL_FORMAT_REAR32                         #x1209)
   (define AL_FORMAT_51CHN8                         #x120A)
   (define AL_FORMAT_51CHN16                        #x120B)
   (define AL_FORMAT_51CHN32                        #x120C)
   (define AL_FORMAT_61CHN8                         #x120D)
   (define AL_FORMAT_61CHN16                        #x120E)
   (define AL_FORMAT_61CHN32                        #x120F)
   (define AL_FORMAT_71CHN8                         #x1210)
   (define AL_FORMAT_71CHN16                        #x1211)
   (define AL_FORMAT_71CHN32                        #x1212)

))
