(define-library (OpenAL EXT OFFSET)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_OFFSET

   AL_SEC_OFFSET
   AL_SAMPLE_OFFSET
   AL_BYTE_OFFSET
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_OFFSET (al:QueryExtension "AL_EXT_OFFSET"))

   (define AL_SEC_OFFSET                           #x1024)
   (define AL_SAMPLE_OFFSET                        #x1025)
   (define AL_BYTE_OFFSET                          #x1026)

))
