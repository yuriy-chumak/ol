(define-library (OpenAL SOFT source_length)

(import (scheme core)
   (OpenAL platform))

(export AL_SOFT_source_length

   AL_BYTE_LENGTH
   AL_SAMPLE_LENGTH
   AL_SEC_LENGTH
)

; ---------------------------------------------------------------------------
(begin
   (define AL_SOFT_source_length (al:QueryExtension "AL_SOFT_source_length"))

   (define AL_BYTE_LENGTH                      #x2009)
   (define AL_SAMPLE_LENGTH                    #x200A)
   (define AL_SEC_LENGTH                       #x200B)
))
