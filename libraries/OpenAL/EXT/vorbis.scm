(define-library (OpenAL EXT vorbis)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_vorbis

   AL_FORMAT_VORBIS_EXT
)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_vorbis (al:QueryExtension "AL_EXT_vorbis"))

   (define AL_FORMAT_VORBIS_EXT               #x10003)

))
