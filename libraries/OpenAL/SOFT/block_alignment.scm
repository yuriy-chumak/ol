(define-library (OpenAL SOFT block_alignment)

(import (scheme core)
   (OpenAL platform))

(export AL_SOFT_block_alignment

   AL_UNPACK_BLOCK_ALIGNMENT
   AL_PACK_BLOCK_ALIGNMENT
)

; ---------------------------------------------------------------------------
(begin
   (define AL_SOFT_block_alignment (al:QueryExtension "AL_SOFT_block_alignment"))

   (define AL_UNPACK_BLOCK_ALIGNMENT           #x200C)
   (define AL_PACK_BLOCK_ALIGNMENT             #x200D)

))
