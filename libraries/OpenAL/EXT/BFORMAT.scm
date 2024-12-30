(define-library (OpenAL EXT BFORMAT)

(import (scheme core)
   (OpenAL platform))

(export AL_EXT_BFORMAT

   AL_FORMAT_BFORMAT2D_8
   AL_FORMAT_BFORMAT2D_16
   AL_FORMAT_BFORMAT2D_FLOAT32
   AL_FORMAT_BFORMAT3D_8
   AL_FORMAT_BFORMAT3D_16
   AL_FORMAT_BFORMAT3D_FLOAT32

)

; ---------------------------------------------------------------------------
(begin
   (define AL_EXT_BFORMAT (al:QueryExtension "AL_EXT_BFORMAT"))

   (define AL_FORMAT_BFORMAT2D_8                       #x20021)
   (define AL_FORMAT_BFORMAT2D_16                      #x20022)
   (define AL_FORMAT_BFORMAT2D_FLOAT32                 #x20023)
   (define AL_FORMAT_BFORMAT3D_8                       #x20031)
   (define AL_FORMAT_BFORMAT3D_16                      #x20032)
   (define AL_FORMAT_BFORMAT3D_FLOAT32                 #x20033)

))
