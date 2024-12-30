(define-library (OpenAL SOFT loop_points)

(import (scheme core)
   (OpenAL platform))

(export AL_SOFT_loop_points

   AL_LOOP_POINTS
)

; ---------------------------------------------------------------------------
(begin
   (define AL_SOFT_loop_points (al:QueryExtension "AL_SOFT_loop_points"))

   (define AL_LOOP_POINTS                      #x2015)
))
