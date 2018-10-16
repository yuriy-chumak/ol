; OpenGL 3.2 (2009)

; this version introduced "deprecation" model,
; so i think we should copy all defines here
; and remove old constants

; if you want to not disable "deprecated" functionality, please
; add corresponding GL_VERSION_x_x_DEPRECATED variable to config
(define-library (OpenGL version-3-2)
(export

   GL_VERSION_3_2

   GL_VERSION_1_2_DEPRECATED
   GL_RESCALE_NORMAL
   GL_LIGHT_MODEL_COLOR_CONTROL
   GL_SINGLE_COLOR
   GL_SEPARATE_SPECULAR_COLOR
   GL_ALIASED_POINT_SIZE_RANGE

   GL_VERSION_1_3_DEPRECATED
   ;...

   GL_VERSION_1_4_DEPRECATED
   ;...

   GL_VERSION_1_5_DEPRECATED
   ;...

   GL_VERSION_2_0_DEPRECATED
   GL_VERTEX_PROGRAM_TWO_SIDE
   GL_POINT_SPRITE
   GL_COORD_REPLACE
   GL_MAX_TEXTURE_COORDS

   GL_VERSION_2_1_DEPRECATED
   ;...

   GL_VERSION_3_0_DEPRECATED
   ;...

   (exports (OpenGL version-3-1)))

(import (scheme core)
   (OpenGL config)
   (OpenGL version-3-1))

(begin
   (define GL_VERSION_3_2 1)

   (define GL_VERSION_1_2_DEPRECATED (getf config 'GL_VERSION_1_2_DEPRECATED))

   (define GL_RESCALE_NORMAL            (if GL_VERSION_1_2_DEPRECATED GL_RESCALE_NORMAL))
   (define GL_LIGHT_MODEL_COLOR_CONTROL (if GL_VERSION_1_2_DEPRECATED GL_LIGHT_MODEL_COLOR_CONTROL))
   (define GL_SINGLE_COLOR              (if GL_VERSION_1_2_DEPRECATED GL_SINGLE_COLOR))
   (define GL_SEPARATE_SPECULAR_COLOR   (if GL_VERSION_1_2_DEPRECATED GL_SEPARATE_SPECULAR_COLOR))
   (define GL_ALIASED_POINT_SIZE_RANGE  (if GL_VERSION_1_2_DEPRECATED GL_ALIASED_POINT_SIZE_RANGE))


   (define GL_VERSION_1_3_DEPRECATED (getf config 'GL_VERSION_1_3_DEPRECATED))
   
   (define GL_VERSION_1_4_DEPRECATED (getf config 'GL_VERSION_1_4_DEPRECATED))
   
   (define GL_VERSION_1_5_DEPRECATED (getf config 'GL_VERSION_1_5_DEPRECATED))


   (define GL_VERSION_2_0_DEPRECATED (getf config 'GL_VERSION_2_0_DEPRECATED))
   (define GL_VERTEX_PROGRAM_TWO_SIDE   (if GL_VERSION_2_0_DEPRECATED #x8643#|GL_VERTEX_PROGRAM_TWO_SIDE|#))
   (define GL_POINT_SPRITE              (if GL_VERSION_2_0_DEPRECATED #x8861#|GL_POINT_SPRITE|#))
   (define GL_COORD_REPLACE             (if GL_VERSION_2_0_DEPRECATED #x8862#|GL_COORD_REPLACE|#))
   (define GL_MAX_TEXTURE_COORDS        (if GL_VERSION_2_0_DEPRECATED #x8871#|GL_MAX_TEXTURE_COORDS|#))

   
   (define GL_VERSION_2_1_DEPRECATED (getf config 'GL_VERSION_2_1_DEPRECATED))
   
   (define GL_VERSION_3_0_DEPRECATED (getf config 'GL_VERSION_3_0_DEPRECATED))

   ; ...
))