; ===========================================================================
; EXT_draw_range_elements                            (included in OpenGL 1.2)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_polygon_offset.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT draw_range_elements)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;  None

; ---------------------------------------------------------------------------
(export EXT_draw_range_elements

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glPolygonOffsetEXT

; ---------------------------------------------------------------------------
; New Tokens

   GL_POLYGON_OFFSET_EXT
   GL_POLYGON_OFFSET_FACTOR_EXT
   GL_POLYGON_OFFSET_BIAS_EXT

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_draw_range_elements (gl:QueryExtension "GL_EXT_draw_range_elements"))

   (setq GL gl:GetProcAddress)
   (define glPolygonOffsetEXT (GL GLvoid "glPolygonOffsetEXT" GLfloat GLfloat))

   (define GL_POLYGON_OFFSET_EXT               #x8037)
   (define GL_POLYGON_OFFSET_FACTOR_EXT        #x8038)
   (define GL_POLYGON_OFFSET_BIAS_EXT          #x8039)

))
