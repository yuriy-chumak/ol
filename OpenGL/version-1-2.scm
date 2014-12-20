; OpenGL 1.2 (1998)
; 

;EXT_texture3D	Three-dimensional texturing.
;EXT_bgra	Pixel data may be specified in BGR or BGRA order, to match the pixel format of Windows bitmaps.
;EXT_packed_pixels	Pixel data may be "packed" into a larger primitive type. For example, all four components of an RGBA pixel may be specified as a single 32-bit integer.
;EXT_rescale_normal	Normals may be automatically rescaled by the GL, which in some cases removes the need for an expensive normalization.
;EXT_separate_specular_color	The GL's lighting capabilities are extended to support texture-independent specular highlighting.
;SGIS_texture_edge_clamp	A new texture-coordinate clamping mode which, unlike GL_CLAMP, will never sample from the texture's border.
;SGIS_texture_lod	The programmer is given greater control over the level-of-detail calculation used to select a texture's mipmap.
;EXT_draw_range_elements	The DrawRangeElements function; a slightly more performant alternative to DrawElements.

(define-library (OpenGL version-1-2)
   (export
      (exports (OpenGL version-1-1))
    GL_VERSION_1_2

    ; todo: move to the right place
    GL_BGR GL_BGRA

;    (if (defined? GL_VERSION_1_2_DEPRECATED)
;        glColorTable
;
;    )
   )
  
   (import
      (owl defmac) (owl io)
      (owl pinvoke)
      (OpenGL version-1-1))
   (begin

(define    GL_VERSION_1_2    1)
(define % (dlopen "opengl32" 0))

  (define GL_BGR #x80E0)
  (define GL_BGRA #x80E1)

))