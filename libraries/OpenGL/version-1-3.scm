; OpenGL 1.3 (14 Aug 2001)

(define-library (OpenGL version-1-3)
(export

   GL_VERSION_1_3

   (exports (OpenGL ARB transpose_matrix))

 ; ARB_multitexture
glActiveTexture
;glClientActiveTextureARB( texture )
;   glMultiTexCoord1dARB ;( target , s )
;glMultiTexCoord1dvARB( target , v )
;glMultiTexCoord1fARB( target , s )
;glMultiTexCoord1fvARB( target , v )
;glMultiTexCoord1iARB( target , s )
;glMultiTexCoord1ivARB( target , v )
;glMultiTexCoord1sARB( target , s )
;glMultiTexCoord1svARB( target , v )
;glMultiTexCoord2dARB( target , s , t )
;glMultiTexCoord2dvARB( target , v )
;glMultiTexCoord2fARB( target , s , t )
;glMultiTexCoord2fvARB( target , v )
;glMultiTexCoord2iARB( target , s , t )
;glMultiTexCoord2ivARB( target , v )
;glMultiTexCoord2sARB( target , s , t )
;glMultiTexCoord2svARB( target , v )
;glMultiTexCoord3dARB( target , s , t , r )
;glMultiTexCoord3dvARB( target , v )
;glMultiTexCoord3fARB( target , s , t , r )
;glMultiTexCoord3fvARB( target , v )
;glMultiTexCoord3iARB( target , s , t , r )
;glMultiTexCoord3ivARB( target , v )
;glMultiTexCoord3sARB( target , s , t , r )
;glMultiTexCoord3svARB( target , v )
;glMultiTexCoord4dARB( target , s , t , r , q )
;glMultiTexCoord4dvARB( target , v )
;glMultiTexCoord4fARB( target , s , t , r , q )
;glMultiTexCoord4fvARB( target , v )
;glMultiTexCoord4iARB( target , s , t , r , q )
;glMultiTexCoord4ivARB( target , v )
;glMultiTexCoord4sARB( target , s , t , r , q )
;glMultiTexCoord4svARB( target , v )

   GL_TEXTURE0
   GL_TEXTURE1
   GL_TEXTURE2
   GL_TEXTURE3
   GL_TEXTURE4
   GL_TEXTURE5
   GL_TEXTURE6
   GL_TEXTURE7
   GL_TEXTURE8
   GL_TEXTURE9
   GL_TEXTURE10
   GL_TEXTURE11
   GL_TEXTURE12
   GL_TEXTURE13
   GL_TEXTURE14
   GL_TEXTURE15
   GL_TEXTURE16
   GL_TEXTURE17
   GL_TEXTURE18
   GL_TEXTURE19
   GL_TEXTURE20
   GL_TEXTURE21
   GL_TEXTURE22
   GL_TEXTURE23
   GL_TEXTURE24
   GL_TEXTURE25
   GL_TEXTURE26
   GL_TEXTURE27
   GL_TEXTURE28
   GL_TEXTURE29
   GL_TEXTURE30
   GL_TEXTURE31

;#define GL_ACTIVE_TEXTURE_ARB             0x84E0
;#define GL_CLIENT_ACTIVE_TEXTURE_ARB      0x84E1
;#define GL_MAX_TEXTURE_UNITS_ARB          0x84E2

 ;

   GL_REFLECTION_MAP
   GL_TEXTURE_CUBE_MAP
   GL_TEXTURE_BINDING_CUBE_MAP
   GL_TEXTURE_CUBE_MAP_POSITIVE_X
   GL_TEXTURE_CUBE_MAP_NEGATIVE_X
   GL_TEXTURE_CUBE_MAP_POSITIVE_Y
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
   GL_TEXTURE_CUBE_MAP_POSITIVE_Z
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
   GL_PROXY_TEXTURE_CUBE_MAP
   GL_MAX_CUBE_MAP_TEXTURE_SIZE


   (exports (OpenGL version-1-2)))
; ============================================================================
; == implementation ==========================================================
(import (scheme core)
   (OpenGL version-1-2)
   (OpenGL ARB transpose_matrix))

(begin
   (define GL_VERSION_1_3 1)

   (setq GL gl:GetProcAddress)

   (define glActiveTexture (GL GLvoid "glActiveTexture" GLenum))

   (define GL_TEXTURE0                   #x84C0)
   (define GL_TEXTURE1                   #x84C1)
   (define GL_TEXTURE2                   #x84C2)
   (define GL_TEXTURE3                   #x84C3)
   (define GL_TEXTURE4                   #x84C4)
   (define GL_TEXTURE5                   #x84C5)
   (define GL_TEXTURE6                   #x84C6)
   (define GL_TEXTURE7                   #x84C7)
   (define GL_TEXTURE8                   #x84C8)
   (define GL_TEXTURE9                   #x84C9)
   (define GL_TEXTURE10                  #x84CA)
   (define GL_TEXTURE11                  #x84CB)
   (define GL_TEXTURE12                  #x84CC)
   (define GL_TEXTURE13                  #x84CD)
   (define GL_TEXTURE14                  #x84CE)
   (define GL_TEXTURE15                  #x84CF)
   (define GL_TEXTURE16                  #x84D0)
   (define GL_TEXTURE17                  #x84D1)
   (define GL_TEXTURE18                  #x84D2)
   (define GL_TEXTURE19                  #x84D3)
   (define GL_TEXTURE20                  #x84D4)
   (define GL_TEXTURE21                  #x84D5)
   (define GL_TEXTURE22                  #x84D6)
   (define GL_TEXTURE23                  #x84D7)
   (define GL_TEXTURE24                  #x84D8)
   (define GL_TEXTURE25                  #x84D9)
   (define GL_TEXTURE26                  #x84DA)
   (define GL_TEXTURE27                  #x84DB)
   (define GL_TEXTURE28                  #x84DC)
   (define GL_TEXTURE29                  #x84DD)
   (define GL_TEXTURE30                  #x84DE)
   (define GL_TEXTURE31                  #x84DF)

   (define GL_REFLECTION_MAP #x8512)
   (define GL_TEXTURE_CUBE_MAP #x8513)
   (define GL_TEXTURE_BINDING_CUBE_MAP #x8514)
   (define GL_TEXTURE_CUBE_MAP_POSITIVE_X #x8515)
   (define GL_TEXTURE_CUBE_MAP_NEGATIVE_X #x8516)
   (define GL_TEXTURE_CUBE_MAP_POSITIVE_Y #x8517)
   (define GL_TEXTURE_CUBE_MAP_NEGATIVE_Y #x8518)
   (define GL_TEXTURE_CUBE_MAP_POSITIVE_Z #x8519)
   (define GL_TEXTURE_CUBE_MAP_NEGATIVE_Z #x851A)
   (define GL_PROXY_TEXTURE_CUBE_MAP #x851B)
   (define GL_MAX_CUBE_MAP_TEXTURE_SIZE #x851C)

))