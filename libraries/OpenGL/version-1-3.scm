; OpenGL 1.3 (2001)

(define-library (OpenGL version-1-3)
(export

   GL_VERSION_1_3

 ; ARB_multitexture
;glActiveTextureARB( texture )
;glClientActiveTextureARB( texture )
;glMultiTexCoord1dARB( target , s )
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

;#define GL_TEXTURE0_ARB                   0x84C0
;#define GL_TEXTURE1_ARB                   0x84C1
;#define GL_TEXTURE2_ARB                   0x84C2
;#define GL_TEXTURE3_ARB                   0x84C3
;#define GL_TEXTURE4_ARB                   0x84C4
;#define GL_TEXTURE5_ARB                   0x84C5
;#define GL_TEXTURE6_ARB                   0x84C6
;#define GL_TEXTURE7_ARB                   0x84C7
;#define GL_TEXTURE8_ARB                   0x84C8
;#define GL_TEXTURE9_ARB                   0x84C9
;#define GL_TEXTURE10_ARB                  0x84CA
;#define GL_TEXTURE11_ARB                  0x84CB
;#define GL_TEXTURE12_ARB                  0x84CC
;#define GL_TEXTURE13_ARB                  0x84CD
;#define GL_TEXTURE14_ARB                  0x84CE
;#define GL_TEXTURE15_ARB                  0x84CF
;#define GL_TEXTURE16_ARB                  0x84D0
;#define GL_TEXTURE17_ARB                  0x84D1
;#define GL_TEXTURE18_ARB                  0x84D2
;#define GL_TEXTURE19_ARB                  0x84D3
;#define GL_TEXTURE20_ARB                  0x84D4
;#define GL_TEXTURE21_ARB                  0x84D5
;#define GL_TEXTURE22_ARB                  0x84D6
;#define GL_TEXTURE23_ARB                  0x84D7
;#define GL_TEXTURE24_ARB                  0x84D8
;#define GL_TEXTURE25_ARB                  0x84D9
;#define GL_TEXTURE26_ARB                  0x84DA
;#define GL_TEXTURE27_ARB                  0x84DB
;#define GL_TEXTURE28_ARB                  0x84DC
;#define GL_TEXTURE29_ARB                  0x84DD
;#define GL_TEXTURE30_ARB                  0x84DE
;#define GL_TEXTURE31_ARB                  0x84DF
;#define GL_ACTIVE_TEXTURE_ARB             0x84E0
;#define GL_CLIENT_ACTIVE_TEXTURE_ARB      0x84E1
;#define GL_MAX_TEXTURE_UNITS_ARB          0x84E2

 ;

   (exports (OpenGL version-1-2)))
; ============================================================================
; == implementation ==========================================================
(import (scheme core)
   (OpenGL version-1-2))

(begin
   (define GL_VERSION_1_3 1)

   (define GL GL_LIBRARY)

   ;...

))