; ===========================================================================
; EXT_texture_object                                 (included in OpenGL 1.1)
;
;    Texture state may be stored in a GL object, for greater efficiency.
;
;    https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_texture_object.txt
;
; Version
;    $Date: 1995/10/03 05:39:56 $ $Revision: 1.27 $
;
; Overview
;    This extension introduces named texture objects.  The only way to name
;    a texture in GL 1.0 is by defining it as a single display list.  Because
;    display lists cannot be edited, these objects are static.  Yet it is
;    important to be able to change the images and parameters of a texture.
(define-library (OpenGL EXT texture_object)

; ---------------------------------------------------------------------------
; Dependencies
;  EXT_texture3D affects the definition of this extension
(import (scheme core)
        (OpenGL platform))

; ---------------------------------------------------------------------------
(export EXT_texture_object

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glGenTexturesEXT
   glDeleteTexturesEXT
   glBindTextureEXT
   glPrioritizeTexturesEXT
   glAreTexturesResidentEXT
   glIsTextureEXT

; ---------------------------------------------------------------------------
; New Tokens

;    Accepted by the <pname> parameters of TexParameteri, TexParameterf,
;    TexParameteriv, TexParameterfv, GetTexParameteriv, and GetTexParameterfv:
   GL_TEXTURE_PRIORITY_EXT

;    Accepted by the <pname> parameters of GetTexParameteriv and
;    GetTexParameterfv:
   GL_TEXTURE_RESIDENT_EXT

;    Accepted by the <pname> parameters of GetBooleanv, GetIntegerv,
;    GetFloatv, and GetDoublev:
   GL_TEXTURE_1D_BINDING_EXT
   GL_TEXTURE_2D_BINDING_EXT
   GL_TEXTURE_3D_BINDING_EXT

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_texture_object (gl:QueryExtension "GL_EXT_texture_object"))

   (setq GL GL_LIBRARY)
   (define glGenTexturesEXT (GL GLvoid "glGenTexturesEXT" GLsizei GLuint*))
   (define glDeleteTexturesEXT (GL GLvoid "glDeleteTexturesEXT" GLsizei GLuint*))
   (define glBindTextureEXT (GL GLvoid "glBindTextureEXT" GLenum GLuint))
   (define glPrioritizeTexturesEXT (GL GLvoid "glPrioritizeTexturesEXT" GLsizei GLuint* (fft* GLclampf)))
   (define glAreTexturesResidentEXT (GL GLboolean "glAreTexturesResidentEXT" GLsizei GLuint* GLboolean*))
   (define glIsTextureEXT (GL GLboolean "glIsTextureEXT" GLuint))

   (define GL_TEXTURE_PRIORITY_EXT            #x8066)
   (define GL_TEXTURE_RESIDENT_EXT            #x8067)
   (define GL_TEXTURE_1D_BINDING_EXT          #x8068)
   (define GL_TEXTURE_2D_BINDING_EXT          #x8069)
   (define GL_TEXTURE_3D_BINDING_EXT          #x806A)

))
