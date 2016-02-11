; EXT_texture_object
;	Texture state may be stored in a GL object, for greater efficiency.
;
;	https://www.opengl.org/registry/specs/EXT/texture_object.txt
;
; Version
;	$Date: 1995/10/03 05:39:56 $ $Revision: 1.27 $
;
; Overview
;	This extension introduces named texture objects.  The only way to name
;	a texture in GL 1.0 is by defining it as a single display list.  Because
;	display lists cannot be edited, these objects are static.  Yet it is
;	important to be able to change the images and parameters of a texture.
(define-library (OpenGL EXT texture_object)

; ---------------------------------------------------------------------------
; Dependencies
;	EXT_texture3D affects the definition of this extension
   (import
      (OpenGL version-1-0))

; ---------------------------------------------------------------------------
   (export
    EXT_texture_object

; ---------------------------------------------------------------------------
; New Procedures and Functions
      glGenTextures ; void (GLsizei n, GLuint *textures)
      ;DeleteTextures
      glBindTexture ; void (GLenum target, GLuint texture)
      ;PrioritizeTextures
      ;AreTexturesResident
      ;IsTexture

; ---------------------------------------------------------------------------
; New Tokens
    
)
  
; ---------------------------------------------------------------------------
   (begin
;   (gl:make-current)
   (define EXT_texture_object (glIsExtensionSupported "GL_EXT_texture_object"))
   (define $ (dlopen GL_LIBRARY))

   (define glBindTexture (if EXT_texture_object
         (dlsym $ GLvoid "glBindTexture" GLenum GLuint)))
   ;WINGDIAPI void APIENTRY glDeleteTextures (GLsizei n, const GLuint *textures);
   (define glGenTextures (if EXT_texture_object
         (dlsym $ GLvoid "glGenTextures" GLsizei GLuint*)))
   ;WINGDIAPI GLboolean APIENTRY glIsTexture (GLuint texture);
   
;   (gl:stop-current)
))
