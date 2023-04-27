; ==========================================================================
; OES_framebuffer_object
;
;     https://registry.khronos.org/OpenGL/extensions/OES/OES_framebuffer_object.txt
;
; Version
;     Last Modified Date: April 10, 2008
;
; Status:
;     Ratified by the Khronos BOP, July 22, 2005.
;

(define-library (OpenGL OES framebuffer_object)

; --------------------------------------------------------------------------
; Dependencies
;     OpenGL ES 1.0 is required.
   (import
      (scheme core)
      (OpenGL platform))

; --------------------------------------------------------------------------
   (export  OES_framebuffer_object

; --------------------------------------------------------------------------
; New Procedures and Functions
   glIsRenderbuffer
   glBindRenderbuffer
   glDeleteRenderbuffers
   glGenRenderbuffers
   glRenderbufferStorage
   glGetRenderbufferParameteriv
   glIsFramebuffer
   glBindFramebuffer
   glDeleteFramebuffers
   glGenFramebuffers
   glCheckFramebufferStatus
   glFramebufferTexture2D
   glFramebufferRenderbuffer
   glGetFramebufferAttachmentParameteriv
   glGenerateMipmap

; --------------------------------------------------------------------------
; New Tokens

   GL_FRAMEBUFFER

   GL_RENDERBUFFER

   GL_DEPTH_COMPONENT16
   GL_RGBA4
   GL_RGB5_A1
   GL_RGB565
   GL_STENCIL_INDEX1
   GL_STENCIL_INDEX4
   GL_STENCIL_INDEX8

   GL_RENDERBUFFER_WIDTH
   GL_RENDERBUFFER_HEIGHT
   GL_RENDERBUFFER_INTERNAL_FORMAT
   GL_RENDERBUFFER_RED_SIZE
   GL_RENDERBUFFER_GREEN_SIZE
   GL_RENDERBUFFER_BLUE_SIZE
   GL_RENDERBUFFER_ALPHA_SIZE
   GL_RENDERBUFFER_DEPTH_SIZE
   GL_RENDERBUFFER_STENCIL_SIZE

   GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
   GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET

   GL_COLOR_ATTACHMENT0
   GL_DEPTH_ATTACHMENT
   GL_STENCIL_ATTACHMENT

   GL_NONE

   GL_FRAMEBUFFER_COMPLETE
   GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
   GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
   GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS
   GL_FRAMEBUFFER_INCOMPLETE_FORMATS
   GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER
   GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER
   GL_FRAMEBUFFER_UNSUPPORTED

   GL_FRAMEBUFFER_BINDING
   GL_RENDERBUFFER_BINDING
   GL_MAX_RENDERBUFFER_SIZE

   GL_INVALID_FRAMEBUFFER_OPERATION
)

; --------------------------------------------------------------------------
(begin
   (setq GL gl:GetProcAddress)
   (define OES_framebuffer_object (gl:QueryExtension "GL_OES_framebuffer_object"))

   (define glIsRenderbuffer (GL GLboolean "glIsRenderbuffer" GLuint))
   (define glBindRenderbuffer (GL GLvoid "glBindRenderbuffer" GLenum GLuint))
   (define glDeleteRenderbuffers (GL GLvoid "glDeleteRenderbuffers" GLsizei GLuint*))
   (define glGenRenderbuffers (GL GLvoid "glGenRenderbuffers" GLsizei GLuint&))
   (define glRenderbufferStorage (GL GLvoid "glRenderbufferStorage" GLenum GLenum GLsizei GLsizei))
   (define glGetRenderbufferParameteriv (GL GLvoid "glGetRenderbufferParameteriv" GLenum GLenum GLint*))
   (define glIsFramebuffer (GL GLboolean "glIsFramebuffer" GLuint))
   (define glBindFramebuffer (GL GLvoid "glBindFramebuffer" GLenum GLuint))
   (define glDeleteFramebuffers (GL GLvoid "glDeleteFramebuffers" GLsizei GLuint*))
   (define glGenFramebuffers (GL GLvoid "glGenFramebuffers" GLsizei GLuint&))
   (define glCheckFramebufferStatus (GL GLenum "glCheckFramebufferStatus" GLenum))
   (define glFramebufferTexture2D (GL GLvoid "glFramebufferTexture2D" GLenum GLenum GLenum GLuint GLint))
   (define glFramebufferRenderbuffer (GL GLvoid "glFramebufferRenderbuffer" GLenum GLenum GLenum GLuint))
   (define glGetFramebufferAttachmentParameteriv (GL GLvoid "glGetFramebufferAttachmentParameteriv" GLenum GLenum GLenum GLint*))
   (define glGenerateMipmap (GL GLvoid "glGenerateMipmap" GLenum))

   (define GL_FRAMEBUFFER                     #x8D40)
   (define GL_RENDERBUFFER                    #x8D41)
   (define GL_DEPTH_COMPONENT16               #x81A5)
   (define GL_RGBA4                           #x8056)
   (define GL_RGB5_A1                         #x8057)
   (define GL_RGB565                          #x8D62)
   (define GL_STENCIL_INDEX1                  #x8D46)
   (define GL_STENCIL_INDEX4                  #x8D47)
   (define GL_STENCIL_INDEX8                  #x8D48)
   (define GL_RENDERBUFFER_WIDTH              #x8D42)
   (define GL_RENDERBUFFER_HEIGHT             #x8D43)
   (define GL_RENDERBUFFER_INTERNAL_FORMAT    #x8D44)
   (define GL_RENDERBUFFER_RED_SIZE           #x8D50)
   (define GL_RENDERBUFFER_GREEN_SIZE         #x8D51)
   (define GL_RENDERBUFFER_BLUE_SIZE          #x8D52)
   (define GL_RENDERBUFFER_ALPHA_SIZE         #x8D53)
   (define GL_RENDERBUFFER_DEPTH_SIZE         #x8D54)
   (define GL_RENDERBUFFER_STENCIL_SIZE       #x8D55)
   (define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE #x8CD0)
   (define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME #x8CD1)
   (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL #x8CD2)
   (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE #x8CD3)
   (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET #x8CD4)
   (define GL_COLOR_ATTACHMENT0               #x8CE0)
   (define GL_DEPTH_ATTACHMENT                #x8D00)
   (define GL_STENCIL_ATTACHMENT              #x8D20)
   (define GL_NONE 0)
   (define GL_FRAMEBUFFER_COMPLETE            #x8CD5)
   (define GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT #x8CD6)
   (define GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT #x8CD7)
   (define GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS #x8CD9)
   (define GL_FRAMEBUFFER_INCOMPLETE_FORMATS  #x8CDA)
   (define GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER #x8CDB)
   (define GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER #x8CDC)
   (define GL_FRAMEBUFFER_UNSUPPORTED         #x8CDD)
   (define GL_FRAMEBUFFER_BINDING             #x8CA6)
   (define GL_RENDERBUFFER_BINDING            #x8CA7)
   (define GL_MAX_RENDERBUFFER_SIZE           #x84E8)
   (define GL_INVALID_FRAMEBUFFER_OPERATION   #x0506)

))
