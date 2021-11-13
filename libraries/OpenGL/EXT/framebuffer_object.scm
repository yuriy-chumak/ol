; ==========================================================================
; EXT_framebuffer_object                            (included in OpenGL 3.0)
;
;     https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_framebuffer_object.txt
;
; Version
;     Last Modified Date: October 6, 2016
;     Revision: #123
;
; Status:
;     Approved by the ARB "superbuffers" Working Group on January 31, 2005.
;
; Overview
;
;     ...

(define-library (OpenGL EXT framebuffer_object)

; --------------------------------------------------------------------------
; Dependencies
;     a lot of
   (import
      (scheme core)(owl io)
      (OpenGL version-1-1))

; --------------------------------------------------------------------------
   (export  EXT_framebuffer_object

; --------------------------------------------------------------------------
; New Procedures and Functions
   glIsRenderbuffer ;boolean (uint renderbuffer);
   glBindRenderbuffer ;void (enum target, uint renderbuffer);
   glDeleteRenderbuffers ;void (sizei n, const uint *renderbuffers);
   glGenRenderbuffers ;void (sizei n, uint *renderbuffers);

   glRenderbufferStorage ;void (enum target, enum internalformat, sizei width, sizei height);

   glGetRenderbufferParameteriv ;void (enum target, enum pname, int *params);

   glIsFramebuffer ;boolean (uint framebuffer);
   glBindFramebuffer ;void (enum target, uint framebuffer);
   glDeleteFramebuffers ;void (sizei n, const uint *framebuffers);
   glGenFramebuffers ;void (sizei n, uint *framebuffers);

   glCheckFramebufferStatus ;enum (enum target);

   glFramebufferTexture1D ;void (enum target, enum attachment, enum textarget, uint texture, int level);
   glFramebufferTexture2D ;void (enum target, enum attachment, enum textarget, uint texture, int level);
   glFramebufferTexture3D ;void (enum target, enum attachment, enum textarget, uint texture, int level, int zoffset);

   glFramebufferRenderbuffer ;void (enum target, enum attachment, enum renderbuffertarget, uint renderbuffer);

   glGetFramebufferAttachmentParameteriv ;void (enum target, enum attachment, enum pname, int *params);

   glGenerateMipmap ;void (enum target);

; --------------------------------------------------------------------------
; New Tokens

   ;;  Accepted by the <target> parameter of BindFramebufferEXT,
   ;;  CheckFramebufferStatusEXT, FramebufferTexture{1D|2D|3D}EXT,
   ;;  FramebufferRenderbufferEXT, and
   ;;  GetFramebufferAttachmentParameterivEXT:

   GL_FRAMEBUFFER                     ;0x8D40

   ;;  Accepted by the <target> parameter of BindRenderbufferEXT,
   ;;  RenderbufferStorageEXT, and GetRenderbufferParameterivEXT, and
   ;;  returned by GetFramebufferAttachmentParameterivEXT:

   GL_RENDERBUFFER                    ;0x8D41

   ;;  Accepted by the <internalformat> parameter of
   ;;  RenderbufferStorageEXT:

   GL_STENCIL_INDEX1                  ;0x8D46
   GL_STENCIL_INDEX4                  ;0x8D47
   GL_STENCIL_INDEX8                  ;0x8D48
   GL_STENCIL_INDEX16                 ;0x8D49

   ;;  Accepted by the <pname> parameter of GetRenderbufferParameterivEXT:

   GL_RENDERBUFFER_WIDTH              ;0x8D42
   GL_RENDERBUFFER_HEIGHT             ;0x8D43
   GL_RENDERBUFFER_INTERNAL_FORMAT    ;0x8D44
   GL_RENDERBUFFER_RED_SIZE           ;0x8D50
   GL_RENDERBUFFER_GREEN_SIZE         ;0x8D51
   GL_RENDERBUFFER_BLUE_SIZE          ;0x8D52
   GL_RENDERBUFFER_ALPHA_SIZE         ;0x8D53
   GL_RENDERBUFFER_DEPTH_SIZE         ;0x8D54
   GL_RENDERBUFFER_STENCIL_SIZE       ;0x8D55

   ;;  Accepted by the <pname> parameter of
   ;;  GetFramebufferAttachmentParameterivEXT:

   GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE            ;0x8CD0
   GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME            ;0x8CD1
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL          ;0x8CD2
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE  ;0x8CD3
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET     ;0x8CD4

   ;;  Accepted by the <attachment> parameter of
   ;;  FramebufferTexture{1D|2D|3D}EXT, FramebufferRenderbufferEXT, and
   ;;  GetFramebufferAttachmentParameterivEXT

   GL_COLOR_ATTACHMENT0                ;0x8CE0
   GL_COLOR_ATTACHMENT1                ;0x8CE1
   GL_COLOR_ATTACHMENT2                ;0x8CE2
   GL_COLOR_ATTACHMENT3                ;0x8CE3
   GL_COLOR_ATTACHMENT4                ;0x8CE4
   GL_COLOR_ATTACHMENT5                ;0x8CE5
   GL_COLOR_ATTACHMENT6                ;0x8CE6
   GL_COLOR_ATTACHMENT7                ;0x8CE7
   GL_COLOR_ATTACHMENT8                ;0x8CE8
   GL_COLOR_ATTACHMENT9                ;0x8CE9
   GL_COLOR_ATTACHMENT10               ;0x8CEA
   GL_COLOR_ATTACHMENT11               ;0x8CEB
   GL_COLOR_ATTACHMENT12               ;0x8CEC
   GL_COLOR_ATTACHMENT13               ;0x8CED
   GL_COLOR_ATTACHMENT14               ;0x8CEE
   GL_COLOR_ATTACHMENT15               ;0x8CEF
   GL_DEPTH_ATTACHMENT                 ;0x8D00
   GL_STENCIL_ATTACHMENT               ;0x8D20

   ;;  Returned by CheckFramebufferStatus():

   GL_FRAMEBUFFER_COMPLETE                          ;0x8CD5
   GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT             ;0x8CD6
   GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT     ;0x8CD7
   GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS             ;0x8CD9
   GL_FRAMEBUFFER_INCOMPLETE_FORMATS                ;0x8CDA
   GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER            ;0x8CDB
   GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER            ;0x8CDC
   GL_FRAMEBUFFER_UNSUPPORTED                       ;0x8CDD

   ;;  Accepted by GetIntegerv():

   GL_FRAMEBUFFER_BINDING             ;0x8CA6
   GL_RENDERBUFFER_BINDING            ;0x8CA7
   GL_MAX_COLOR_ATTACHMENTS           ;0x8CDF
   GL_MAX_RENDERBUFFER_SIZE           ;0x84E8

   ;;  Returned by GetError():

   GL_INVALID_FRAMEBUFFER_OPERATION   ;0x0506

)

; --------------------------------------------------------------------------
(begin
   (define EXT_framebuffer_object (gl:QueryExtension "GL_EXT_framebuffer_object"))
   (setq GL gl:GetProcAddress)

   (define glIsRenderbuffer (GL GLboolean "glIsRenderbufferEXT" GLuint))
   (define glBindRenderbuffer (GL GLvoid "glBindRenderbufferEXT" GLenum GLuint))
   (define glDeleteRenderbuffers (GL GLvoid "glDeleteRenderbuffersEXT" GLsizei GLuint*))
   (define glGenRenderbuffers (GL GLvoid "glGenRenderbuffersEXT" GLsizei GLuint&))

   (define glRenderbufferStorage (GL GLvoid "glRenderbufferStorageEXT" GLenum GLenum GLsizei GLsizei))
   (define glGetRenderbufferParameteriv (GL GLvoid "glGetRenderbufferParameterivEXT" GLenum GLenum GLint*))

   (define glIsFramebuffer (GL GLboolean "glIsFramebufferEXT" GLuint))
   (define glBindFramebuffer (GL GLvoid "glBindFramebufferEXT" GLenum GLuint))
   (define glDeleteFramebuffers (GL GLvoid "glDeleteFramebuffersEXT" GLsizei GLuint*))
   (define glGenFramebuffers (GL GLvoid "glGenFramebuffersEXT" GLsizei GLuint&))

   (define glCheckFramebufferStatus (GL GLvoid "glCheckFramebufferStatusEXT" GLenum GLenum))

   (define glFramebufferTexture1D (GL GLvoid "glFramebufferTexture1DEXT" GLenum GLenum GLenum GLuint GLint))
   (define glFramebufferTexture2D (GL GLvoid "glFramebufferTexture2DEXT" GLenum GLenum GLenum GLuint GLint))
   (define glFramebufferTexture3D (GL GLvoid "glFramebufferTexture3DEXT" GLenum GLenum GLenum GLuint GLint GLint))

   (define glFramebufferRenderbuffer (GL GLvoid "glFramebufferRenderbufferEXT" GLenum GLenum GLenum GLuint))

   (define glGetFramebufferAttachmentParameteriv (GL GLvoid "glGetFramebufferAttachmentParameterivEXT" GLenum GLenum GLenum GLint*))

   (define glGenerateMipmap (GL GLvoid "glGenerateMipmapEXT" GLenum))


   (define GL_FRAMEBUFFER                     #x8D40)
   (define GL_RENDERBUFFER                    #x8D41)
   (define GL_STENCIL_INDEX1                  #x8D46)
   (define GL_STENCIL_INDEX4                  #x8D47)
   (define GL_STENCIL_INDEX8                  #x8D48)
   (define GL_STENCIL_INDEX16                 #x8D49)
   (define GL_RENDERBUFFER_WIDTH              #x8D42)
   (define GL_RENDERBUFFER_HEIGHT             #x8D43)
   (define GL_RENDERBUFFER_INTERNAL_FORMAT    #x8D44)
   (define GL_RENDERBUFFER_RED_SIZE           #x8D50)
   (define GL_RENDERBUFFER_GREEN_SIZE         #x8D51)
   (define GL_RENDERBUFFER_BLUE_SIZE          #x8D52)
   (define GL_RENDERBUFFER_ALPHA_SIZE         #x8D53)
   (define GL_RENDERBUFFER_DEPTH_SIZE         #x8D54)
   (define GL_RENDERBUFFER_STENCIL_SIZE       #x8D55)
   (define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE            #x8CD0)
   (define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME            #x8CD1)
   (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL          #x8CD2)
   (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE  #x8CD3)
   (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET     #x8CD4)
   (define GL_COLOR_ATTACHMENT0                #x8CE0)
   (define GL_COLOR_ATTACHMENT1                #x8CE1)
   (define GL_COLOR_ATTACHMENT2                #x8CE2)
   (define GL_COLOR_ATTACHMENT3                #x8CE3)
   (define GL_COLOR_ATTACHMENT4                #x8CE4)
   (define GL_COLOR_ATTACHMENT5                #x8CE5)
   (define GL_COLOR_ATTACHMENT6                #x8CE6)
   (define GL_COLOR_ATTACHMENT7                #x8CE7)
   (define GL_COLOR_ATTACHMENT8                #x8CE8)
   (define GL_COLOR_ATTACHMENT9                #x8CE9)
   (define GL_COLOR_ATTACHMENT10               #x8CEA)
   (define GL_COLOR_ATTACHMENT11               #x8CEB)
   (define GL_COLOR_ATTACHMENT12               #x8CEC)
   (define GL_COLOR_ATTACHMENT13               #x8CED)
   (define GL_COLOR_ATTACHMENT14               #x8CEE)
   (define GL_COLOR_ATTACHMENT15               #x8CEF)
   (define GL_DEPTH_ATTACHMENT                 #x8D00)
   (define GL_STENCIL_ATTACHMENT               #x8D20)
   (define GL_FRAMEBUFFER_COMPLETE                          #x8CD5)
   (define GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT             #x8CD6)
   (define GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT     #x8CD7)
   (define GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS             #x8CD9)
   (define GL_FRAMEBUFFER_INCOMPLETE_FORMATS                #x8CDA)
   (define GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER            #x8CDB)
   (define GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER            #x8CDC)
   (define GL_FRAMEBUFFER_UNSUPPORTED                       #x8CDD)
   (define GL_FRAMEBUFFER_BINDING             #x8CA6)
   (define GL_RENDERBUFFER_BINDING            #x8CA7)
   (define GL_MAX_COLOR_ATTACHMENTS           #x8CDF)
   (define GL_MAX_RENDERBUFFER_SIZE           #x84E8)
   (define GL_INVALID_FRAMEBUFFER_OPERATION   #x0506)

))
