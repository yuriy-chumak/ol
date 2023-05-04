; ==========================================================================
; ARB_framebuffer_object
;     ARB_framebuffer_object defines an interface for drawing to rendering
;     destinations other than the buffers provided to the GL by the
;     window-system.
;
;     https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_framebuffer_object.txt
;
; Version
;     Last Modified Date: October 6, 2016
;     Revision: #38
;
; Status:
;     Approved by the ARB on August 4, 2008
;
; Overview
;
;     This extension differs from EXT_framebuffer_object by splitting the
;     framebuffer object binding point into separate DRAW and READ
;     bindings (incorporating functionality introduced by
;     EXT_framebuffer_blit). This allows copying directly from one
;     framebuffer to another. In addition, a new high performance blit
;     function is added to facilitate these blits and perform some data
;     conversion where allowed.
;
;     ...

(define-library (OpenGL ARB framebuffer_object)

; --------------------------------------------------------------------------
; Dependencies
;     a lot of
   (import
      (scheme core) (owl io)
      (OpenGL 1.1))

; --------------------------------------------------------------------------
   (export  ARB_framebuffer_object

; --------------------------------------------------------------------------
; New Procedures and Functions

    glIsRenderbuffer ;boolean (uint renderbuffer);
    glBindRenderbuffer ;void (enum target, uint renderbuffer);
    glDeleteRenderbuffers ;void (sizei n, const uint *renderbuffers);
    glGenRenderbuffers ;void (sizei n, uint *renderbuffers);

    glRenderbufferStorage ;void (enum target, enum internalformat, sizei width, sizei height);

    glRenderbufferStorageMultisample ;void (enum target, sizei samples, enum internalformat, sizei width, sizei height);

    glGetRenderbufferParameteriv ;void (enum target, enum pname, int *params);

    glIsFramebuffer ;boolean (uint framebuffer);
    glBindFramebuffer ;void (enum target, uint framebuffer);
    glDeleteFramebuffers ;void (sizei n, const uint *framebuffers);
    glGenFramebuffers ;void (sizei n, uint *framebuffers);

    glCheckFramebufferStatus ;enum (enum target);

    glFramebufferTexture1D ;void (enum target, enum attachment, enum textarget, uint texture, int level);
    glFramebufferTexture2D ;void (enum target, enum attachment, enum textarget, uint texture, int level);
    glFramebufferTexture3D ;void (enum target, enum attachment, enum textarget, uint texture, int level, int layer);
    glFramebufferTextureLayer ;void (enum target,enum attachment, uint texture,int level,int layer);

    glFramebufferRenderbuffer ;void (enum target, enum attachment, enum renderbuffertarget, uint renderbuffer);

    glGetFramebufferAttachmentParameteriv ;void (enum target, enum attachment, enum pname, int *params);

    glBlitFramebuffer ;void (int srcX0, int srcY0, int srcX1, int srcY1,
                            ;int dstX0, int dstY0, int dstX1, int dstY1,
                            ;bitfield mask, enum filter);

    glGenerateMipmap ;void (enum target);

; --------------------------------------------------------------------------
; New Tokens
;
;    Accepted by the <target> parameter of BindFramebuffer,
;    CheckFramebufferStatus, FramebufferTexture{1D|2D|3D},
;    FramebufferRenderbuffer, and
;    GetFramebufferAttachmentParameteriv:
;
        GL_FRAMEBUFFER                     ;0x8D40
        GL_READ_FRAMEBUFFER                ;0x8CA8
        GL_DRAW_FRAMEBUFFER                ;0x8CA9
;
;    Accepted by the <target> parameter of BindRenderbuffer,
;    RenderbufferStorage, and GetRenderbufferParameteriv, and
;    returned by GetFramebufferAttachmentParameteriv:
;
        GL_RENDERBUFFER                    ;0x8D41
;
;    Accepted by the <internalformat> parameter of
;    RenderbufferStorage:
;
        GL_STENCIL_INDEX1                  ;0x8D46
        GL_STENCIL_INDEX4                  ;0x8D47
        GL_STENCIL_INDEX8                  ;0x8D48
        GL_STENCIL_INDEX16                 ;0x8D49
;
;    Accepted by the <pname> parameter of GetRenderbufferParameteriv:
;
        GL_RENDERBUFFER_WIDTH              ;0x8D42
        GL_RENDERBUFFER_HEIGHT             ;0x8D43
        GL_RENDERBUFFER_INTERNAL_FORMAT    ;0x8D44
        GL_RENDERBUFFER_RED_SIZE           ;0x8D50
        GL_RENDERBUFFER_GREEN_SIZE         ;0x8D51
        GL_RENDERBUFFER_BLUE_SIZE          ;0x8D52
        GL_RENDERBUFFER_ALPHA_SIZE         ;0x8D53
        GL_RENDERBUFFER_DEPTH_SIZE         ;0x8D54
        GL_RENDERBUFFER_STENCIL_SIZE       ;0x8D55
        GL_RENDERBUFFER_SAMPLES            ;0x8CAB
;
;    Accepted by the <pname> parameter of
;    GetFramebufferAttachmentParameteriv:
;
        GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE            ;0x8CD0
        GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME            ;0x8CD1
        GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL          ;0x8CD2
        GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE  ;0x8CD3
        GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER          ;0x8CD4
        GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING         ;0x8210
        GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE         ;0x8211
        GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE               ;0x8212
        GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE             ;0x8213
        GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE              ;0x8214
        GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE             ;0x8215
        GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE             ;0x8216
        GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE           ;0x8217
;
;    Returned in <params> by GetFramebufferAttachmentParameteriv:
;
        GL_SRGB                                          ;0x8C40
        GL_UNSIGNED_NORMALIZED                           ;0x8C17
        GL_FRAMEBUFFER_DEFAULT                           ;0x8218
        GL_INDEX                                         ;0x8222
;
;    Accepted by the <attachment> parameter of
;    FramebufferTexture{1D|2D|3D}, FramebufferRenderbuffer, and
;    GetFramebufferAttachmentParameteriv
;
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
        GL_DEPTH_STENCIL_ATTACHMENT         ;0x821A
;
;    Accepted by the <pname> parameter of GetBooleanv, GetIntegerv,
;    GetFloatv, and GetDoublev:
;
        GL_MAX_SAMPLES                     ;0x8D57
        GL_FRAMEBUFFER_BINDING             ;0x8CA6 // alias DRAW_FRAMEBUFFER_BINDING
        GL_DRAW_FRAMEBUFFER_BINDING        ;0x8CA6
        GL_READ_FRAMEBUFFER_BINDING        ;0x8CAA
        GL_RENDERBUFFER_BINDING            ;0x8CA7
        GL_MAX_COLOR_ATTACHMENTS           ;0x8CDF
        GL_MAX_RENDERBUFFER_SIZE           ;0x84E8
;
;
;    Returned by CheckFramebufferStatus():
;
        GL_FRAMEBUFFER_COMPLETE                          ;0x8CD5
        GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT             ;0x8CD6
        GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT     ;0x8CD7
        GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER            ;0x8CDB
        GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER            ;0x8CDC
        GL_FRAMEBUFFER_UNSUPPORTED                       ;0x8CDD
        GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE            ;0x8D56
        GL_FRAMEBUFFER_UNDEFINED                         ;0x8219
;
;    Returned by GetError():
;
        GL_INVALID_FRAMEBUFFER_OPERATION   ;0x0506
;
;    Accepted by the <format> parameter of DrawPixels, ReadPixels,
;    TexImage1D, TexImage2D, TexImage3D, TexSubImage1D, TexSubImage2D,
;    TexSubImage3D, and GetTexImage, by the <type> parameter of
;    CopyPixels, by the <internalformat> parameter of TexImage1D,
;    TexImage2D, TexImage3D, CopyTexImage1D, CopyTexImage2D, and
;    RenderbufferStorage, and returned in the <data> parameter of
;    GetTexLevelParameter and GetRenderbufferParameteriv:
;
        GL_DEPTH_STENCIL                              ;0x84F9
;
;    Accepted by the <type> parameter of DrawPixels, ReadPixels,
;    TexImage1D, TexImage2D, TexImage3D, TexSubImage1D, TexSubImage2D,
;    TexSubImage3D, and GetTexImage:
;
        GL_UNSIGNED_INT_24_8                          ;0x84FA
;
;    Accepted by the <internalformat> parameter of TexImage1D,
;    TexImage2D, TexImage3D, CopyTexImage1D, CopyTexImage2D, and
;    RenderbufferStorage, and returned in the <data> parameter of
;    GetTexLevelParameter and GetRenderbufferParameteriv:
;
        GL_DEPTH24_STENCIL8                           ;0x88F0
;
;    Accepted by the <value> parameter of GetTexLevelParameter:
;
        GL_TEXTURE_STENCIL_SIZE                       ;0x88F1
)

; --------------------------------------------------------------------------
(begin
   (define ARB_framebuffer_object (gl:QueryExtension "GL_ARB_framebuffer_object"))

   (setq GL gl:GetProcAddress)
   (define glIsRenderbuffer (GL GLboolean "glIsRenderbuffer" GLuint))
   (define glBindRenderbuffer (GL GLvoid "glBindRenderbuffer" GLenum GLuint))
   (define glDeleteRenderbuffers (GL GLvoid "glDeleteRenderbuffers" GLsizei GLuint*))
   (define glGenRenderbuffers (GL GLvoid "glGenRenderbuffers" GLsizei GLuint&))

   (define glRenderbufferStorage (GL GLvoid "glRenderbufferStorage" GLenum GLenum GLsizei GLsizei))
   (define glRenderbufferStorageMultisample (GL GLvoid "glRenderbufferStorageMultisample" GLenum GLsizei GLenum GLsizei GLsizei))
   (define glGetRenderbufferParameteriv (GL GLvoid "glGetRenderbufferParameteriv" GLenum GLenum GLint*))
   (define glIsFramebuffer (GL GLboolean "glIsFramebuffer" GLuint))

   (define glBindFramebuffer (GL GLvoid "glBindFramebuffer" GLenum GLuint))
   (define glDeleteFramebuffers (GL GLvoid "glDeleteFramebuffers" GLsizei GLuint*))
   (define glGenFramebuffers (GL GLvoid "glGenFramebuffers" GLsizei GLuint&))

   (define glCheckFramebufferStatus (GL GLenum "glCheckFramebufferStatus" GLenum))

   (define glFramebufferTexture1D (GL GLvoid "glFramebufferTexture1D" GLenum GLenum GLenum GLuint GLint))
   (define glFramebufferTexture2D (GL GLvoid "glFramebufferTexture2D" GLenum GLenum GLenum GLuint GLint))
   (define glFramebufferTexture3D (GL GLvoid "glFramebufferTexture3D" GLenum GLenum GLenum GLuint GLint GLint))
   (define glFramebufferTextureLayer (GL GLvoid "glFramebufferTextureLayer" GLenum GLenum GLuint GLint GLint))
   (define glFramebufferRenderbuffer (GL GLvoid "glFramebufferRenderbuffer" GLenum GLenum GLenum GLuint))

   (define glGetFramebufferAttachmentParameteriv (GL GLvoid "glGetFramebufferAttachmentParameteriv" GLenum GLenum GLenum GLint*))
   (define glBlitFramebuffer (GL GLvoid "glBlitFramebuffer" GLint GLint GLint GLint GLint GLint GLint GLint GLbitfield GLenum))

   (define glGenerateMipmap (GL GLvoid "glGenerateMipmap" GLenum))


   (define GL_FRAMEBUFFER                     #x8D40)
   (define GL_READ_FRAMEBUFFER                #x8CA8)
   (define GL_DRAW_FRAMEBUFFER                #x8CA9)
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
   (define GL_RENDERBUFFER_SAMPLES            #x8CAB)
   (define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE            #x8CD0)
   (define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME            #x8CD1)
   (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL          #x8CD2)
   (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE  #x8CD3)
   (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER          #x8CD4)
   (define GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING         #x8210)
   (define GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE         #x8211)
   (define GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE               #x8212)
   (define GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE             #x8213)
   (define GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE              #x8214)
   (define GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE             #x8215)
   (define GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE             #x8216)
   (define GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE           #x8217)
   (define GL_SRGB                                          #x8C40)
   (define GL_UNSIGNED_NORMALIZED                           #x8C17)
   (define GL_FRAMEBUFFER_DEFAULT                           #x8218)
   (define GL_INDEX                                         #x8222)
   (define GL_COLOR_ATTACHMENT0               #x8CE0)
   (define GL_COLOR_ATTACHMENT1               #x8CE1)
   (define GL_COLOR_ATTACHMENT2               #x8CE2)
   (define GL_COLOR_ATTACHMENT3               #x8CE3)
   (define GL_COLOR_ATTACHMENT4               #x8CE4)
   (define GL_COLOR_ATTACHMENT5               #x8CE5)
   (define GL_COLOR_ATTACHMENT6               #x8CE6)
   (define GL_COLOR_ATTACHMENT7               #x8CE7)
   (define GL_COLOR_ATTACHMENT8               #x8CE8)
   (define GL_COLOR_ATTACHMENT9               #x8CE9)
   (define GL_COLOR_ATTACHMENT10              #x8CEA)
   (define GL_COLOR_ATTACHMENT11              #x8CEB)
   (define GL_COLOR_ATTACHMENT12              #x8CEC)
   (define GL_COLOR_ATTACHMENT13              #x8CED)
   (define GL_COLOR_ATTACHMENT14              #x8CEE)
   (define GL_COLOR_ATTACHMENT15              #x8CEF)
   (define GL_DEPTH_ATTACHMENT                #x8D00)
   (define GL_STENCIL_ATTACHMENT              #x8D20)
   (define GL_DEPTH_STENCIL_ATTACHMENT        #x821A)
   (define GL_MAX_SAMPLES                     #x8D57)
   (define GL_FRAMEBUFFER_BINDING             #x8CA6) ; alias DRAW_FRAMEBUFFER_BINDING
   (define GL_DRAW_FRAMEBUFFER_BINDING        #x8CA6)
   (define GL_READ_FRAMEBUFFER_BINDING        #x8CAA)
   (define GL_RENDERBUFFER_BINDING            #x8CA7)
   (define GL_MAX_COLOR_ATTACHMENTS           #x8CDF)
   (define GL_MAX_RENDERBUFFER_SIZE           #x84E8)
   (define GL_FRAMEBUFFER_COMPLETE                          #x8CD5)
   (define GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT             #x8CD6)
   (define GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT     #x8CD7)
   (define GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER            #x8CDB)
   (define GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER            #x8CDC)
   (define GL_FRAMEBUFFER_UNSUPPORTED                       #x8CDD)
   (define GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE            #x8D56)
   (define GL_FRAMEBUFFER_UNDEFINED                         #x8219)
   (define GL_INVALID_FRAMEBUFFER_OPERATION   #x0506)
   (define GL_DEPTH_STENCIL                   #x84F9)
   (define GL_UNSIGNED_INT_24_8               #x84FA)
   (define GL_DEPTH24_STENCIL8                #x88F0)
   (define GL_TEXTURE_STENCIL_SIZE            #x88F1)

))
