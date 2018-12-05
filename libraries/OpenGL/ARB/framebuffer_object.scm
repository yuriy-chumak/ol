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
;     In this extension, these newly defined rendering destinations are
;     known collectively as "framebuffer-attachable images".  This
;     extension provides a mechanism for attaching framebuffer-attachable
;     images to the GL framebuffer as one of the standard GL logical
;     buffers: color, depth, and stencil.  (Attaching a
;     framebuffer-attachable image to the accum logical buffer is left for
;     a future extension to define).  When a framebuffer-attachable image
;     is attached to the framebuffer, it is used as the source and
;     destination of fragment operations as described in Chapter 4.
;
;     ...

(define-library (OpenGL ARB framebuffer_object)

; --------------------------------------------------------------------------
; Dependencies
;     a lot of
   (import
      (scheme core) (owl io)
      (OpenGL version-1-1))

; --------------------------------------------------------------------------
   (export  ARB_framebuffer_object

; --------------------------------------------------------------------------
; New Procedures and Functions
   glIsRenderbuffer ; boolean (uint renderbuffer)
;    void BindRenderbuffer(enum target, uint renderbuffer);
;    void DeleteRenderbuffers(sizei n, const uint *renderbuffers);
;    void GenRenderbuffers(sizei n, uint *renderbuffers);
;
;    void RenderbufferStorage(enum target, enum internalformat,
;                             sizei width, sizei height);
;
;    void RenderbufferStorageMultisample(enum target, sizei samples,
;                                        enum internalformat,
;                                        sizei width, sizei height);
;
;    void GetRenderbufferParameteriv(enum target, enum pname, int *params);
;
;    boolean IsFramebuffer(uint framebuffer);
   glBindFramebuffer ; void (enum target, uint framebuffer)
;    void DeleteFramebuffers(sizei n, const uint *framebuffers);
   glGenFramebuffers ; void (sizei n, uint *framebuffers)
;
;    enum CheckFramebufferStatus(enum target);
;
   glFramebufferTexture1D ; void (enum target, enum attachment, enum textarget, uint texture, int level);
   glFramebufferTexture2D ; void (enum target, enum attachment, enum textarget, uint texture, int level);
;    void FramebufferTexture3D(enum target, enum attachment,
;                              enum textarget, uint texture,
;                              int level, int layer);
;    void FramebufferTextureLayer(enum target,enum attachment,
;                                 uint texture,int level,int layer);
;
;    void FramebufferRenderbuffer(enum target, enum attachment,
;                                 enum renderbuffertarget, uint renderbuffer);
;
;    void GetFramebufferAttachmentParameteriv(enum target, enum attachment,
;                                             enum pname, int *params);
;
;    void BlitFramebuffer(int srcX0, int srcY0, int srcX1, int srcY1,
;                         int dstX0, int dstY0, int dstX1, int dstY1,
;                         bitfield mask, enum filter);
;
;    void GenerateMipmap(enum target);
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
;        RENDERBUFFER                    0x8D41
;
;    Accepted by the <internalformat> parameter of
;    RenderbufferStorage:
;
;        STENCIL_INDEX1                  0x8D46
;        STENCIL_INDEX4                  0x8D47
;        STENCIL_INDEX8                  0x8D48
;        STENCIL_INDEX16                 0x8D49
;
;    Accepted by the <pname> parameter of GetRenderbufferParameteriv:
;
;        RENDERBUFFER_WIDTH              0x8D42
;        RENDERBUFFER_HEIGHT             0x8D43
;        RENDERBUFFER_INTERNAL_FORMAT    0x8D44
;        RENDERBUFFER_RED_SIZE           0x8D50
;        RENDERBUFFER_GREEN_SIZE         0x8D51
;        RENDERBUFFER_BLUE_SIZE          0x8D52
;        RENDERBUFFER_ALPHA_SIZE         0x8D53
;        RENDERBUFFER_DEPTH_SIZE         0x8D54
;        RENDERBUFFER_STENCIL_SIZE       0x8D55
;        RENDERBUFFER_SAMPLES            0x8CAB
;
;    Accepted by the <pname> parameter of
;    GetFramebufferAttachmentParameteriv:
;
;        FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE            0x8CD0
;        FRAMEBUFFER_ATTACHMENT_OBJECT_NAME            0x8CD1
;        FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL          0x8CD2
;        FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE  0x8CD3
;        FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER          0x8CD4
;        FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING         0x8210
;        FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE         0x8211
;        FRAMEBUFFER_ATTACHMENT_RED_SIZE               0x8212
;        FRAMEBUFFER_ATTACHMENT_GREEN_SIZE             0x8213
;        FRAMEBUFFER_ATTACHMENT_BLUE_SIZE              0x8214
;        FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE             0x8215
;        FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE             0x8216
;        FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE           0x8217
;
;    Returned in <params> by GetFramebufferAttachmentParameteriv:
;
;        SRGB                                          0x8C40
;        UNSIGNED_NORMALIZED                           0x8C17
;        FRAMEBUFFER_DEFAULT                           0x8218
;        INDEX                                         0x8222
;
;    Accepted by the <attachment> parameter of
;    FramebufferTexture{1D|2D|3D}, FramebufferRenderbuffer, and
;    GetFramebufferAttachmentParameteriv
;
;        COLOR_ATTACHMENT0                0x8CE0
;        COLOR_ATTACHMENT1                0x8CE1
;        COLOR_ATTACHMENT2                0x8CE2
;        COLOR_ATTACHMENT3                0x8CE3
;        COLOR_ATTACHMENT4                0x8CE4
;        COLOR_ATTACHMENT5                0x8CE5
;        COLOR_ATTACHMENT6                0x8CE6
;        COLOR_ATTACHMENT7                0x8CE7
;        COLOR_ATTACHMENT8                0x8CE8
;        COLOR_ATTACHMENT9                0x8CE9
;        COLOR_ATTACHMENT10               0x8CEA
;        COLOR_ATTACHMENT11               0x8CEB
;        COLOR_ATTACHMENT12               0x8CEC
;        COLOR_ATTACHMENT13               0x8CED
;        COLOR_ATTACHMENT14               0x8CEE
;        COLOR_ATTACHMENT15               0x8CEF
   GL_DEPTH_ATTACHMENT                 ; 0x8D00
;        STENCIL_ATTACHMENT               0x8D20
;        DEPTH_STENCIL_ATTACHMENT         0x821A
;
;    Accepted by the <pname> parameter of GetBooleanv, GetIntegerv,
;    GetFloatv, and GetDoublev:
;
;        MAX_SAMPLES                     0x8D57
;        FRAMEBUFFER_BINDING             0x8CA6 // alias DRAW_FRAMEBUFFER_BINDING
;        DRAW_FRAMEBUFFER_BINDING        0x8CA6
;        READ_FRAMEBUFFER_BINDING        0x8CAA
;        RENDERBUFFER_BINDING            0x8CA7
;        MAX_COLOR_ATTACHMENTS           0x8CDF
;        MAX_RENDERBUFFER_SIZE           0x84E8
;
;
;    Returned by CheckFramebufferStatus():
;
;        FRAMEBUFFER_COMPLETE                          0x8CD5
;        FRAMEBUFFER_INCOMPLETE_ATTACHMENT             0x8CD6
;        FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT     0x8CD7
;        FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER            0x8CDB
;        FRAMEBUFFER_INCOMPLETE_READ_BUFFER            0x8CDC
;        FRAMEBUFFER_UNSUPPORTED                       0x8CDD
;        FRAMEBUFFER_INCOMPLETE_MULTISAMPLE            0x8D56
;        FRAMEBUFFER_UNDEFINED                         0x8219
;
;    Returned by GetError():
;
;        INVALID_FRAMEBUFFER_OPERATION   0x0506
;
;    Accepted by the <format> parameter of DrawPixels, ReadPixels,
;    TexImage1D, TexImage2D, TexImage3D, TexSubImage1D, TexSubImage2D,
;    TexSubImage3D, and GetTexImage, by the <type> parameter of
;    CopyPixels, by the <internalformat> parameter of TexImage1D,
;    TexImage2D, TexImage3D, CopyTexImage1D, CopyTexImage2D, and
;    RenderbufferStorage, and returned in the <data> parameter of
;    GetTexLevelParameter and GetRenderbufferParameteriv:
;
;        DEPTH_STENCIL                              0x84F9
;
;    Accepted by the <type> parameter of DrawPixels, ReadPixels,
;    TexImage1D, TexImage2D, TexImage3D, TexSubImage1D, TexSubImage2D,
;    TexSubImage3D, and GetTexImage:
;
;        UNSIGNED_INT_24_8                          0x84FA
;
;    Accepted by the <internalformat> parameter of TexImage1D,
;    TexImage2D, TexImage3D, CopyTexImage1D, CopyTexImage2D, and
;    RenderbufferStorage, and returned in the <data> parameter of
;    GetTexLevelParameter and GetRenderbufferParameteriv:
;
;        DEPTH24_STENCIL8                           0x88F0
;
;    Accepted by the <value> parameter of GetTexLevelParameter:
;
;        TEXTURE_STENCIL_SIZE 0x88F1

)

; --------------------------------------------------------------------------
(begin
   (define ARB_framebuffer_object (gl:QueryExtension "GL_ARB_framebuffer_object"))

;    boolean IsRenderbuffer(uint renderbuffer);
   (define glIsRenderbuffer (if ARB_framebuffer_object
         (gl:GetProcAddress GLboolean "glIsRenderbuffer" GLuint)))
;    void BindRenderbuffer(enum target, uint renderbuffer);
;    void DeleteRenderbuffers(sizei n, const uint *renderbuffers);
;    void GenRenderbuffers(sizei n, uint *renderbuffers);
;
;    void RenderbufferStorage(enum target, enum internalformat,
;                             sizei width, sizei height);
;
;    void RenderbufferStorageMultisample(enum target, sizei samples,
;                                        enum internalformat,
;                                        sizei width, sizei height);
;
;    void GetRenderbufferParameteriv(enum target, enum pname, int *params);
;
;    boolean IsFramebuffer(uint framebuffer);
   (define glBindFramebuffer (if ARB_framebuffer_object
         (gl:GetProcAddress GLvoid "glBindFramebuffer" GLenum GLuint)))
;    void DeleteFramebuffers(sizei n, const uint *framebuffers);
;    void GenFramebuffers(sizei n, uint *framebuffers);
   (define glGenFramebuffers (if ARB_framebuffer_object
         (gl:GetProcAddress GLvoid "glGenFramebuffers" GLsizei GLuint&)))
;
;    enum CheckFramebufferStatus(enum target);
;
   (define glFramebufferTexture1D (if ARB_framebuffer_object
         (gl:GetProcAddress GLvoid "FramebufferTexture1D" GLenum GLenum GLenum GLuint GLint)))
   (define glFramebufferTexture2D (if ARB_framebuffer_object
         (gl:GetProcAddress GLvoid "FramebufferTexture2D" GLenum GLenum GLenum GLuint GLint)))
;    void FramebufferTexture3D(enum target, enum attachment,
;                              enum textarget, uint texture,
;                              int level, int layer);
;    void FramebufferTextureLayer(enum target,enum attachment,
;                                 uint texture,int level,int layer);
;
;    void FramebufferRenderbuffer(enum target, enum attachment,
;                                 enum renderbuffertarget, uint renderbuffer);
;
;    void GetFramebufferAttachmentParameteriv(enum target, enum attachment,
;                                             enum pname, int *params);
;
;    void BlitFramebuffer(int srcX0, int srcY0, int srcX1, int srcY1,
;                         int dstX0, int dstY0, int dstX1, int dstY1,
;                         bitfield mask, enum filter);
;
;    void GenerateMipmap(enum target);


   (define GL_FRAMEBUFFER                     #x8D40)
   (define GL_READ_FRAMEBUFFER                #x8CA8)
   (define GL_DRAW_FRAMEBUFFER                #x8CA9)
;        RENDERBUFFER                    0x8D41
;        STENCIL_INDEX1                  0x8D46
;        STENCIL_INDEX4                  0x8D47
;        STENCIL_INDEX8                  0x8D48
;        STENCIL_INDEX16                 0x8D49
;        RENDERBUFFER_WIDTH              0x8D42
;        RENDERBUFFER_HEIGHT             0x8D43
;        RENDERBUFFER_INTERNAL_FORMAT    0x8D44
;        RENDERBUFFER_RED_SIZE           0x8D50
;        RENDERBUFFER_GREEN_SIZE         0x8D51
;        RENDERBUFFER_BLUE_SIZE          0x8D52
;        RENDERBUFFER_ALPHA_SIZE         0x8D53
;        RENDERBUFFER_DEPTH_SIZE         0x8D54
;        RENDERBUFFER_STENCIL_SIZE       0x8D55
;        RENDERBUFFER_SAMPLES            0x8CAB
;        FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE            0x8CD0
;        FRAMEBUFFER_ATTACHMENT_OBJECT_NAME            0x8CD1
;        FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL          0x8CD2
;        FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE  0x8CD3
;        FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER          0x8CD4
;        FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING         0x8210
;        FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE         0x8211
;        FRAMEBUFFER_ATTACHMENT_RED_SIZE               0x8212
;        FRAMEBUFFER_ATTACHMENT_GREEN_SIZE             0x8213
;        FRAMEBUFFER_ATTACHMENT_BLUE_SIZE              0x8214
;        FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE             0x8215
;        FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE             0x8216
;        FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE           0x8217
;        SRGB                                          0x8C40
;        UNSIGNED_NORMALIZED                           0x8C17
;        FRAMEBUFFER_DEFAULT                           0x8218
;        INDEX                                         0x8222
;        COLOR_ATTACHMENT0                0x8CE0
;        COLOR_ATTACHMENT1                0x8CE1
;        COLOR_ATTACHMENT2                0x8CE2
;        COLOR_ATTACHMENT3                0x8CE3
;        COLOR_ATTACHMENT4                0x8CE4
;        COLOR_ATTACHMENT5                0x8CE5
;        COLOR_ATTACHMENT6                0x8CE6
;        COLOR_ATTACHMENT7                0x8CE7
;        COLOR_ATTACHMENT8                0x8CE8
;        COLOR_ATTACHMENT9                0x8CE9
;        COLOR_ATTACHMENT10               0x8CEA
;        COLOR_ATTACHMENT11               0x8CEB
;        COLOR_ATTACHMENT12               0x8CEC
;        COLOR_ATTACHMENT13               0x8CED
;        COLOR_ATTACHMENT14               0x8CEE
;        COLOR_ATTACHMENT15               0x8CEF
   (define GL_DEPTH_ATTACHMENT                 #x8D00)
;        STENCIL_ATTACHMENT               0x8D20
;        DEPTH_STENCIL_ATTACHMENT         0x821A
;        MAX_SAMPLES                     0x8D57
;        FRAMEBUFFER_BINDING             0x8CA6 // alias DRAW_FRAMEBUFFER_BINDING
;        DRAW_FRAMEBUFFER_BINDING        0x8CA6
;        READ_FRAMEBUFFER_BINDING        0x8CAA
;        RENDERBUFFER_BINDING            0x8CA7
;        MAX_COLOR_ATTACHMENTS           0x8CDF
;        MAX_RENDERBUFFER_SIZE           0x84E8
;        FRAMEBUFFER_COMPLETE                          0x8CD5
;        FRAMEBUFFER_INCOMPLETE_ATTACHMENT             0x8CD6
;        FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT     0x8CD7
;        FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER            0x8CDB
;        FRAMEBUFFER_INCOMPLETE_READ_BUFFER            0x8CDC
;        FRAMEBUFFER_UNSUPPORTED                       0x8CDD
;        FRAMEBUFFER_INCOMPLETE_MULTISAMPLE            0x8D56
;        FRAMEBUFFER_UNDEFINED                         0x8219
;        INVALID_FRAMEBUFFER_OPERATION   0x0506
;        DEPTH_STENCIL                              0x84F9
;        UNSIGNED_INT_24_8                          0x84FA
;        DEPTH24_STENCIL8                           0x88F0
;        TEXTURE_STENCIL_SIZE 0x88F1

))
