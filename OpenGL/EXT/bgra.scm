; ===========================================================================
; EXT_bgra
;  Pixel data may be specified in BGR or BGRA order, to match the pixel format of Windows bitmaps.
;
;  https://www.opengl.org/registry/specs/EXT/bgra.txt
;
; Version
;  Microsoft revision 1.0, May 19, 1997 (drewb)
;  $Date: 1999/04/03 08:40:34 $ $Revision: 1.4 $
;
; Overview
;  EXT_bgra extends the list of host-memory color formats.
;  Specifically, it provides formats which match the memory layout of
;  Windows DIBs so that applications can use the same data in both
;  Windows API calls and OpenGL pixel API calls.
(define-library (OpenGL EXT bgra)

; ---------------------------------------------------------------------------
; Dependencies
;  None
   (import
      (OpenGL version-1-0))

; ---------------------------------------------------------------------------
   (export
    EXT_bgra

; ---------------------------------------------------------------------------
; New Procedures and Functions
;  None

; ---------------------------------------------------------------------------
; New Tokens

;  Accepted by the <format> parameter of DrawPixels, GetTexImage,
;  ReadPixels, TexImage1D, and TexImage2D:
   GL_BGR
   GL_BGRA

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_bgra (gl:ExtensionSupported? "GL_EXT_bgra"))

   (define GL_BGR                 #x80E0)
   (define GL_BGRA                #x80E1)

))
