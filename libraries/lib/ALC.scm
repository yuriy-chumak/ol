(define-library (lib ALC)
(export
   ALC_LIBRARY

   ALC_CAPTURE_SAMPLES

   alcOpenDevice
   alcCloseDevice

   alcCreateContext
   alcMakeContextCurrent
   alcDestroyContext

   alcGetError
   alcCaptureOpenDevice
   alcCaptureCloseDevice
   alcCaptureStart
   alcCaptureStop
   alcCaptureSamples

   alcGetIntegerv
)

; ============================================================================
; == implementation ==========================================================
(import (scheme core)
        (OpenAL platform))

(begin
   (define ALC_LIBRARY AL_LIBRARY))

(begin

   (define ALCboolean fft-char)
   (define ALCchar    fft-char)
      (define ALCchar* type-string)
   (define ALCbyte    fft-char)
   (define ALCubyte   fft-unsigned-char)
   (define ALCshort   fft-short)
   (define ALCushort  fft-unsigned-short)
   (define ALCint     fft-int)
      (define ALCint*  (fft* ALCint))
      (define ALCint&  (fft& ALCint))
   (define ALCuint    fft-unsigned-int)
      (define ALCuint*  (fft* ALCuint))
      (define ALCuint&  (fft& ALCuint))
   (define ALCsizei   fft-int)
   (define ALCenum    fft-int)
   (define ALCfloat   fft-float)
   (define ALCdouble  fft-double)

   (define ALCvoid    fft-void)
   (define ALCvoid*   type-vptr)

   (define ALCdevice* type-vptr)
   (define ALCcontext* type-vptr)

   ; ----------------------------------
   (setq ALC ALC_LIBRARY)

   (define ALC_FALSE 0)
   (define ALC_TRUE  1)

   (define ALC_CAPTURE_SAMPLES #x312)


   (define alcCreateContext (ALC ALCcontext* "alcCreateContext" ALCdevice* ALCint*))
   (define alcMakeContextCurrent (ALC type-int+ "alcMakeContextCurrent" ALCcontext*))
   ;alcProcessContext
   ;alcSuspendContext
   (define alcDestroyContext (ALC ALvoid "alcDestroyContext" ALCcontext*))
   ;alcGetCurrentContext
   ;alcGetContextsDevice


   ; Device Management

   (define alcOpenDevice (ALC ALCdevice* "alcOpenDevice" ALCchar*))
   (define alcCloseDevice (ALC ALCboolean "alcCloseDevice" ALCdevice*))

   (define alcGetError (ALC ALCenum "alcGetError" ALCdevice*))

   ; * Extension support.
   ; * Query for the presence of an extension, and obtain any appropriate
   ; * function pointers and enum values.

   ;alcIsExtensionPresent
   ;alcGetProcAddress
   ;alcGetEnumValue
   
   ; Query functions
   ;alcGetString
   (define alcGetIntegerv (ALC ALvoid "alcGetIntegerv" ALCdevice* ALCenum ALCsizei ALCint&))

   ; Capture functions
   (define alcCaptureOpenDevice (ALC ALCdevice* "alcCaptureOpenDevice" ALCchar* ALCuint ALCenum ALCsizei))
   (define alcCaptureCloseDevice (ALC ALCboolean "alcCaptureCloseDevice" ALCdevice*))
   (define alcCaptureStart (ALC ALvoid "alcCaptureStart" ALCdevice*))
   (define alcCaptureStop (ALC ALvoid "alcCaptureStop" ALCdevice*))
   (define alcCaptureSamples (ALC ALvoid "alcCaptureSamples" ALCdevice*))

))
