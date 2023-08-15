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

   alcGetString
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

   (define ALC_VERSION_0_1 1)

   (define ALC_FALSE 0)
   (define ALC_TRUE  1)

   (define ALC_FREQUENCY                            #x1007)
   (define ALC_REFRESH                              #x1008)
   (define ALC_SYNC                                 #x1009)
   (define ALC_MONO_SOURCES                         #x1010)
   (define ALC_STEREO_SOURCES                       #x1011)

   (define ALC_NO_ERROR                             0)
   (define ALC_INVALID_DEVICE                       #xA001)
   (define ALC_INVALID_CONTEXT                      #xA002)
   (define ALC_INVALID_ENUM                         #xA003)
   (define ALC_INVALID_VALUE                        #xA004)
   (define ALC_OUT_OF_MEMORY                        #xA005)

   (define ALC_MAJOR_VERSION                        #x1000)
   (define ALC_MINOR_VERSION                        #x1001)

   (define ALC_ATTRIBUTES_SIZE                      #x1002)
   (define ALC_ALL_ATTRIBUTES                       #x1003)

   (define ALC_DEFAULT_DEVICE_SPECIFIER             #x1004)
   (define ALC_DEVICE_SPECIFIER                     #x1005)
   (define ALC_EXTENSIONS                           #x1006)

   (define ALC_EXT_CAPTURE 1)
   (define ALC_CAPTURE_DEVICE_SPECIFIER             #x310)
   (define ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER     #x311)
   (define ALC_CAPTURE_SAMPLES                      #x312)

   (define ALC_ENUMERATE_ALL_EXT 1)
   (define ALC_DEFAULT_ALL_DEVICES_SPECIFIER        #x1012)
   (define ALC_ALL_DEVICES_SPECIFIER                #x1013)


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
   (define alcGetString (ALC ALchar* "alcGetString"  ALCdevice* ALenum))
   (define alcGetIntegerv (ALC ALvoid "alcGetIntegerv" ALCdevice* ALCenum ALCsizei ALCint&))

   ; Capture functions
   (define alcCaptureOpenDevice (ALC ALCdevice* "alcCaptureOpenDevice" ALCchar* ALCuint ALCenum ALCsizei))
   (define alcCaptureCloseDevice (ALC ALCboolean "alcCaptureCloseDevice" ALCdevice*))
   (define alcCaptureStart (ALC ALvoid "alcCaptureStart" ALCdevice*))
   (define alcCaptureStop (ALC ALvoid "alcCaptureStop" ALCdevice*))
   (define alcCaptureSamples (ALC ALvoid "alcCaptureSamples" ALCdevice*))

))
