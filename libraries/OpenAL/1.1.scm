; OpenAL 1.1 (Jun 2005)

(define-library (OpenAL 1.1)
(export
      (exports (OpenAL 1.0))

   AL_VERSION_1_1

      ; 3.1. Querying OpenAL State
      ;alGetBoolean
      ;alGetInteger
      ;alGetFloat
      ;alGetDouble

      ; 1.4.1. Recording API (ALC_EXT_CAPTURE)
      ALC_CAPTURE_DEVICE_SPECIFIER
      ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER
      ALC_CAPTURE_SAMPLES
      alcCaptureOpenDevice
      alcCaptureCloseDevice
      alcCaptureStart
      alcCaptureStop
      alcCaptureSamples

      ; 1.4.2. Get/Set Offset (AL_EXT_OFFSET)
      AL_SEC_OFFSET
      AL_SAMPLE_OFFSET
      AL_BYTE_OFFSET

      ; 1.4.3. Linear Distance Models (AL_EXT_LINEAR_DISTANCE)
      AL_LINEAR_DISTANCE
      AL_LINEAR_DISTANCE_CLAMPED

      ; 1.4.4. Exponential Distance Models (AL_EXT_EXPONENT_DISTANCE)
      AL_EXPONENT_DISTANCE
      AL_EXPONENT_DISTANCE_CLAMPED

      ; 1.4.5. Doppler
      AL_SPEED_OF_SOUND
      ;alSpeedOfSound

      ; 1.4.6. Mono/Stereo Hints
      ALC_MONO_SOURCES
      ALC_STEREO_SOURCES

      ; 1.4.7. Standard Extensions Listings

      ; 1.4.8. Standard Suspend/Process Behavior
      ;alcProcessContext
      ;alcSuspendContext

      ; 1.4.9. ALUT Revisions

      ; 1.4.10. Streaming Clarifications

      ; 1.4.11. Error Codes

      ; 1.4.12. Pitch Shifting Limits

      ; 1.4.13. New ALchar and ALCchar types

      ; 1.4.14. alcCloseDevice Return Value
      alcCloseDevice

      ; 1.4.15. Versioning Changes
   )
; ============================================================================
; == implementation ==========================================================
(import (scheme core)
        (OpenAL 1.0))

(begin
   (setq ALC ALC_LIBRARY)

   (setq void ALvoid)
   (setq ALCchar* type-string)

   (define AL_VERSION_1_1 1)

   (define ALCboolean fft-char)
   (define ALCuint    fft-unsigned-int)
   (define ALCsizei   fft-int)
   (define ALCenum    fft-int)

   (define ALCdevice* type-vptr)
   (define ALCcontext* type-vptr)

   ; alGetBoolean
   ; alGetInteger
   ; alGetFloat
   ; alGetDouble

   (define ALC_MONO_SOURCES                         #x1010)
   (define ALC_STEREO_SOURCES                       #x1011)

   (define ALC_EXT_CAPTURE 1)
   (define ALC_CAPTURE_DEVICE_SPECIFIER             #x310)
   (define ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER     #x311)
   (define ALC_CAPTURE_SAMPLES                      #x312)

   (define alcCaptureOpenDevice (ALC ALCdevice* "alcCaptureOpenDevice" ALCchar* ALCuint ALCenum ALCsizei))
   (define alcCaptureCloseDevice (ALC ALCboolean "alcCaptureCloseDevice" ALCdevice*))
   (define alcCaptureStart (ALC ALvoid "alcCaptureStart" ALCdevice*))
   (define alcCaptureStop (ALC ALvoid "alcCaptureStop" ALCdevice*))
   (define alcCaptureSamples (ALC ALvoid "alcCaptureSamples" ALCdevice*))

   ; AL_EXT_OFFSET
   (define AL_SEC_OFFSET         #x1024)
   (define AL_SAMPLE_OFFSET      #x1025)
   (define AL_BYTE_OFFSET        #x1026)
      ; AL_SEC_OFFSET         f, fv, i, iv
      ; AL_SAMPLE_OFFSET      f, fv, i, iv
      ; AL_BYTE_OFFSET        f, fv, i, iv

   ; AL_EXT_LINEAR_DISTANCE
   (define AL_LINEAR_DISTANCE           #xD003)
   (define AL_LINEAR_DISTANCE_CLAMPED   #xD004)

   ; AL_EXT_EXPONENT_DISTANCE
   (define AL_EXPONENT_DISTANCE         #xD005)
   (define AL_EXPONENT_DISTANCE_CLAMPED #xD006)

   (define AL_SPEED_OF_SOUND     #xC003)

   ;alcProcessContext
   ;alcSuspendContext

   (define alcCloseDevice (ALC ALCboolean "alcCloseDevice" ALCdevice*))

))