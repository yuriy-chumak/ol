; OpenAL
;
; Version 1.1
; Published June 2005 (by authors)
;
; Version 1.0 Draft Edition
; Published June 2000 (by Loki Software)

(define-library (OpenAL version-1-1)
   (export
    AL_VERSION_1_0
    AL_VERSION_1_1

      AL_LIBRARY  ;internal variable

      ; AL base types
      ;cl_char     ;   signed  8-bit
      ;cl_uchar    ; unsigned  8-bit
      ;cl_short    ;   signed 16-bit
      ;cl_ushort   ; unsigned 16-bit
      ;cl_int      ;   signed 32-bit
      ;cl_uint     ; unsigned 32-bit
      ;cl_long     ;   signed 64-bit
      ;cl_ulong    ; unsigned 64-bit
      ;
      ;cl_half     ; unsigned 16-bit
      ;cl_float    ; floating 32-bit
      ;cl_double   ; floating 64-bit

      alGetError

      alcOpenDevice
      alcCreateContext
      alcMakeContextCurrent

      alGenSources
      alGenBuffers

      alSourcei
      alBufferData

      alSourcePlay

      ; Source:
      AL_BUFFER AL_LOOPING

   )
; ============================================================================
; == implementation ==========================================================
   (import
      (r5rs core) (owl math) (owl io) (owl string)
      (otus pinvoke)
      (owl interop) (owl list))

(begin
   (define AL_VERSION_1_0 1)
   (define AL_VERSION_1_1 1)

;   (define type-int64 44)

   (define ALboolean type-fix+)
   (define ALchar    type-fix+)
   (define ALbyte    type-fix+)
   (define ALubyte   type-fix+) ;unsigned
   (define ALshort   type-fix+)
   (define ALushort  type-fix+) ;unsigned
   (define ALint     type-int+) ; signed 32-bit 2's complement integer
   (define ALuint    type-int+)   (define ALuint*  type-vector-raw)
   (define ALsizei   type-int+) ; non-negative 32-bit binary integer size
   (define ALenum    type-int+) ; enumerated 32-bit value
   (define ALfloat   type-float)  ; 32-bit IEEE754 floating-point
   (define ALdouble  type-double) ; 64-bit IEEE754 floating-point

   (define ALvoid    type-void)
   (define ALvoid*   type-vptr)


   ; https://en.wikipedia.org/wiki/Uname
   (define uname (syscall 63 #f #f #f))

   (define AL_LIBRARY (c-string
      (cond
         ((string-ci=? (ref uname 1) "Windows") "openal32")
         ((string-ci=? (ref uname 1) "Linux")   "libAL.so")
         ;"HP-UX"
         ;"SunOS"
         ;"Darwin"
         ;"FreeBSD"
         ;"CYGWIN_NT-5.2-WOW64"
         ;"MINGW32_NT-5.2"
         ;...
         (else
            (runtime-error "Unknown platform" uname)))))

   (define $ (or
      (dlopen AL_LIBRARY)
      (runtime-error "Can't load OpenAL library")))

   ; ======================================================
   (define AL_NONE 0)
   (define AL_FALSE 0)
   (define AL_TRUE 1)

   ; AL_SOURCE_RELATIVE
   ; ...
   (define AL_LOOPING #x1007)
   ; ...
   ;

   ; ...
   (define AU_ULAW_8 1)   ; 8-bit ISDN u-law
   (define AU_PCM_8  2)   ; 8-bit linear PCM (signed)
   (define AU_PCM_16 3)   ; 16-bit linear PCM (signed, big-endian)
   (define AU_PCM_24 4)   ; 24-bit linear PCM
   (define AU_PCM_32 5)   ; 32-bit linear PCM
   (define AU_FLOAT_32 6) ; 32-bit IEEE floating point
   (define AU_FLOAT_64 7) ; 64-bit IEEE floating point
   (define AU_ALAW_8 27)  ; 8-bit ISDN a-law


   ; ===================================================

   ; Renderer State management
   ; alEnable
   ; alDisable
   ; alIsEnabled

   ; State retrieval
   ; alGetString
   ; alGetBooleanv
   ; alGetIntegerv
   ; alGetFloatv
   ; alGetDoublev
   ; alGetBoolean
   ; alGetInteger
   ; alGetFloat
   ; alGetDouble

   ; Error support.
   ; Obtain the most recent error generated in the AL state machine.
   (define alGetError   (dlsym $ ALenum "alGetError"))

   ; Extension support.
   ; Query for the presence of an extension, and obtain any appropriate
   ; function pointers and enum values.
   ; alIsExtensionPresent
   ; alGetProcAddress
   ; alGetEnumValue

   ; * LISTENER
   ; * Listener represents the location and orientation of the
   ; * 'user' in 3D-space.
   ; *
   ; * Properties include: -
   ; *
   ; * Gain         AL_GAIN         ALfloat
   ; * Position     AL_POSITION     ALfloat[3]
   ; * Velocity     AL_VELOCITY     ALfloat[3]
   ; * Orientation  AL_ORIENTATION  ALfloat[6] (Forward then Up vectors)
   ; alListenerf
   ; alListener3f
   ; alListenerfv
   ; alListeneri
   ; alListener3i
   ; alListeneriv
   
   ; alGetListenerf
   ; alGetListener3f
   ; alGetListenerfv
   ; alGetListeneri
   ; alGetListener3i
   ; alGetListeneriv

   ; * SOURCE
   ; * Sources represent individual sound objects in 3D-space.
   ; * Sources take the PCM data provided in the specified Buffer,
   ; * apply Source-specific modifications, and then
   ; * submit them to be mixed according to spatial arrangement etc.
   ; * 
   ; * Properties include: -
   ; *
   ; * Gain                              AL_GAIN                 ALfloat
   ; * Min Gain                          AL_MIN_GAIN             ALfloat
   ; * Max Gain                          AL_MAX_GAIN             ALfloat
   ; * Position                          AL_POSITION             ALfloat[3]
   ; * Velocity                          AL_VELOCITY             ALfloat[3]
   ; * Direction                         AL_DIRECTION            ALfloat[3]
   ; * Head Relative Mode                AL_SOURCE_RELATIVE      ALint (AL_TRUE or AL_FALSE)
   ; * Reference Distance                AL_REFERENCE_DISTANCE   ALfloat
   ; * Max Distance                      AL_MAX_DISTANCE         ALfloat
   ; * RollOff Factor                    AL_ROLLOFF_FACTOR       ALfloat
   ; * Inner Angle                       AL_CONE_INNER_ANGLE     ALint or ALfloat
   ; * Outer Angle                       AL_CONE_OUTER_ANGLE     ALint or ALfloat
   ; * Cone Outer Gain                   AL_CONE_OUTER_GAIN      ALint or ALfloat
   ; * Pitch                             AL_PITCH                ALfloat
   ; * Looping                           AL_LOOPING              ALint (AL_TRUE or AL_FALSE)
   ; * MS Offset                         AL_MSEC_OFFSET          ALint or ALfloat
   ; * Byte Offset                       AL_BYTE_OFFSET          ALint or ALfloat
   ; * Sample Offset                     AL_SAMPLE_OFFSET        ALint or ALfloat
   ; * Attached Buffer                   AL_BUFFER               ALint
   ; * State (Query only)                AL_SOURCE_STATE         ALint
   ; * Buffers Queued (Query only)       AL_BUFFERS_QUEUED       ALint
   ; * Buffers Processed (Query only)    AL_BUFFERS_PROCESSED    ALint

   (define alGenSources (dlsym $ type-void "alGenSources" ALsizei ALuint*))
   (define alDeleteSources (dlsym $ type-void "alDeleteSources" ALsizei ALuint*))
   (define alIsSource (dlsym $ ALboolean "alIsSource" ALuint))

   ;alSourcef
   ;alSource3f
   ;alSourcefv
   (define alSourcei (dlsym $ type-void "alSourcei" type-int+ type-int+ type-int+))
   ;alSource3i
   ;alSourceiv

   ;alGetSourcef
   ;alGetSource3f
   ;alGetSourcefv
   ;alGetSourcei
   ;alGetSource3i
   ;alGetSourceiv

   ; Source vector based playback calls
   ;alSourcePlayv
   ;alSourceStopv
   ;alSourceRewindv
   ;alSourcePausev

   ; Source based playback calls
   (define alSourcePlay (dlsym $ type-void "alSourcePlay" ALuint))
   ;alSourceStop
   ;alSourceRewind
   ;alSourcePause

   ; Source Queuing
   ;alSourceQueueBuffers
   ;alSourceUnqueueBuffers


   ; * BUFFER
   ; * Buffer objects are storage space for sample data.
   ; * Buffers are referred to by Sources. One Buffer can be used
   ; * by multiple Sources.
   ; *
   ; * Properties include: -
   ; *
   ; * Frequency (Query only)    AL_FREQUENCY      ALint
   ; * Size (Query only)         AL_SIZE           ALint
   ; * Bits (Query only)         AL_BITS           ALint
   ; * Channels (Query only)     AL_CHANNELS       ALint

   (define alGenBuffers (dlsym $ type-void "alGenBuffers" ALsizei ALuint*))
   ;alDeleteBuffers
   ;alIsBuffer
   (define alBufferData (dlsym $ type-void "alBufferData"
            ALuint  #|bid|#
            ALenum  #|format|#
            ALvoid* #|data|#
            ALsizei #|size|#
            ALsizei #|freq|#))

   ;alBufferf
   ;alBuffer3f
   ;alBufferfv
   ;alBufferi
   ;alBuffer3i
   ;alBufferiv

   ;alGetBufferf
   ;alGetBuffer3f
   ;alGetBufferfv
   ;alGetBufferi
   ;alGetBuffer3i
   ;alGetBufferiv

   ; Global Parameters
   ;alDopplerFactor
   ;alDopplerVelocity
   ;alSpeedOfSound
   ;alDistanceModel


     (define AL_BUFFER      #x1009)

   ; ============================
   (define ALCdevice* type-vptr)
   (define ALCcontext* type-vptr)

   (define ALCboolean type-fix+)
   (define ALCchar    type-fix+)   (define ALCchar* type-string)
   (define ALCbyte    type-fix+)
   (define ALCubyte   type-fix+)
   (define ALCshort   type-fix+)
   (define ALCushort  type-fix+)
   (define ALCint     type-int+)   (define ALCint*  type-vector-raw) ; type-vector-raw or (bor ALCint #x40)?
   (define ALCuint    type-int+)   (define ALCuint*  type-vector-raw)
   (define ALCsizei   type-int+)
   (define ALCenum    type-int+)
   (define ALCfloat   type-float)
   (define ALCdouble  type-double)

   (define ALCvoid    type-void)
   (define ALCvoid*   type-vptr)


   (define ALC_FALSE 0)
   (define ALC_TRUE  1)


   ; Context Management

   (define alcCreateContext (dlsym $ ALCcontext* "alcCreateContext" ALCdevice* ALCint*))
   (define alcMakeContextCurrent (dlsym $ type-int+ "alcMakeContextCurrent" ALCcontext*))
   ;alcProcessContext
   ;alcSuspendContext
   ;alcDestroyContext
   ;alcGetCurrentContext
   ;alcGetContextsDevice


   ; Device Management

   (define alcOpenDevice (dlsym $ ALCdevice* "alcOpenDevice" ALCchar*))
   ;alcCloseDevice

   (define alcGetError (dlsym $ ALCenum "alcGetError" ALCdevice*))

   ; * Extension support.
   ; * Query for the presence of an extension, and obtain any appropriate
   ; * function pointers and enum values.

   ;alcIsExtensionPresent
   ;alcGetProcAddress
   ;alcGetEnumValue
   
   ; Query functions
   ;alcGetString
   ;alcGetIntegerv

   ; Capture functions
   ;alcCaptureOpenDevice
   ;alcCaptureCloseDevice
   ;alcCaptureStart
   ;alcCaptureStop
   ;alcCaptureSamples

))