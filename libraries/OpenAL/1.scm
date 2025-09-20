; OpenAL 1.0 Draft Edition (Jun 2000)

(define-library (OpenAL 1.0)
(export
      (exports (OpenAL platform))

   AL_VERSION_1_0
   ALC_VERSION_0_1

      AL_LIBRARY  ; * internal
      AL_NONE AL_FALSE AL_TRUE

      alGetError
         AL_NO_ERROR
         AL_INVALID_NAME
         AL_INVALID_ENUM AL_ILLEGAL_ENUM
         AL_INVALID_VALUE
         AL_INVALID_OPERATION AL_ILLEGAL_COMMAND
         AL_OUT_OF_MEMORY

      alIsExtensionPresent
      ;alGetProcAddress
      ;alGetEnumValue

      ; Constants
      AL_SOURCE_RELATIVE
      AL_CONE_INNER_ANGLE
      AL_CONE_OUTER_ANGLE
      AL_PITCH
      AL_POSITION
      AL_DIRECTION
      AL_VELOCITY
      AL_LOOPING
      AL_BUFFER
      AL_GAIN
      AL_MIN_GAIN
      AL_MAX_GAIN
      AL_ORIENTATION
      ;AL_CHANNEL_MASK

      AL_SOURCE_STATE
      AL_INITIAL
      AL_PLAYING
      AL_PAUSED
      AL_STOPPED

      AL_BUFFERS_QUEUED
      AL_BUFFERS_PROCESSED

      AL_REFERENCE_DISTANCE
      AL_ROLLOFF_FACTOR
      AL_CONE_OUTER_GAIN

      AL_MAX_DISTANCE

      AL_SOURCE_TYPE
      AL_STATIC
      AL_STREAMING
      AL_UNDETERMINED

      AL_FREQUENCY
      AL_BITS
      AL_CHANNELS
      AL_SIZE

      AL_UNUSED
      AL_PENDING
      AL_PROCESSED

      AL_VENDOR
      AL_VERSION
      AL_RENDERER
      AL_EXTENSIONS

      AL_DOPPLER_FACTOR
      AL_DOPPLER_VELOCITY

      AL_DISTANCE_MODEL
      AL_INVERSE_DISTANCE
      AL_INVERSE_DISTANCE_CLAMPED

      ; 2.6. Controlling AL Execution
      alEnable
      alDisable
      alIsEnabled

      ; 3.1. Querying AL State
      alGetString  ; (OpenAL platform)
         AL_VENDOR
         AL_VERSION
         AL_RENDERER
         AL_EXTENSIONS
      ;alGetBooleanv
      ;alGetIntegerv
      ;alGetFloatv
      ;alGetDoublev

      ; 3.4. Attenuation By Distance
      ;alDistanceModel

      ; 3.7. Velocity Dependent Doppler Effect
      ;alDopplerFactor
      ;alDopplerVelocity

      ; Chapter 4. Listener and Sources
      alListenerf
      alListener3f
      alListenerfv
      alListeneri
      alListener3i
      alListeneriv

      ;alGetListenerf
      ;alGetListener3f
      ;alGetListenerfv
      ;alGetListeneri
      ;alGetListener3i
      ;alGetListeneriv

      alGenSources
      alDeleteSources
      alIsSource

      alSourcef
      alSource3f
      alSourcefv
      alSourcei
      ;;    AL_BUFFER AL_LOOPING
      alSource3i
      alSourceiv

      ;alGetSourcef
      ;alGetSource3f
      ;alGetSourcefv
      alGetSourcei
      ;;    AL_BUFFERS_QUEUED AL_BUFFERS_PROCESSED
      ;alGetSource3i
      ;alGetSourceiv

      alSourcePlay
      alSourceStop
      ;alSourceRewind
      ;alSourcePause
      ;alSourcePlayv
      ;alSourceStopv
      ;alSourceRewindv
      ;alSourcePausev
      alSourceQueueBuffers
      alSourceUnqueueBuffers

      ; Chapter 5. Buffers
      alGenBuffers
      alDeleteBuffers
      ;alIsBuffer
      alBufferData
         AL_FORMAT_MONO8 AL_FORMAT_MONO16
         AL_FORMAT_STEREO8 AL_FORMAT_STEREO16

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

      ; -- ALC -------------------
      ; Constants
      ALC_LIBRARY  ; * internal
      ALC_FALSE ALC_TRUE

      ALC_EXTENSIONS

      ALC_FREQUENCY
      ALC_REFRESH
      ALC_SYNC

      ALC_NO_ERROR
      ALC_INVALID_DEVICE
      ALC_INVALID_CONTEXT
      ALC_INVALID_ENUM
      ALC_INVALID_VALUE
      ALC_OUT_OF_MEMORY

      ALC_MAJOR_VERSION
      ALC_MINOR_VERSION

      ALC_ATTRIBUTES_SIZE
      ALC_ALL_ATTRIBUTES

      ALC_DEFAULT_DEVICE_SPECIFIER
      ALC_DEVICE_SPECIFIER
      ALC_EXTENSIONS

      ; Chapter 6. AL Contexts and the ALC API
      alcOpenDevice
      alcCloseDevice

      alcCreateContext
      alcMakeContextCurrent
      alcDestroyContext

      alcGetCurrentContext
      alcGetContextsDevice

      alcIsExtensionPresent
      ;alcGetProcAddress
      ;alcGetEnumValue
      alcGetError

      alcGetString
      alcGetIntegerv
   )

; ============================================================================
; == implementation ==========================================================
(import (scheme core)
        (OpenAL platform))

(begin
   (setq AL AL_LIBRARY)

   (setq void ALvoid)
   (setq ALchar* type-string)

   (define AL_VERSION_1_0 1)
   (define AL_VERSION_1_1 1)

   (define AL_INVALID -1)
   (define AL_NONE 0)
   (define AL_FALSE 0)
   (define AL_TRUE 1)

   (define AL_SOURCE_RELATIVE    #x0202)
   (define AL_CONE_INNER_ANGLE   #x1001) ; in degrees
   (define AL_CONE_OUTER_ANGLE   #x1002) ; in degrees
   (define AL_PITCH              #x1003)
   (define AL_POSITION           #x1004)
   (define AL_DIRECTION          #x1005)
   (define AL_VELOCITY           #x1006)
   (define AL_LOOPING            #x1007)
   (define AL_BUFFER             #x1009)
   (define AL_GAIN               #x100A)
   (define AL_MIN_GAIN           #x100D)
   (define AL_MAX_GAIN           #x100E)
   (define AL_ORIENTATION        #x100F)
   (define AL_CHANNEL_MASK       #x3000)

   (define AL_SOURCE_STATE       #x1010)
   (define AL_INITIAL            #x1011)
   (define AL_PLAYING            #x1012)
   (define AL_PAUSED             #x1013)
   (define AL_STOPPED            #x1014)

   (define AL_BUFFERS_QUEUED     #x1015)
   (define AL_BUFFERS_PROCESSED  #x1016)

   (define AL_SOURCE_TYPE        #x1027)
   (define AL_STATIC             #x1028)
   (define AL_STREAMING          #x1029)
   (define AL_UNDETERMINED       #x1030)

   (define AL_FORMAT_MONO8       #x1100)
   (define AL_FORMAT_MONO16      #x1101)
   (define AL_FORMAT_STEREO8     #x1102)
   (define AL_FORMAT_STEREO16    #x1103)

   (define AL_REFERENCE_DISTANCE #x1020)
   (define AL_ROLLOFF_FACTOR     #x1021)
   (define AL_CONE_OUTER_GAIN    #x1022)
   (define AL_MAX_DISTANCE       #x1023)

   (define AL_FREQUENCY          #x2001)
   (define AL_BITS               #x2002)
   (define AL_CHANNELS           #x2003)
   (define AL_SIZE               #x2004)

   (define AL_UNUSED             #x2010)
   (define AL_PENDING            #x2011)
   (define AL_PROCESSED          #x2012)

   (define AL_NO_ERROR         AL_FALSE)
   (define AL_INVALID_NAME       #xA001)
   (define AL_INVALID_ENUM       #xA002)
   (define AL_ILLEGAL_ENUM       AL_INVALID_ENUM)
   (define AL_INVALID_VALUE      #xA003)
   (define AL_INVALID_OPERATION  #xA004)
   (define AL_ILLEGAL_COMMAND    AL_INVALID_OPERATION)
   (define AL_OUT_OF_MEMORY      #xA005)

   ;(define AL_VENDOR             #xB001)
   ;(define AL_VERSION            #xB002)
   ;(define AL_RENDERER           #xB003)
   ;(define AL_EXTENSIONS         #xB004)

   (define AL_DOPPLER_FACTOR     #xC000)
   (define AL_DOPPLER_VELOCITY   #xC001)

   (define AL_DISTANCE_MODEL            #xD000)
   (define AL_INVERSE_DISTANCE          #xD001)
   (define AL_INVERSE_DISTANCE_CLAMPED  #xD002)

   ; ===================================================
   (define alGetError   (AL ALenum "alGetError"))
   (define alIsExtensionPresent (AL ALboolean "alIsExtensionPresent" ALchar*))
   ; alGetProcAddress
   (define alGetEnumValue (AL ALenum "alGetEnumValue" ALchar*))

   ; Renderer State management
   (define alEnable (AL void "alEnable" ALenum))
   (define alDisable (AL void "alDisable" ALenum))
   (define alIsEnabled (AL ALboolean "alIsEnabled" ALenum))

   alGetString
      (define AL_VENDOR      #xB001)
      (define AL_VERSION     #xB002)
      (define AL_RENDERER    #xB003)
      (define AL_EXTENSIONS  #xB004)
   ; alGetBooleanv
   ; alGetIntegerv
   ; alGetFloatv
   ; alGetDoublev

   ; alDistanceModel
   ; alDopplerFactor
   ; alDopplerVelocity

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
   (define alListenerf (AL void "alListenerf" ALenum ALfloat))
   (define alListener3f (AL void "alListener3f" ALenum ALfloat ALfloat ALfloat))
   (define alListenerfv (AL void "alListenerfv" ALenum ALfloat&))
   (define alListeneri (AL void "alListeneri" ALenum ALint))
   (define alListener3i (AL void "alListener3i" ALenum ALint ALint ALint))
   (define alListeneriv (AL void "alListeneriv" ALenum ALint&))
   
   ; alGetListenerf
   ; alGetListener3f
   ; alGetListenerfv
   ; alGetListeneri
   ; alGetListener3i
   ; alGetListeneriv
      ; AL_GAIN               f, fv
      ; AL_POSITION           fv, 3f, iv, 3i
      ; AL_VELOCITY           fv, 3f, iv, 3i
      ; AL_ORIENTATION        fv, iv

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

   (define alGenSources (AL fft-void "alGenSources" ALsizei ALuint&))
   (define alDeleteSources (AL fft-void "alDeleteSources" ALsizei ALuint*))
   (define alIsSource (AL ALboolean "alIsSource" ALuint))

   (define alSourcef (AL void "alSourcef" ALuint ALenum ALfloat))
   (define alSource3f (AL void "alSource3f" ALuint ALenum ALfloat ALfloat ALfloat))
   (define alSourcefv (AL void "alSourcefv" ALuint ALenum ALfloat&))
   (define alSourcei (AL void "alSourcei" ALuint ALenum ALint))
   (define alSource3i (AL void "alSource3i" ALuint ALenum ALint ALint ALint))
   (define alSourceiv (AL void "alSourceiv" ALuint ALenum ALint&))

   ;alGetSourcef
   ;alGetSource3f
   ;alGetSourcefv
   (define alGetSourcei (AL fft-void "alGetSourcei" ALuint ALenum ALint&))
   ;alGetSource3i
   ;alGetSourceiv

      ; AL_PITCH              f, fv
      ; AL_GAIN               f, fv
      ; AL_MAX_DISTANCE       f, fv, i, iv
      ; AL_ROLLOFF_FACTOR     f, fv, i, iv
      ; AL_REFERENCE_DISTANCE f, fv, i, iv
      ; AL_MIN_GAIN           f, fv
      ; AL_MAX_GAIN           f, fv
      ; AL_CONE_OUTER_GAIN    f, fv
      ; AL_CONE_INNER_ANGLE   f, fv, i, iv
      ; AL_CONE_OUTER_ANGLE   f, fv, i, iv
      ; AL_POSITION           fv, 3f
      ; AL_VELOCITY           fv, 3f
      ; AL_DIRECTION          fv, 3f, iv, 3i
      ; AL_SOURCE_RELATIVE    i, iv
      ; AL_SOURCE_TYPE        i, iv
      ; AL_LOOPING            i, iv
      ; AL_BUFFER             i, iv
      ; AL_SOURCE_STATE       i, iv
      ; AL_BUFFERS_QUEUED     i, iv
      ; AL_BUFFERS_PROCESSED  i, iv

   ; Source vector based playback calls
   ;alSourcePlayv
   ;alSourceStopv
   ;alSourceRewindv
   ;alSourcePausev

   ; Source based playback calls
   (define alSourcePlay (AL fft-void "alSourcePlay" ALuint))
   (define alSourceStop (AL fft-void "alSourceStop" ALuint))
   ;alSourceRewind
   ;alSourcePause

   ; Source Queuing
   (define alSourceQueueBuffers (AL fft-void "alSourceQueueBuffers" ALuint ALsizei ALuint*))
   (define alSourceUnqueueBuffers (AL fft-void "alSourceUnqueueBuffers" ALuint ALsizei ALuint&))

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

   (define alGenBuffers (AL fft-void "alGenBuffers" ALsizei ALuint&))
   (define alDeleteBuffers (AL void "alDeleteBuffers" ALsizei ALuint*))
   ;alIsBuffer
   (define alBufferData (AL fft-void "alBufferData"
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
      ; AL_FREQUENCY    i, iv
      ; AL_BITS         i, iv
      ; AL_CHANNELS     i, iv
      ; AL_SIZE         i, iv
      ; AL_DATA         i, iv

   ; ============================
   ; -- ALC ------------------
   (setq ALC_LIBRARY AL_LIBRARY)

   (define ALCboolean fft-char)
   (define ALCchar    fft-char)
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
   (setq ALCchar* type-string)

   (define ALC_VERSION_0_1 1)

   (define ALC_FALSE 0)
   (define ALC_TRUE  1)

   (define ALC_FREQUENCY                            #x1007)
   (define ALC_REFRESH                              #x1008)
   (define ALC_SYNC                                 #x1009)

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

   (define ALC_ENUMERATE_ALL_EXT 1)
   (define ALC_DEFAULT_ALL_DEVICES_SPECIFIER        #x1012)
   (define ALC_ALL_DEVICES_SPECIFIER                #x1013)

   (define alcOpenDevice (ALC ALCdevice* "alcOpenDevice" ALCchar*))
   (define alcCloseDevice (ALC ALvoid "alcCloseDevice" ALCdevice*))

   (define alcCreateContext (ALC ALCcontext* "alcCreateContext" ALCdevice* ALCint*))
   (define alcMakeContextCurrent (ALC type-integer+ "alcMakeContextCurrent" ALCcontext*))
   (define alcDestroyContext (ALC ALvoid "alcDestroyContext" ALCcontext*))

   (define alcGetCurrentContext (ALC ALCcontext* "alcGetCurrentContext"))
   (define alcGetContextsDevice (ALC ALCdevice* "alcGetContextsDevice" ALCcontext*))

   (define alcIsExtensionPresent (ALC ALCboolean "alcIsExtensionPresent" ALCdevice* ALCchar*))
   ;alcGetProcAddress
   ;alcGetEnumValue
   (define alcGetError (ALC ALCenum "alcGetError" ALCdevice*))

   (define alcGetString (ALC ALchar* "alcGetString"  ALCdevice* ALenum))
   (define alcGetIntegerv (ALC ALvoid "alcGetIntegerv" ALCdevice* ALCenum ALCsizei ALCint&))

))