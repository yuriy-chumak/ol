; OpenAL
;
; Version 1.1
; Published June 2005 (by authors)
;
; Version 1.0 Draft Edition
; Published June 2000 (by Loki Software)

(define-library (OpenAL 1.1)
(export
      (exports (OpenAL platform))

   AL_VERSION_1_0
   AL_VERSION_1_1

   AL_LIBRARY  ; * internal

      alGetError

      alGetString
         AL_VENDOR
         AL_VERSION
         AL_RENDERER
         AL_EXTENSIONS

      alGenSources
      alGenBuffers

      alDeleteSources
      alDeleteBuffers

      alListenerf
      alListener3f
      alListenerfv
      alListeneri
      alListener3i
      alListeneriv

      alSourcef
      alSource3f
      alSourcefv
      alSourcei
      alSource3i
      alSourceiv

      alSourcei
         AL_BUFFER AL_LOOPING

      alGetSourcei
         AL_BUFFERS_QUEUED AL_BUFFERS_PROCESSED

      alBufferData
         AL_FORMAT_MONO8 AL_FORMAT_MONO16
         AL_FORMAT_STEREO8 AL_FORMAT_STEREO16

      alSourcePlay
      alSourceStop
      alSourceQueueBuffers
      alSourceUnqueueBuffers

      ; constants
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
      AL_CHANNEL_MASK

      AL_SOURCE_STATE
      AL_INITIAL
      AL_PLAYING
      AL_PAUSED
      AL_STOPPED

      AL_BUFFERS_QUEUED
      AL_BUFFERS_PROCESSED

      AL_SEC_OFFSET
      AL_SAMPLE_OFFSET
      AL_BYTE_OFFSET

      AL_SOURCE_TYPE
      AL_STATIC
      AL_STREAMING
      AL_UNDETERMINED

      ;AL_FORMAT_MONO8
      ;AL_FORMAT_MONO16
      ;AL_FORMAT_STEREO8
      ;AL_FORMAT_STEREO16

      AL_REFERENCE_DISTANCE
      AL_ROLLOFF_FACTOR
      AL_CONE_OUTER_GAIN
      AL_CONE_OUTER_GAIN

      AL_FREQUENCY
      AL_BITS
      AL_CHANNELS
      AL_SIZE

      AL_UNUSED
      AL_PENDING
      AL_PROCESSED

      AL_NO_ERROR
      AL_INVALID_NAME
      AL_ILLEGAL_ENUM
      AL_INVALID_ENUM
      AL_INVALID_VALUE
      AL_ILLEGAL_COMMAND
      AL_INVALID_OPERATION
      AL_OUT_OF_MEMORY

      ;AL_VENDOR
      ;AL_VERSION
      ;AL_RENDERER
      ;AL_EXTENSIONS

      AL_DOPPLER_FACTOR
      AL_DOPPLER_VELOCITY
      AL_SPEED_OF_SOUND
   
      AL_DISTANCE_MODEL
      AL_INVERSE_DISTANCE
      AL_INVERSE_DISTANCE_CLAMPED
      AL_LINEAR_DISTANCE
      AL_LINEAR_DISTANCE_CLAMPED
      AL_EXPONENT_DISTANCE
      AL_EXPONENT_DISTANCE_CLAMPED

   )
; ============================================================================
; == implementation ==========================================================
(import (scheme core)
        (OpenAL platform))
      ;;  (owl io)
      ;; (owl math) (otus async)
      ;; (owl string)

      ;; (otus ffi)
      ;; (owl list))

(begin
   (setq AL AL_LIBRARY)

   (setq void ALvoid)


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

   (define AL_SEC_OFFSET         #x1024)
   (define AL_SAMPLE_OFFSET      #x1025)
   (define AL_BYTE_OFFSET        #x1026)

   (define AL_SOURCE_TYPE        #x1027)
   (define AL_STATIC             #x1028)
   (define AL_STREAMING          #x1029)
   (define AL_UNDETERMINED       #x1030)

   ;(define AL_FORMAT_MONO8       #x1100)
   ;(define AL_FORMAT_MONO16      #x1101)
   ;(define AL_FORMAT_STEREO8     #x1102)
   ;(define AL_FORMAT_STEREO16    #x1103)

   (define AL_REFERENCE_DISTANCE #x1020)
   (define AL_ROLLOFF_FACTOR     #x1021)
   (define AL_CONE_OUTER_GAIN    #x1022)
   (define AL_CONE_OUTER_GAIN    #x1022)

   (define AL_FREQUENCY          #x2001)
   (define AL_BITS               #x2002)
   (define AL_CHANNELS           #x2003)
   (define AL_SIZE               #x2004)

   (define AL_UNUSED             #x2010)
   (define AL_PENDING            #x2011)
   (define AL_PROCESSED          #x2012)

   (define AL_NO_ERROR         AL_FALSE)
   (define AL_INVALID_NAME       #xA001)
   (define AL_ILLEGAL_ENUM       #xA002)
   (define AL_INVALID_ENUM       #xA002)
   (define AL_INVALID_VALUE      #xA003)
   (define AL_ILLEGAL_COMMAND    #xA004)
   (define AL_INVALID_OPERATION  #xA004)
   (define AL_OUT_OF_MEMORY      #xA005)

   ;(define AL_VENDOR             #xB001)
   ;(define AL_VERSION            #xB002)
   ;(define AL_RENDERER           #xB003)
   ;(define AL_EXTENSIONS         #xB004)

   (define AL_DOPPLER_FACTOR     #xC000)
   (define AL_DOPPLER_VELOCITY   #xC001)
   (define AL_SPEED_OF_SOUND     #xC003)
   
   (define AL_DISTANCE_MODEL            #xD000)
   (define AL_INVERSE_DISTANCE          #xD001)
   (define AL_INVERSE_DISTANCE_CLAMPED  #xD002)
   (define AL_LINEAR_DISTANCE           #xD003)
   (define AL_LINEAR_DISTANCE_CLAMPED   #xD004)
   (define AL_EXPONENT_DISTANCE         #xD005)
   (define AL_EXPONENT_DISTANCE_CLAMPED #xD006)


   ; ===================================================

   ; Renderer State management
   ; alEnable
   ; alDisable
   ; alIsEnabled

   ; State retrieval
   (define alGetString (AL ALchar* "alGetString" ALenum))
      (define AL_VENDOR      #xB001)
      (define AL_VERSION     #xB002)
      (define AL_RENDERER    #xB003)
      (define AL_EXTENSIONS  #xB004)
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
   (define alGetError   (AL ALenum "alGetError"))

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
      ; AL_SEC_OFFSET         f, fv, i, iv
      ; AL_SAMPLE_OFFSET      f, fv, i, iv
      ; AL_BYTE_OFFSET        f, fv, i, iv

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
      ; format:
      (define AL_FORMAT_MONO8    #x1100)
      (define AL_FORMAT_MONO16   #x1101)
      (define AL_FORMAT_STEREO8  #x1102)
      (define AL_FORMAT_STEREO16 #x1103)

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

   ; Global Parameters
   ;alDopplerFactor
   ;alDopplerVelocity
   ;alSpeedOfSound
   ;alDistanceModel


     (define AL_BUFFER      #x1009)

   ; ============================




   ; Context Management


))