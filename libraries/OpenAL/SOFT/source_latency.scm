(define-library (OpenAL SOFT source_latency)

(import (scheme core)
   (OpenAL platform))

(export AL_SOFT_source_latency

   ALint64
   ALuint64

   alSourced
   alSource3d
   alSourcedv
   alGetSourced
   alGetSource3d
   alGetSourcedv
   alSourcei64
   alSource3i64
   alSourcei64v
   alGetSourcei64
   alGetSource3i64
   alGetSourcei64v

   AL_SAMPLE_OFFSET_LATENCY
   AL_SEC_OFFSET_LATENCY
)

; ---------------------------------------------------------------------------
(begin
   (define AL_SOFT_source_latency (al:QueryExtension "AL_SOFT_source_latency"))

   (define ALint64 fft-int64)
   (define ALuint64 fft-uint64)

   (setq ALint64* (fft* ALint64))
   (setq ALint64& (fft& ALint64))
   (setq ALdouble* (fft* ALdouble))
   (setq ALdouble& (fft& ALdouble))

   (setq AL al:GetProcAddress)
   (define alSourced (AL ALvoid "alSourcedSOFT" ALuint ALenum ALdouble))
   (define alSource3d (AL ALvoid "alSource3dSOFT" ALuint ALenum ALdouble ALdouble ALdouble))
   (define alSourcedv (AL ALvoid "alSourcedvSOFT" ALuint ALenum ALdouble*))
   (define alGetSourced (AL ALvoid "alGetSourcedSOFT" ALuint ALenum ALdouble&))
   (define alGetSource3d (AL ALvoid "alGetSource3dSOFT" ALuint ALenum ALdouble& ALdouble& ALdouble&))
   (define alGetSourcedv (AL ALvoid "alGetSourcedvSOFT" ALuint ALenum ALdouble&))
   (define alSourcei64 (AL ALvoid "alSourcei64SOFT" ALuint ALenum ALint64))
   (define alSource3i64 (AL ALvoid "alSource3i64SOFT" ALuint ALenum ALint64 ALint64 ALint64))
   (define alSourcei64v (AL ALvoid "alSourcei64vSOFT" ALuint ALenum ALint64*))
   (define alGetSourcei64 (AL ALvoid "alGetSourcei64SOFT" ALuint ALenum ALint64&))
   (define alGetSource3i64 (AL ALvoid "alGetSource3i64SOFT" ALuint ALenum ALint64& ALint64& ALint64&))
   (define alGetSourcei64v (AL ALvoid "alGetSourcei64vSOFT" ALuint ALenum ALint64&))

   (define AL_SAMPLE_OFFSET_LATENCY             #x1200)
   (define AL_SEC_OFFSET_LATENCY                #x1201)
))
