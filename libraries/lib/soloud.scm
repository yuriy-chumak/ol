(define-library (lib soloud)
   (export

      SOLOUD_AUTO
      SOLOUD_SDL1
      SOLOUD_SDL2
      SOLOUD_PORTAUDIO
      SOLOUD_WINMM
      SOLOUD_XAUDIO2
      SOLOUD_WASAPI
      SOLOUD_ALSA
      SOLOUD_JACK
      SOLOUD_OSS
      SOLOUD_OPENAL
      SOLOUD_COREAUDIO
      SOLOUD_OPENSLES
      SOLOUD_VITA_HOMEBREW
      SOLOUD_MINIAUDIO
      SOLOUD_NOSOUND
      SOLOUD_NULLDRIVER
      SOLOUD_BACKEND_MAX
      SOLOUD_CLIP_ROUNDOFF
      SOLOUD_ENABLE_VISUALIZATION
      SOLOUD_LEFT_HANDED_3D
      SOLOUD_NO_FPU_REGISTER_CHANGE
      SOLOUD_WAVE_SQUARE
      SOLOUD_WAVE_SAW
      SOLOUD_WAVE_SIN
      SOLOUD_WAVE_TRIANGLE
      SOLOUD_WAVE_BOUNCE
      SOLOUD_WAVE_JAWS
      SOLOUD_WAVE_HUMPS
      SOLOUD_WAVE_FSQUARE
      SOLOUD_WAVE_FSAW
      SOLOUD_RESAMPLER_POINT
      SOLOUD_RESAMPLER_LINEAR
      SOLOUD_RESAMPLER_CATMULLROM

      ;; void Soloud_destroy(Soloud * aSoloud);
      Soloud_create ;; Soloud * ();
      ;; int Soloud_init(Soloud * aSoloud);
      Soloud_initEx ;; int (Soloud * aSoloud, unsigned int aFlags /* = Soloud::CLIP_ROUNDOFF */, unsigned int aBackend /* = Soloud::AUTO */, unsigned int aSamplerate /* = Soloud::AUTO */, unsigned int aBufferSize /* = Soloud::AUTO */, unsigned int aChannels /* = 2 */);
      ;; void Soloud_deinit(Soloud * aSoloud);
      ;; unsigned int Soloud_getVersion(Soloud * aSoloud);
      ;; const char * Soloud_getErrorString(Soloud * aSoloud, int aErrorCode);
      ;; unsigned int Soloud_getBackendId(Soloud * aSoloud);
      ;; const char * Soloud_getBackendString(Soloud * aSoloud);
      ;; unsigned int Soloud_getBackendChannels(Soloud * aSoloud);
      ;; unsigned int Soloud_getBackendSamplerate(Soloud * aSoloud);
      ;; unsigned int Soloud_getBackendBufferSize(Soloud * aSoloud);
      ;; int Soloud_setSpeakerPosition(Soloud * aSoloud, unsigned int aChannel, float aX, float aY, float aZ);
      ;; int Soloud_getSpeakerPosition(Soloud * aSoloud, unsigned int aChannel, float * aX, float * aY, float * aZ);
      ;; unsigned int Soloud_play(Soloud * aSoloud, AudioSource * aSound);
      ;; unsigned int Soloud_playEx(Soloud * aSoloud, AudioSource * aSound, float aVolume /* = -1.0f */, float aPan /* = 0.0f */, int aPaused /* = 0 */, unsigned int aBus /* = 0 */);
      ;; unsigned int Soloud_playClocked(Soloud * aSoloud, double aSoundTime, AudioSource * aSound);
      ;; unsigned int Soloud_playClockedEx(Soloud * aSoloud, double aSoundTime, AudioSource * aSound, float aVolume /* = -1.0f */, float aPan /* = 0.0f */, unsigned int aBus /* = 0 */);
      ;; unsigned int Soloud_play3d(Soloud * aSoloud, AudioSource * aSound, float aPosX, float aPosY, float aPosZ);
      ;; unsigned int Soloud_play3dEx(Soloud * aSoloud, AudioSource * aSound, float aPosX, float aPosY, float aPosZ, float aVelX /* = 0.0f */, float aVelY /* = 0.0f */, float aVelZ /* = 0.0f */, float aVolume /* = 1.0f */, int aPaused /* = 0 */, unsigned int aBus /* = 0 */);
      ;; unsigned int Soloud_play3dClocked(Soloud * aSoloud, double aSoundTime, AudioSource * aSound, float aPosX, float aPosY, float aPosZ);
      ;; unsigned int Soloud_play3dClockedEx(Soloud * aSoloud, double aSoundTime, AudioSource * aSound, float aPosX, float aPosY, float aPosZ, float aVelX /* = 0.0f */, float aVelY /* = 0.0f */, float aVelZ /* = 0.0f */, float aVolume /* = 1.0f */, unsigned int aBus /* = 0 */);
      ;; unsigned int Soloud_playBackground(Soloud * aSoloud, AudioSource * aSound);
      ;; unsigned int Soloud_playBackgroundEx(Soloud * aSoloud, AudioSource * aSound, float aVolume /* = -1.0f */, int aPaused /* = 0 */, unsigned int aBus /* = 0 */);
      ;; int Soloud_seek(Soloud * aSoloud, unsigned int aVoiceHandle, double aSeconds);
      ;; void Soloud_stop(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; void Soloud_stopAll(Soloud * aSoloud);
      ;; void Soloud_stopAudioSource(Soloud * aSoloud, AudioSource * aSound);
      ;; int Soloud_countAudioSource(Soloud * aSoloud, AudioSource * aSound);
      ;; void Soloud_setFilterParameter(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aFilterId, unsigned int aAttributeId, float aValue);
      ;; float Soloud_getFilterParameter(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aFilterId, unsigned int aAttributeId);
      ;; void Soloud_fadeFilterParameter(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aFilterId, unsigned int aAttributeId, float aTo, double aTime);
      ;; void Soloud_oscillateFilterParameter(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aFilterId, unsigned int aAttributeId, float aFrom, float aTo, double aTime);
      ;; double Soloud_getStreamTime(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; double Soloud_getStreamPosition(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; int Soloud_getPause(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; float Soloud_getVolume(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; float Soloud_getOverallVolume(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; float Soloud_getPan(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; float Soloud_getSamplerate(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; int Soloud_getProtectVoice(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; unsigned int Soloud_getActiveVoiceCount(Soloud * aSoloud);
      ;; unsigned int Soloud_getVoiceCount(Soloud * aSoloud);
      ;; int Soloud_isValidVoiceHandle(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; float Soloud_getRelativePlaySpeed(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; float Soloud_getPostClipScaler(Soloud * aSoloud);
      ;; unsigned int Soloud_getMainResampler(Soloud * aSoloud);
      ;; float Soloud_getGlobalVolume(Soloud * aSoloud);
      ;; unsigned int Soloud_getMaxActiveVoiceCount(Soloud * aSoloud);
      ;; int Soloud_getLooping(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; int Soloud_getAutoStop(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; double Soloud_getLoopPoint(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; void Soloud_setLoopPoint(Soloud * aSoloud, unsigned int aVoiceHandle, double aLoopPoint);
      ;; void Soloud_setLooping(Soloud * aSoloud, unsigned int aVoiceHandle, int aLooping);
      ;; void Soloud_setAutoStop(Soloud * aSoloud, unsigned int aVoiceHandle, int aAutoStop);
      ;; int Soloud_setMaxActiveVoiceCount(Soloud * aSoloud, unsigned int aVoiceCount);
      ;; void Soloud_setInaudibleBehavior(Soloud * aSoloud, unsigned int aVoiceHandle, int aMustTick, int aKill);
      ;; void Soloud_setGlobalVolume(Soloud * aSoloud, float aVolume);
      ;; void Soloud_setPostClipScaler(Soloud * aSoloud, float aScaler);
      ;; void Soloud_setMainResampler(Soloud * aSoloud, unsigned int aResampler);
      ;; void Soloud_setPause(Soloud * aSoloud, unsigned int aVoiceHandle, int aPause);
      ;; void Soloud_setPauseAll(Soloud * aSoloud, int aPause);
      ;; int Soloud_setRelativePlaySpeed(Soloud * aSoloud, unsigned int aVoiceHandle, float aSpeed);
      ;; void Soloud_setProtectVoice(Soloud * aSoloud, unsigned int aVoiceHandle, int aProtect);
      ;; void Soloud_setSamplerate(Soloud * aSoloud, unsigned int aVoiceHandle, float aSamplerate);
      ;; void Soloud_setPan(Soloud * aSoloud, unsigned int aVoiceHandle, float aPan);
      ;; void Soloud_setPanAbsolute(Soloud * aSoloud, unsigned int aVoiceHandle, float aLVolume, float aRVolume);
      ;; void Soloud_setChannelVolume(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aChannel, float aVolume);
      ;; void Soloud_setVolume(Soloud * aSoloud, unsigned int aVoiceHandle, float aVolume);
      ;; void Soloud_setDelaySamples(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aSamples);
      ;; void Soloud_fadeVolume(Soloud * aSoloud, unsigned int aVoiceHandle, float aTo, double aTime);
      ;; void Soloud_fadePan(Soloud * aSoloud, unsigned int aVoiceHandle, float aTo, double aTime);
      ;; void Soloud_fadeRelativePlaySpeed(Soloud * aSoloud, unsigned int aVoiceHandle, float aTo, double aTime);
      ;; void Soloud_fadeGlobalVolume(Soloud * aSoloud, float aTo, double aTime);
      ;; void Soloud_schedulePause(Soloud * aSoloud, unsigned int aVoiceHandle, double aTime);
      ;; void Soloud_scheduleStop(Soloud * aSoloud, unsigned int aVoiceHandle, double aTime);
      ;; void Soloud_oscillateVolume(Soloud * aSoloud, unsigned int aVoiceHandle, float aFrom, float aTo, double aTime);
      ;; void Soloud_oscillatePan(Soloud * aSoloud, unsigned int aVoiceHandle, float aFrom, float aTo, double aTime);
      ;; void Soloud_oscillateRelativePlaySpeed(Soloud * aSoloud, unsigned int aVoiceHandle, float aFrom, float aTo, double aTime);
      ;; void Soloud_oscillateGlobalVolume(Soloud * aSoloud, float aFrom, float aTo, double aTime);
      ;; void Soloud_setGlobalFilter(Soloud * aSoloud, unsigned int aFilterId, Filter * aFilter);
      ;; void Soloud_setVisualizationEnable(Soloud * aSoloud, int aEnable);
      ;; float * Soloud_calcFFT(Soloud * aSoloud);
      ;; float * Soloud_getWave(Soloud * aSoloud);
      ;; float Soloud_getApproximateVolume(Soloud * aSoloud, unsigned int aChannel);
      ;; unsigned int Soloud_getLoopCount(Soloud * aSoloud, unsigned int aVoiceHandle);
      ;; float Soloud_getInfo(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aInfoKey);
      ;; unsigned int Soloud_createVoiceGroup(Soloud * aSoloud);
      ;; int Soloud_destroyVoiceGroup(Soloud * aSoloud, unsigned int aVoiceGroupHandle);
      ;; int Soloud_addVoiceToGroup(Soloud * aSoloud, unsigned int aVoiceGroupHandle, unsigned int aVoiceHandle);
      ;; int Soloud_isVoiceGroup(Soloud * aSoloud, unsigned int aVoiceGroupHandle);
      ;; int Soloud_isVoiceGroupEmpty(Soloud * aSoloud, unsigned int aVoiceGroupHandle);
      ;; void Soloud_update3dAudio(Soloud * aSoloud);
      ;; int Soloud_set3dSoundSpeed(Soloud * aSoloud, float aSpeed);
      ;; float Soloud_get3dSoundSpeed(Soloud * aSoloud);
      ;; void Soloud_set3dListenerParameters(Soloud * aSoloud, float aPosX, float aPosY, float aPosZ, float aAtX, float aAtY, float aAtZ, float aUpX, float aUpY, float aUpZ);
      ;; void Soloud_set3dListenerParametersEx(Soloud * aSoloud, float aPosX, float aPosY, float aPosZ, float aAtX, float aAtY, float aAtZ, float aUpX, float aUpY, float aUpZ, float aVelocityX /* = 0.0f */, float aVelocityY /* = 0.0f */, float aVelocityZ /* = 0.0f */);
      ;; void Soloud_set3dListenerPosition(Soloud * aSoloud, float aPosX, float aPosY, float aPosZ);
      ;; void Soloud_set3dListenerAt(Soloud * aSoloud, float aAtX, float aAtY, float aAtZ);
      ;; void Soloud_set3dListenerUp(Soloud * aSoloud, float aUpX, float aUpY, float aUpZ);
      ;; void Soloud_set3dListenerVelocity(Soloud * aSoloud, float aVelocityX, float aVelocityY, float aVelocityZ);
      ;; void Soloud_set3dSourceParameters(Soloud * aSoloud, unsigned int aVoiceHandle, float aPosX, float aPosY, float aPosZ);
      ;; void Soloud_set3dSourceParametersEx(Soloud * aSoloud, unsigned int aVoiceHandle, float aPosX, float aPosY, float aPosZ, float aVelocityX /* = 0.0f */, float aVelocityY /* = 0.0f */, float aVelocityZ /* = 0.0f */);
      ;; void Soloud_set3dSourcePosition(Soloud * aSoloud, unsigned int aVoiceHandle, float aPosX, float aPosY, float aPosZ);
      ;; void Soloud_set3dSourceVelocity(Soloud * aSoloud, unsigned int aVoiceHandle, float aVelocityX, float aVelocityY, float aVelocityZ);
      ;; void Soloud_set3dSourceMinMaxDistance(Soloud * aSoloud, unsigned int aVoiceHandle, float aMinDistance, float aMaxDistance);
      ;; void Soloud_set3dSourceAttenuation(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aAttenuationModel, float aAttenuationRolloffFactor);
      ;; void Soloud_set3dSourceDopplerFactor(Soloud * aSoloud, unsigned int aVoiceHandle, float aDopplerFactor);
      ;; void Soloud_mix(Soloud * aSoloud, float * aBuffer, unsigned int aSamples);
      ;; void Soloud_mixSigned16(Soloud * aSoloud, short * aBuffer, unsigned int aSamples);

      BASSBOOSTFILTER_WET
      BASSBOOSTFILTER_BOOST
      BIQUADRESONANTFILTER_LOWPASS
      BIQUADRESONANTFILTER_HIGHPASS
      BIQUADRESONANTFILTER_BANDPASS
      BIQUADRESONANTFILTER_WET
      BIQUADRESONANTFILTER_TYPE
      BIQUADRESONANTFILTER_FREQUENCY
      BIQUADRESONANTFILTER_RESONANCE
      ECHOFILTER_WET
      ECHOFILTER_DELAY
      ECHOFILTER_DECAY
      ECHOFILTER_FILTER
      FLANGERFILTER_WET
      FLANGERFILTER_DELAY
      FLANGERFILTER_FREQ
      FREEVERBFILTER_WET
      FREEVERBFILTER_FREEZE
      FREEVERBFILTER_ROOMSIZE
      FREEVERBFILTER_DAMP
      FREEVERBFILTER_WIDTH
      LOFIFILTER_WET
      LOFIFILTER_SAMPLERATE
      LOFIFILTER_BITDEPTH
      NOISE_WHITE
      NOISE_PINK
      NOISE_BROWNISH
      NOISE_BLUEISH
      ROBOTIZEFILTER_WET
      ROBOTIZEFILTER_FREQ
      ROBOTIZEFILTER_WAVE
      SFXR_COIN
      SFXR_LASER
      SFXR_EXPLOSION
      SFXR_POWERUP
      SFXR_HURT
      SFXR_JUMP
      SFXR_BLIP

      ; Speech
      SPEECH_KW_SAW
      SPEECH_KW_TRIANGLE
      SPEECH_KW_SIN
      SPEECH_KW_SQUARE
      SPEECH_KW_PULSE
      SPEECH_KW_NOISE
      SPEECH_KW_WARBLE
      ;; void Speech_destroy(Speech * aSpeech);
      Speech_create ;; Speech * ();
      Speech_setText ;; int (Speech * aSpeech, const char * aText);
      ;; int Speech_setParams(Speech * aSpeech);
      ;; int Speech_setParamsEx(Speech * aSpeech, unsigned int aBaseFrequency /* = 1330 */, float aBaseSpeed /* = 10.0f */, float aBaseDeclination /* = 0.5f */, int aBaseWaveform /* = KW_TRIANGLE */);
      ;; void Speech_setVolume(Speech * aSpeech, float aVolume);
      ;; void Speech_setLooping(Speech * aSpeech, int aLoop);
      ;; void Speech_setAutoStop(Speech * aSpeech, int aAutoStop);
      ;; void Speech_set3dMinMaxDistance(Speech * aSpeech, float aMinDistance, float aMaxDistance);
      ;; void Speech_set3dAttenuation(Speech * aSpeech, unsigned int aAttenuationModel, float aAttenuationRolloffFactor);
      ;; void Speech_set3dDopplerFactor(Speech * aSpeech, float aDopplerFactor);
      ;; void Speech_set3dListenerRelative(Speech * aSpeech, int aListenerRelative);
      ;; void Speech_set3dDistanceDelay(Speech * aSpeech, int aDistanceDelay);
      ;; void Speech_set3dCollider(Speech * aSpeech, AudioCollider * aCollider);
      ;; void Speech_set3dColliderEx(Speech * aSpeech, AudioCollider * aCollider, int aUserData /* = 0 */);
      ;; void Speech_set3dAttenuator(Speech * aSpeech, AudioAttenuator * aAttenuator);
      ;; void Speech_setInaudibleBehavior(Speech * aSpeech, int aMustTick, int aKill);
      ;; void Speech_setLoopPoint(Speech * aSpeech, double aLoopPoint);
      ;; double Speech_getLoopPoint(Speech * aSpeech);
      ;; void Speech_setFilter(Speech * aSpeech, unsigned int aFilterId, Filter * aFilter);
      ;; void Speech_stop(Speech * aSpeech);

      VIC_PAL
      VIC_NTSC
      VIC_BASS
      VIC_ALTO
      VIC_SOPRANO
      VIC_NOISE
      VIC_MAX_REGS
      WAVESHAPERFILTER_WET
      WAVESHAPERFILTER_AMOUNT
   )
   (import
      (scheme core)
      (otus ffi))

(begin
   (define SOLOUD (load-dynamic-library "libsoloud.so"))

   ;; typedef void * AlignedFloatBuffer;
   ;; typedef void * TinyAlignedFloatBuffer;
   (define Soloud* type-vptr)
   ;; typedef void * Ay;
   ;; typedef void * AudioCollider;
   ;; typedef void * AudioAttenuator;
   ;; typedef void * AudioSource;
   ;; typedef void * BassboostFilter;
   ;; typedef void * BiquadResonantFilter;
   ;; typedef void * Bus;
   ;; typedef void * DCRemovalFilter;
   ;; typedef void * EchoFilter;
   ;; typedef void * Fader;
   ;; typedef void * FFTFilter;
   ;; typedef void * Filter;
   ;; typedef void * FlangerFilter;
   ;; typedef void * FreeverbFilter;
   ;; typedef void * LofiFilter;
   ;; typedef void * Monotone;
   ;; typedef void * Noise;
   ;; typedef void * Openmpt;
   ;; typedef void * Queue;
   ;; typedef void * RobotizeFilter;
   ;; typedef void * Sfxr;
   (define Speech* type-vptr)
   ;; typedef void * TedSid;
   ;; typedef void * Vic;
   ;; typedef void * Vizsn;
   ;; typedef void * Wav;
   ;; typedef void * WaveShaperFilter;
   ;; typedef void * WavStream;
   ;; typedef void * File;

   (define int fft-int)
   (define void fft-void)
   (define char* type-string)

	(define SOLOUD_AUTO 0)
	(define SOLOUD_SDL1 1)
	(define SOLOUD_SDL2 2)
	(define SOLOUD_PORTAUDIO 3)
	(define SOLOUD_WINMM 4)
	(define SOLOUD_XAUDIO2 5)
	(define SOLOUD_WASAPI 6)
	(define SOLOUD_ALSA 7)
	(define SOLOUD_JACK 8)
	(define SOLOUD_OSS 9)
	(define SOLOUD_OPENAL 10)
	(define SOLOUD_COREAUDIO 11)
	(define SOLOUD_OPENSLES 12)
	(define SOLOUD_VITA_HOMEBREW 13)
	(define SOLOUD_MINIAUDIO 14)
	(define SOLOUD_NOSOUND 15)
	(define SOLOUD_NULLDRIVER 16)
	(define SOLOUD_BACKEND_MAX 17)
	(define SOLOUD_CLIP_ROUNDOFF 1)
	(define SOLOUD_ENABLE_VISUALIZATION 2)
	(define SOLOUD_LEFT_HANDED_3D 4)
	(define SOLOUD_NO_FPU_REGISTER_CHANGE 8)
	(define SOLOUD_WAVE_SQUARE 0)
	(define SOLOUD_WAVE_SAW 1)
	(define SOLOUD_WAVE_SIN 2)
	(define SOLOUD_WAVE_TRIANGLE 3)
	(define SOLOUD_WAVE_BOUNCE 4)
	(define SOLOUD_WAVE_JAWS 5)
	(define SOLOUD_WAVE_HUMPS 6)
	(define SOLOUD_WAVE_FSQUARE 7)
	(define SOLOUD_WAVE_FSAW 8)
	(define SOLOUD_RESAMPLER_POINT 0)
	(define SOLOUD_RESAMPLER_LINEAR 1)
	(define SOLOUD_RESAMPLER_CATMULLROM 2)

   ;; void Soloud_destroy(Soloud * aSoloud);
   (define Soloud_create (SOLOUD Soloud* "Soloud_create"))
   ;; int Soloud_init(Soloud * aSoloud);
   (define Soloud_initEx (SOLOUD int "Soloud_initEx" Soloud* fft-unsigned-int fft-unsigned-int fft-unsigned-int fft-unsigned-int fft-unsigned-int))
   ;; void Soloud_deinit(Soloud * aSoloud);
   ;; unsigned int Soloud_getVersion(Soloud * aSoloud);
   ;; const char * Soloud_getErrorString(Soloud * aSoloud, int aErrorCode);
   ;; unsigned int Soloud_getBackendId(Soloud * aSoloud);
   ;; const char * Soloud_getBackendString(Soloud * aSoloud);
   ;; unsigned int Soloud_getBackendChannels(Soloud * aSoloud);
   ;; unsigned int Soloud_getBackendSamplerate(Soloud * aSoloud);
   ;; unsigned int Soloud_getBackendBufferSize(Soloud * aSoloud);
   ;; int Soloud_setSpeakerPosition(Soloud * aSoloud, unsigned int aChannel, float aX, float aY, float aZ);
   ;; int Soloud_getSpeakerPosition(Soloud * aSoloud, unsigned int aChannel, float * aX, float * aY, float * aZ);
   ;; unsigned int Soloud_play(Soloud * aSoloud, AudioSource * aSound);
   ;; unsigned int Soloud_playEx(Soloud * aSoloud, AudioSource * aSound, float aVolume /* = -1.0f */, float aPan /* = 0.0f */, int aPaused /* = 0 */, unsigned int aBus /* = 0 */);
   ;; unsigned int Soloud_playClocked(Soloud * aSoloud, double aSoundTime, AudioSource * aSound);
   ;; unsigned int Soloud_playClockedEx(Soloud * aSoloud, double aSoundTime, AudioSource * aSound, float aVolume /* = -1.0f */, float aPan /* = 0.0f */, unsigned int aBus /* = 0 */);
   ;; unsigned int Soloud_play3d(Soloud * aSoloud, AudioSource * aSound, float aPosX, float aPosY, float aPosZ);
   ;; unsigned int Soloud_play3dEx(Soloud * aSoloud, AudioSource * aSound, float aPosX, float aPosY, float aPosZ, float aVelX /* = 0.0f */, float aVelY /* = 0.0f */, float aVelZ /* = 0.0f */, float aVolume /* = 1.0f */, int aPaused /* = 0 */, unsigned int aBus /* = 0 */);
   ;; unsigned int Soloud_play3dClocked(Soloud * aSoloud, double aSoundTime, AudioSource * aSound, float aPosX, float aPosY, float aPosZ);
   ;; unsigned int Soloud_play3dClockedEx(Soloud * aSoloud, double aSoundTime, AudioSource * aSound, float aPosX, float aPosY, float aPosZ, float aVelX /* = 0.0f */, float aVelY /* = 0.0f */, float aVelZ /* = 0.0f */, float aVolume /* = 1.0f */, unsigned int aBus /* = 0 */);
   ;; unsigned int Soloud_playBackground(Soloud * aSoloud, AudioSource * aSound);
   ;; unsigned int Soloud_playBackgroundEx(Soloud * aSoloud, AudioSource * aSound, float aVolume /* = -1.0f */, int aPaused /* = 0 */, unsigned int aBus /* = 0 */);
   ;; int Soloud_seek(Soloud * aSoloud, unsigned int aVoiceHandle, double aSeconds);
   ;; void Soloud_stop(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; void Soloud_stopAll(Soloud * aSoloud);
   ;; void Soloud_stopAudioSource(Soloud * aSoloud, AudioSource * aSound);
   ;; int Soloud_countAudioSource(Soloud * aSoloud, AudioSource * aSound);
   ;; void Soloud_setFilterParameter(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aFilterId, unsigned int aAttributeId, float aValue);
   ;; float Soloud_getFilterParameter(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aFilterId, unsigned int aAttributeId);
   ;; void Soloud_fadeFilterParameter(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aFilterId, unsigned int aAttributeId, float aTo, double aTime);
   ;; void Soloud_oscillateFilterParameter(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aFilterId, unsigned int aAttributeId, float aFrom, float aTo, double aTime);
   ;; double Soloud_getStreamTime(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; double Soloud_getStreamPosition(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; int Soloud_getPause(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; float Soloud_getVolume(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; float Soloud_getOverallVolume(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; float Soloud_getPan(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; float Soloud_getSamplerate(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; int Soloud_getProtectVoice(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; unsigned int Soloud_getActiveVoiceCount(Soloud * aSoloud);
   ;; unsigned int Soloud_getVoiceCount(Soloud * aSoloud);
   ;; int Soloud_isValidVoiceHandle(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; float Soloud_getRelativePlaySpeed(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; float Soloud_getPostClipScaler(Soloud * aSoloud);
   ;; unsigned int Soloud_getMainResampler(Soloud * aSoloud);
   ;; float Soloud_getGlobalVolume(Soloud * aSoloud);
   ;; unsigned int Soloud_getMaxActiveVoiceCount(Soloud * aSoloud);
   ;; int Soloud_getLooping(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; int Soloud_getAutoStop(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; double Soloud_getLoopPoint(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; void Soloud_setLoopPoint(Soloud * aSoloud, unsigned int aVoiceHandle, double aLoopPoint);
   ;; void Soloud_setLooping(Soloud * aSoloud, unsigned int aVoiceHandle, int aLooping);
   ;; void Soloud_setAutoStop(Soloud * aSoloud, unsigned int aVoiceHandle, int aAutoStop);
   ;; int Soloud_setMaxActiveVoiceCount(Soloud * aSoloud, unsigned int aVoiceCount);
   ;; void Soloud_setInaudibleBehavior(Soloud * aSoloud, unsigned int aVoiceHandle, int aMustTick, int aKill);
   ;; void Soloud_setGlobalVolume(Soloud * aSoloud, float aVolume);
   ;; void Soloud_setPostClipScaler(Soloud * aSoloud, float aScaler);
   ;; void Soloud_setMainResampler(Soloud * aSoloud, unsigned int aResampler);
   ;; void Soloud_setPause(Soloud * aSoloud, unsigned int aVoiceHandle, int aPause);
   ;; void Soloud_setPauseAll(Soloud * aSoloud, int aPause);
   ;; int Soloud_setRelativePlaySpeed(Soloud * aSoloud, unsigned int aVoiceHandle, float aSpeed);
   ;; void Soloud_setProtectVoice(Soloud * aSoloud, unsigned int aVoiceHandle, int aProtect);
   ;; void Soloud_setSamplerate(Soloud * aSoloud, unsigned int aVoiceHandle, float aSamplerate);
   ;; void Soloud_setPan(Soloud * aSoloud, unsigned int aVoiceHandle, float aPan);
   ;; void Soloud_setPanAbsolute(Soloud * aSoloud, unsigned int aVoiceHandle, float aLVolume, float aRVolume);
   ;; void Soloud_setChannelVolume(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aChannel, float aVolume);
   ;; void Soloud_setVolume(Soloud * aSoloud, unsigned int aVoiceHandle, float aVolume);
   ;; void Soloud_setDelaySamples(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aSamples);
   ;; void Soloud_fadeVolume(Soloud * aSoloud, unsigned int aVoiceHandle, float aTo, double aTime);
   ;; void Soloud_fadePan(Soloud * aSoloud, unsigned int aVoiceHandle, float aTo, double aTime);
   ;; void Soloud_fadeRelativePlaySpeed(Soloud * aSoloud, unsigned int aVoiceHandle, float aTo, double aTime);
   ;; void Soloud_fadeGlobalVolume(Soloud * aSoloud, float aTo, double aTime);
   ;; void Soloud_schedulePause(Soloud * aSoloud, unsigned int aVoiceHandle, double aTime);
   ;; void Soloud_scheduleStop(Soloud * aSoloud, unsigned int aVoiceHandle, double aTime);
   ;; void Soloud_oscillateVolume(Soloud * aSoloud, unsigned int aVoiceHandle, float aFrom, float aTo, double aTime);
   ;; void Soloud_oscillatePan(Soloud * aSoloud, unsigned int aVoiceHandle, float aFrom, float aTo, double aTime);
   ;; void Soloud_oscillateRelativePlaySpeed(Soloud * aSoloud, unsigned int aVoiceHandle, float aFrom, float aTo, double aTime);
   ;; void Soloud_oscillateGlobalVolume(Soloud * aSoloud, float aFrom, float aTo, double aTime);
   ;; void Soloud_setGlobalFilter(Soloud * aSoloud, unsigned int aFilterId, Filter * aFilter);
   ;; void Soloud_setVisualizationEnable(Soloud * aSoloud, int aEnable);
   ;; float * Soloud_calcFFT(Soloud * aSoloud);
   ;; float * Soloud_getWave(Soloud * aSoloud);
   ;; float Soloud_getApproximateVolume(Soloud * aSoloud, unsigned int aChannel);
   ;; unsigned int Soloud_getLoopCount(Soloud * aSoloud, unsigned int aVoiceHandle);
   ;; float Soloud_getInfo(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aInfoKey);
   ;; unsigned int Soloud_createVoiceGroup(Soloud * aSoloud);
   ;; int Soloud_destroyVoiceGroup(Soloud * aSoloud, unsigned int aVoiceGroupHandle);
   ;; int Soloud_addVoiceToGroup(Soloud * aSoloud, unsigned int aVoiceGroupHandle, unsigned int aVoiceHandle);
   ;; int Soloud_isVoiceGroup(Soloud * aSoloud, unsigned int aVoiceGroupHandle);
   ;; int Soloud_isVoiceGroupEmpty(Soloud * aSoloud, unsigned int aVoiceGroupHandle);
   ;; void Soloud_update3dAudio(Soloud * aSoloud);
   ;; int Soloud_set3dSoundSpeed(Soloud * aSoloud, float aSpeed);
   ;; float Soloud_get3dSoundSpeed(Soloud * aSoloud);
   ;; void Soloud_set3dListenerParameters(Soloud * aSoloud, float aPosX, float aPosY, float aPosZ, float aAtX, float aAtY, float aAtZ, float aUpX, float aUpY, float aUpZ);
   ;; void Soloud_set3dListenerParametersEx(Soloud * aSoloud, float aPosX, float aPosY, float aPosZ, float aAtX, float aAtY, float aAtZ, float aUpX, float aUpY, float aUpZ, float aVelocityX /* = 0.0f */, float aVelocityY /* = 0.0f */, float aVelocityZ /* = 0.0f */);
   ;; void Soloud_set3dListenerPosition(Soloud * aSoloud, float aPosX, float aPosY, float aPosZ);
   ;; void Soloud_set3dListenerAt(Soloud * aSoloud, float aAtX, float aAtY, float aAtZ);
   ;; void Soloud_set3dListenerUp(Soloud * aSoloud, float aUpX, float aUpY, float aUpZ);
   ;; void Soloud_set3dListenerVelocity(Soloud * aSoloud, float aVelocityX, float aVelocityY, float aVelocityZ);
   ;; void Soloud_set3dSourceParameters(Soloud * aSoloud, unsigned int aVoiceHandle, float aPosX, float aPosY, float aPosZ);
   ;; void Soloud_set3dSourceParametersEx(Soloud * aSoloud, unsigned int aVoiceHandle, float aPosX, float aPosY, float aPosZ, float aVelocityX /* = 0.0f */, float aVelocityY /* = 0.0f */, float aVelocityZ /* = 0.0f */);
   ;; void Soloud_set3dSourcePosition(Soloud * aSoloud, unsigned int aVoiceHandle, float aPosX, float aPosY, float aPosZ);
   ;; void Soloud_set3dSourceVelocity(Soloud * aSoloud, unsigned int aVoiceHandle, float aVelocityX, float aVelocityY, float aVelocityZ);
   ;; void Soloud_set3dSourceMinMaxDistance(Soloud * aSoloud, unsigned int aVoiceHandle, float aMinDistance, float aMaxDistance);
   ;; void Soloud_set3dSourceAttenuation(Soloud * aSoloud, unsigned int aVoiceHandle, unsigned int aAttenuationModel, float aAttenuationRolloffFactor);
   ;; void Soloud_set3dSourceDopplerFactor(Soloud * aSoloud, unsigned int aVoiceHandle, float aDopplerFactor);
   ;; void Soloud_mix(Soloud * aSoloud, float * aBuffer, unsigned int aSamples);
   ;; void Soloud_mixSigned16(Soloud * aSoloud, short * aBuffer, unsigned int aSamples);


	(define BASSBOOSTFILTER_WET 0)
	(define BASSBOOSTFILTER_BOOST 1)
	(define BIQUADRESONANTFILTER_LOWPASS 0)
	(define BIQUADRESONANTFILTER_HIGHPASS 1)
	(define BIQUADRESONANTFILTER_BANDPASS 2)
	(define BIQUADRESONANTFILTER_WET 0)
	(define BIQUADRESONANTFILTER_TYPE 1)
	(define BIQUADRESONANTFILTER_FREQUENCY 2)
	(define BIQUADRESONANTFILTER_RESONANCE 3)
	(define ECHOFILTER_WET 0)
	(define ECHOFILTER_DELAY 1)
	(define ECHOFILTER_DECAY 2)
	(define ECHOFILTER_FILTER 3)
	(define FLANGERFILTER_WET 0)
	(define FLANGERFILTER_DELAY 1)
	(define FLANGERFILTER_FREQ 2)
	(define FREEVERBFILTER_WET 0)
	(define FREEVERBFILTER_FREEZE 1)
	(define FREEVERBFILTER_ROOMSIZE 2)
	(define FREEVERBFILTER_DAMP 3)
	(define FREEVERBFILTER_WIDTH 4)
	(define LOFIFILTER_WET 0)
	(define LOFIFILTER_SAMPLERATE 1)
	(define LOFIFILTER_BITDEPTH 2)
	(define NOISE_WHITE 0)
	(define NOISE_PINK 1)
	(define NOISE_BROWNISH 2)
	(define NOISE_BLUEISH 3)
	(define ROBOTIZEFILTER_WET 0)
	(define ROBOTIZEFILTER_FREQ 1)
	(define ROBOTIZEFILTER_WAVE 2)
	(define SFXR_COIN 0)
	(define SFXR_LASER 1)
	(define SFXR_EXPLOSION 2)
	(define SFXR_POWERUP 3)
	(define SFXR_HURT 4)
	(define SFXR_JUMP 5)
	(define SFXR_BLIP 6)

   ;; Speech
	(define SPEECH_KW_SAW 0)
	(define SPEECH_KW_TRIANGLE 1)
	(define SPEECH_KW_SIN 2)
	(define SPEECH_KW_SQUARE 3)
	(define SPEECH_KW_PULSE 4)
	(define SPEECH_KW_NOISE 5)
	(define SPEECH_KW_WARBLE 6)

   ;; void Speech_destroy(Speech * aSpeech);
   (define Speech_create (SOLOUD Speech* "Speech_create"))
   (define Speech_setText (SOLOUD int "Speech_setText" Speech* char*))
   ;; int Speech_setParams(Speech * aSpeech);
   ;; int Speech_setParamsEx(Speech * aSpeech, unsigned int aBaseFrequency /* = 1330 */, float aBaseSpeed /* = 10.0f */, float aBaseDeclination /* = 0.5f */, int aBaseWaveform /* = KW_TRIANGLE */);
   ;; void Speech_setVolume(Speech * aSpeech, float aVolume);
   ;; void Speech_setLooping(Speech * aSpeech, int aLoop);
   ;; void Speech_setAutoStop(Speech * aSpeech, int aAutoStop);
   ;; void Speech_set3dMinMaxDistance(Speech * aSpeech, float aMinDistance, float aMaxDistance);
   ;; void Speech_set3dAttenuation(Speech * aSpeech, unsigned int aAttenuationModel, float aAttenuationRolloffFactor);
   ;; void Speech_set3dDopplerFactor(Speech * aSpeech, float aDopplerFactor);
   ;; void Speech_set3dListenerRelative(Speech * aSpeech, int aListenerRelative);
   ;; void Speech_set3dDistanceDelay(Speech * aSpeech, int aDistanceDelay);
   ;; void Speech_set3dCollider(Speech * aSpeech, AudioCollider * aCollider);
   ;; void Speech_set3dColliderEx(Speech * aSpeech, AudioCollider * aCollider, int aUserData /* = 0 */);
   ;; void Speech_set3dAttenuator(Speech * aSpeech, AudioAttenuator * aAttenuator);
   ;; void Speech_setInaudibleBehavior(Speech * aSpeech, int aMustTick, int aKill);
   ;; void Speech_setLoopPoint(Speech * aSpeech, double aLoopPoint);
   ;; double Speech_getLoopPoint(Speech * aSpeech);
   ;; void Speech_setFilter(Speech * aSpeech, unsigned int aFilterId, Filter * aFilter);
   ;; void Speech_stop(Speech * aSpeech);


	(define VIC_PAL 0)
	(define VIC_NTSC 1)
	(define VIC_BASS 0)
	(define VIC_ALTO 1)
	(define VIC_SOPRANO 2)
	(define VIC_NOISE 3)
	(define VIC_MAX_REGS 4)
	(define WAVESHAPERFILTER_WET 0)
	(define WAVESHAPERFILTER_AMOUNT 1)

))