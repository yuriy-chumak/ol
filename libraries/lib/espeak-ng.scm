(define-library (lib espeak-ng)
   (version 1.0)
   (license MIT/LGPL3)
   (description "lib espeak-ng interface")
   (export
      espeak_Initialize
         AUDIO_OUTPUT_PLAYBACK
      espeak_SetVoiceByName
      espeak_SetParameter
         espeakRATE
         espeakPITCH
      espeak_Synth
         POS_CHARACTER
         espeakCHARS_AUTO
      espeak_Synchronize
      espeak_Terminate
   )
(import
   (scheme core)
   (otus ffi))

(cond-expand
   (Linux
      (begin
         (define API (load-dynamic-library "libespeak-ng.so"))))
   (else
      (runtime-error "Unsupported platform" (uname))))

(begin
   (setq int fft-int)
   (setq espeak_ERROR int)
   (setq espeak_AUDIO_OUTPUT int)
   (setq espeak_PARAMETER int)
   (setq espeak_POSITION_TYPE int)

   (define espeak_Initialize (API int "espeak_Initialize" espeak_AUDIO_OUTPUT int type-string int))
      (define AUDIO_OUTPUT_PLAYBACK 0)
   (define espeak_SetVoiceByName (API espeak_ERROR "espeak_SetVoiceByName" type-string))
   (define espeak_SetParameter(API espeak_ERROR "espeak_SetParameter" espeak_PARAMETER int int))
      (define espeakRATE  1)
      (define espeakPITCH 3)

   (define espeak_Synth (API espeak_ERROR "espeak_Synth"
               type-string ; const void *text,
	            fft-size_t  ; size_t size,
	            fft-unsigned-int     ; unsigned int position,
	            espeak_POSITION_TYPE ; espeak_POSITION_TYPE position_type,
	            fft-unsigned-int     ; unsigned int end_position,
	            fft-unsigned-int     ; unsigned int flags,
	            (fft& fft-unsigned-int) ; unsigned int* unique_identifier,
	            fft-void*   ; void* user_data
   ))
      (define POS_CHARACTER 1)
      (define espeakCHARS_AUTO 0)
   
   (define espeak_Synchronize (API espeak_ERROR "espeak_Synchronize"))
   (define espeak_Terminate (API espeak_ERROR "espeak_Terminate"))
))