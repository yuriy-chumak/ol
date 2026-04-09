#!/usr/bin/env ol

(import (file wav))

(import (lib ALC))
(define device (alcOpenDevice #false))
(define context (alcCreateContext device #false))
(alcMakeContextCurrent context)

(import (OpenAL 1.1))

(print-to stderr "OpenAL version: " (alGetString AL_VERSION))
(print-to stderr "OpenAL vendor: " (alGetString AL_VENDOR))
(print-to stderr "OpenAL renderer: " (alGetString AL_RENDERER))
(print-to stderr "OpenAL extensions: " (alGetString AL_EXTENSIONS))

(define buffer (box 0))
(alGenBuffers 1 buffer)
(print "buffer id: " buffer)

(define source (box 0))
(alGenSources 1 source)
(print "source id: " source)
(alSourcei (unbox source) AL_LOOPING 1)

; ------------------------------------------------
; PCM
(print "  * testing pcm media data:")
(define sound (read-wav-file "media/pcm.wav"))
(unless sound (runtime-error "invalid source file" "media/pcm.wav"))
(assert (eq? (sound 'format) 'pcm))

(print (del sound 'samples))
(alBufferData (unbox buffer)
   (case (sound 'channels)
      (1 (case (sound 'bits-per-sample)
            (8 AL_FORMAT_MONO8)
            (16 AL_FORMAT_MONO16)))
      (2 (case (sound 'bits-per-sample)
            (8 AL_FORMAT_STEREO8)
            (16 AL_FORMAT_STEREO16)))
      (else (runtime-error "unsupported channels count" (sound 'channels))))
   (sound 'samples)  (size (sound 'samples))
   (sound 'sample-rate))

(alSourcei (unbox source) AL_BUFFER (unbox buffer))
(alSourcePlay (unbox source))
(let loop ((stop (+ (clock-ms) 3000))) ; 3 seconds
   (sleep 1)
   (if (< (clock-ms) stop) (loop stop)))
(alSourceStop (unbox source))
(alSourcei (unbox source) AL_BUFFER #f)

; ------------------------------------------------
; aLaw
(print "  * testing aLaw media data:")
(import (OpenAL EXT ALAW))
(if AL_EXT_ALAW
then
   (define sound (read-wav-file "media/alaw.wav"))
   (unless sound (runtime-error "invalid source file" "media/alaw.wav"))
   (assert (eq? (sound 'format) 'alaw))

   (print (del sound 'samples))
   (alBufferData (unbox buffer)
      (case (sound 'channels)
         (1 AL_FORMAT_MONO_ALAW_EXT)
         (2 AL_FORMAT_STEREO_ALAW_EXT)
         (else (runtime-error "unsupported channels count" (sound 'channels))))
      (sound 'samples)  (size (sound 'samples))
      (sound 'sample-rate))

   (alSourcei (unbox source) AL_BUFFER (unbox buffer))
   (alSourcePlay (unbox source))
   (let loop ((stop (+ (clock-ms) 3000)))
      (sleep 1)
      (if (< (clock-ms) stop) (loop stop)))
   (alSourceStop (unbox source))
   (alSourcei (unbox source) AL_BUFFER #f)
else
   (print "no aLaw media is supported."))

; ------------------------------------------------
; aLaw
(print "  * testing muLaw media data:")
(import (OpenAL EXT MULAW))
(if AL_EXT_MULAW
then
   (define sound (read-wav-file "media/mulaw.wav"))
   (unless sound (runtime-error "invalid source file" "media/mulaw.wav"))
   (assert (eq? (sound 'format) 'mulaw))

   (print (del sound 'samples))
   (alBufferData (unbox buffer)
      (case (sound 'channels)
         (1 AL_FORMAT_MONO_MULAW_EXT)
         (2 AL_FORMAT_STEREO_MULAW_EXT)
         (else (runtime-error "unsupported channels count" (sound 'channels))))
      (sound 'samples)  (size (sound 'samples))
      (sound 'sample-rate))

   (alSourcei (unbox source) AL_BUFFER (unbox buffer))
   (alSourcePlay (unbox source))
   (let loop ((stop (+ (clock-ms) 3000)))
      (sleep 1)
      (if (< (clock-ms) stop) (loop stop)))
   (alSourceStop (unbox source))
   (alSourcei (unbox source) AL_BUFFER #f)
else
   (print "no muLaw media is supported."))

; -----------------------------------------------
; all other extensions

(import (OpenAL EXT float32))
(import (OpenAL EXT double))
(import (OpenAL EXT MCFORMATS))
; -----------------------------------------------
; done.
(print)
(alcMakeContextCurrent #f)
(alcDestroyContext context)
(alcCloseDevice device)
(print "done.")
