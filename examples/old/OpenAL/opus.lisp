#!/usr/bin/env ol

(import (OpenAL 1.1))
(import (lib ALC)) ; todo: move to (OpenAL ALC)


; main
(define dev (alcOpenDevice #f))
(define ctx (alcCreateContext dev #f))
(alcMakeContextCurrent ctx)
(print-to stderr "OpenAL version: " (alGetString AL_VERSION))
(print-to stderr "OpenAL vendor: " (alGetString AL_VENDOR))
(print-to stderr "OpenAL renderer: " (alGetString AL_RENDERER))
(print-to stderr "OpenAL extensions: " (alGetString AL_EXTENSIONS))

(define testBuffer (box 0))
(alGenBuffers 1 testBuffer)

(define testSource (box 0))
(alGenSources 1 testSource)

; set listener position and gain
(alListener3f AL_POSITION 0 0 0)
(alListenerf AL_GAIN 1)

; load opus file
(import (lib opusfile))
;load_opus:
(define ok (box 0))
(define file (op_open_file "media/ObservingTheStar.opus" ok))
(unless (zero? (unbox ok))
   (runtime-error "Can't load opus music file"))

(define channels (op_channel_count file -1))
(print "channels: " channels)
(define pcm_size (op_pcm_total file -1))
(print "pcm_size: " pcm_size)
(print "seconds: " (inexact (/ pcm_size 48000)))

; stereo and mono
(define format (case channels
   (1 AL_FORMAT_MONO16)
   (2 AL_FORMAT_STEREO16)))

(define buf (make-bytevector (* pcm_size channels 2))) ; 2 for int16_t
; read data in loop (it not reads whole file)
(let loop ((read 0) (total (* pcm_size channels)))
   (unless (= read pcm_size)
                 ; returns number of samples read per channel
      (define ns (op_read file (cons buf (* read channels 2)) total #f))
      (loop (+ read ns) (- total ns))))
(op_free file)

; send to openal
(define testBuffer (unbox testBuffer))
(define testSource (unbox testSource))

(alBufferData testBuffer format buf (size buf) 48000) ; PCM

(alSourcei testSource AL_BUFFER testBuffer)
(alSource3f testSource AL_POSITION 0 0 0)
(alSourcef testSource AL_GAIN 1)

(alSourcePlay testSource)

(define sourceState (box 0))
(let loop ()
   (call/cc (lambda (break)
      (alGetSourcei testSource AL_SOURCE_STATE sourceState)
      (print "sourceState: " sourceState)
      (if (= (unbox sourceState) AL_STOPPED)
         (break))
      ; sleep for
      (wait 1000)
      (loop))))

(alDeleteSources 1 (list testSource))
(alDeleteBuffers 1 (list testBuffer))

(alcMakeContextCurrent 0)
(alcDestroyContext ctx)
(alcCloseDevice dev)

(print "ok.")