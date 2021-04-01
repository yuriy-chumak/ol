#!/usr/bin/env ol

(import (lib openal) (otus ffi))

(define device (alcOpenDevice #false))
(define context (alcCreateContext device #false))
(alcMakeContextCurrent context)

(print "OpenAL version: " (alGetString AL_VERSION))
(print "OpenAL vendor: " (alGetString AL_VENDOR))
(print "OpenAL renderer: " (alGetString AL_RENDERER))
;(print (alGetString AL_EXTENSIONS))

(define buffer (make-32bit-array 1))
(alGenBuffers 1 buffer)
(print "buffer id: " buffer)

(al:decode-file buffer "helloworld.snd")
;al:decode-fd buffer (open-input-file "helloworld.snd");

(define source (make-32bit-array 1))
(alGenSources 1 source)
(print "source id: " source)

(alSourcei (list-ref source 0) AL_BUFFER (list-ref buffer 0))
(alSourcei (list-ref source 0) AL_LOOPING 1)

(alSourcePlay (list-ref source 0))
(print "play error: " (alGetError))

(display "> ")
(read)
