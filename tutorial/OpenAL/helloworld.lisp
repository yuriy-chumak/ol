#!/usr/bin/ol

(import (lib openal))

(define device (alcOpenDevice null))
(define context (alcCreateContext device null))
(alcMakeContextCurrent context)

(print "OpenAL version: " (alGetString AL_VERSION))
(print "OpenAL vendor: " (alGetString AL_VENDOR))
(print "OpenAL renderer: " (alGetString AL_RENDERER))
;(print (alGetString AL_EXTENSIONS))


(define buffer (vm:raw type-vector-raw 4))
(alGenBuffers 1 buffer)
(print "buffer id: " buffer)

(al:decode-file buffer "helloworld.snd")
;al:decode-fd buffer (open-input-file "helloworld.snd");

(define source (vm:raw type-vector-raw 4))
(alGenSources 1 source)
(print "source id: " source)

(alSourcei (ref source 0) AL_BUFFER (ref buffer 0))
(alSourcei (ref source 0) AL_LOOPING 1)

(alSourcePlay (ref source 0))
(print "play error: " (alGetError))

(display "> ")
(read)
