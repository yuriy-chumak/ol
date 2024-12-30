#!/usr/bin/env ol

(import (OpenAL 1.1))

; init
(define dev (alcOpenDevice #f))
(define ctx (alcCreateContext dev #f))
(alcMakeContextCurrent ctx)

; info
(print-to stderr "OpenAL version: " (alGetString AL_VERSION))
(print-to stderr "OpenAL vendor: " (alGetString AL_VENDOR))
(print-to stderr "OpenAL renderer: " (alGetString AL_RENDERER))
(print-to stderr "OpenAL extensions: " (alGetString AL_EXTENSIONS))
(print)

; OpenAL extensions
(import (OpenAL EXT ALAW)) ; AL_EXT_ALAW
(import (OpenAL EXT BFORMAT)) ; AL_EXT_BFORMAT
(import (OpenAL EXT double)) ; AL_EXT_DOUBLE
(import (OpenAL EXT EXPONENT_DISTANCE)) ; AL_EXT_EXPONENT_DISTANCE
(import (OpenAL EXT float32)) ; AL_EXT_FLOAT32
(import (OpenAL EXT IMA4)) ; AL_EXT_IMA4
(import (OpenAL EXT LINEAR_DISTANCE)) ; AL_EXT_LINEAR_DISTANCE
(import (OpenAL EXT MCFORMATS)) ; AL_EXT_MCFORMATS
(import (OpenAL EXT MULAW)) ; AL_EXT_MULAW
(import (OpenAL EXT MULAW_BFORMAT)) ; AL_EXT_MULAW_BFORMAT
(import (OpenAL EXT MULAW_MCFORMATS)) ; AL_EXT_MULAW_MCFORMATS
(import (OpenAL EXT OFFSET)) ; AL_EXT_OFFSET
(import (OpenAL EXT source_distance_model)) ; AL_EXT_source_distance_model
(import (OpenAL EXT SOURCE_RADIUS)) ; AL_EXT_SOURCE_RADIUS
(import (OpenAL EXT STEREO_ANGLES)) ; AL_EXT_STEREO_ANGLES
(import (OpenAL EXT vorbis))
(import (OpenAL LOKI quadriphonic)) ; AL_LOKI_quadriphonic
(import (OpenAL SOFT block_alignment)) ; AL_SOFT_block_alignment
(import (OpenAL SOFT deferred_updates)) ; AL_SOFT_deferred_updates
(import (OpenAL SOFT direct_channels)) ; AL_SOFT_direct_channels
;; AL_SOFTX_events
;; AL_SOFTX_filter_gain_ex
(import (OpenAL SOFT gain_clamp_ex)) ; AL_SOFT_gain_clamp_ex
(import (OpenAL SOFT loop_points)) ; AL_SOFT_loop_points
;; AL_SOFTX_map_buffer
(import (OpenAL SOFT MSADPCM)) ; AL_SOFT_MSADPCM
(import (OpenAL SOFT source_latency)) ; AL_SOFT_source_latency
(import (OpenAL SOFT source_length)) ; AL_SOFT_source_length
(import (OpenAL SOFT source_resampler)) ; AL_SOFT_source_resampler
(import (OpenAL SOFT source_spatialize)) ; AL_SOFT_source_spatialize

(print)
(print-to stderr "OpenAL ALC extensions: " (alcGetString dev ALC_EXTENSIONS))
(print)

; ALC extensions
(import (OpenAL ALC ENUMERATE_ALL_EXT)) ;; ALC_ENUMERATE_ALL_EXT
;; ALC_ENUMERATION_EXT
;; ALC_EXT_CAPTURE
;; ALC_EXT_DEDICATED
;; ALC_EXT_disconnect
;; ALC_EXT_EFX
;; ALC_EXT_thread_local_context
;; ALC_SOFT_device_clock
;; ALC_SOFT_HRTF
;; ALC_SOFT_loopback
;; ALC_SOFT_output_limiter
;; ALC_SOFT_pause_device

; done.
(alcMakeContextCurrent #f)
(alcDestroyContext ctx)
(alcCloseDevice dev)
