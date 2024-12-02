; https://github.com/xiph/opusfile
(define-library (lib opusfile)
   (export
      op_open_file
      op_free
      op_channel_count
      op_pcm_total
      op_read
   )
   (import
      (scheme core)
      (otus ffi)
      (lib glib-2))

; = OS DEPENDENT part ===============
(cond-expand
   (Linux
      (begin
         (define OF_LIBRARY (or
            (load-dynamic-library "libopusfile.so")
            (load-dynamic-library "libopusfile.so.0"))) ))
   ; -=( Windows )=--------------------------------------
   (Windows
      (begin
         (define OF_LIBRARY
            (load-dynamic-library "opusfile.dll")) ))
   ;; ; -=( Android )=-----------
   ;; Android
   ;; Emscripten
   ;; Darwin

   (else (begin
      (runtime-error "Unsupported platform" (uname)))))

(begin
   (define OF OF_LIBRARY)

   (define OggOpusFile* fft-void*)
   (setq int fft-int) (setq int& (fft& fft-int))
   (define ogg_int64_t fft-int64)
   (define opus_int16* type-vptr)

   (define op_open_file (OF OggOpusFile* "op_open_file" type-string int&))
   ;op_open_memory
   ;op_vopen_url
   ;op_open_url
   ;op_open_callbacks
   ;op_test_file
   ;op_test_memory
   ;op_vtest_url
   ;op_test_url
   ;op_test_callbacks
   ;op_test_open
   (define op_free (OF void "op_free" OggOpusFile*))
   ;op_seekable
   ;op_link_count
   ;op_serialno
   (define op_channel_count (OF int "op_channel_count" OggOpusFile* int))
   ;op_raw_total
   (define op_pcm_total (OF ogg_int64_t "op_pcm_total" OggOpusFile* int))
   ;op_head
   ;op_tags
   ;op_current_link
   ;op_bitrate
   ;op_bitrate_instant
   ;op_raw_tell
   ;op_pcm_tell
   ;op_raw_seek
   ;op_pcm_seek
   ; ...
   (define op_read (OF int "op_read" OggOpusFile* opus_int16* int int&))
))