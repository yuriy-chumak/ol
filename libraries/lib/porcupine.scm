(define-library (lib porcupine)
   (version 1.0)
   (license MIT/LGPL3)
   (description "lib porcupine interface")
   (export
      pv_porcupine_init

      pv_sample_rate
      pv_porcupine_frame_length

      pv_porcupine_process
   )
(import
   (scheme core)
   (otus ffi))

(cond-expand
   (Linux
      (begin
         (define PV_API (load-dynamic-library "./libpv_porcupine.so"))))
   (else
      (runtime-error "Unsupported platform" (uname))))

(begin
   (define pv_status_t fft-int)
   (define pv_porcupine_t* type-vptr)
   (define int32_t& (fft& fft-int32))

   (define pv_porcupine_init (PV_API pv_status_t "pv_porcupine_init"
               type-string  ; const char *access_key,
               type-string  ; const char *model_path,
               type-string  ; const char *device,
               fft-int32    ; int32_t num_keywords,
               (fft* type-string)  ; const char *const *keyword_paths,
               (fft* fft-float)    ; const float *sensitivities,
               (fft* pv_porcupine_t*) ; pv_porcupine_t **object
   ))
   (define pv_sample_rate (PV_API fft-int32 "pv_sample_rate"))
   (define pv_porcupine_frame_length (PV_API fft-int32 "pv_porcupine_frame_length"))

   (define pv_porcupine_process (PV_API pv_status_t "pv_porcupine_process" pv_porcupine_t* type-bytevector int32_t&))
))