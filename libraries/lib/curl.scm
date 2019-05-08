(define-library (lib curl)
   (import (scheme core)
           (otus ffi))
   (export
      curl_easy_init
      curl_easy_setopt
         CURLOPT_WRITEDATA
         CURLOPT_URL
         CURLOPT_WRITEFUNCTION
      curl_easy_perform
      curl_easy_cleanup)

(begin

   (define libcurl (or (load-dynamic-library "libcurl.so.4")
                       (runtime-error "Can't load libcurl" #f)))

   (define curl_easy_init (libcurl type-vptr "curl_easy_init"))
   (define curl_easy_setopt (libcurl fft-void "curl_easy_setopt" type-vptr fft-int fft-any))
      (define CURLOPT_WRITEDATA 10001)
      (define CURLOPT_URL 10002)
      (define CURLOPT_WRITEFUNCTION 20011)
   (define curl_easy_perform (libcurl fft-void "curl_easy_perform" type-vptr))
   (define curl_easy_cleanup (libcurl fft-void "curl_easy_cleanup" type-vptr))
))
