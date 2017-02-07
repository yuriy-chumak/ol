(define-library (lib curl)
   (import (r5rs core)
           (otus pinvoke))
   (export
      curl_easy_init
      curl_easy_setopt
         CURLOPT_URL
         CURLOPT_WRITEFUNCTION
      curl_easy_perform
      curl_easy_cleanup)

(begin

   (define libcurl (or (dlopen "libcurl.so.4")
                       (runtime-error "Can't load libcurl" #f)))

   (define curl_easy_init (dlsym libcurl type-vptr "curl_easy_init"))
   (define curl_easy_setopt (dlsym libcurl type-void "curl_easy_setopt" type-vptr type-int+ type-any))
      (define CURLOPT_URL 10002)
      (define CURLOPT_WRITEFUNCTION 20011)
   (define curl_easy_perform (dlsym libcurl type-void "curl_easy_perform" type-vptr))
   (define curl_easy_cleanup (dlsym libcurl type-void "curl_easy_cleanup" type-vptr))
))
