#!/usr/bin/env ol

(import (lib curl))
(import (otus ffi))

(define callback (vm:pin (cons
   (list fft-void type-string fft-int fft-int type-vptr)
   (lambda (ptr nsize nmemb stream)
      (print ptr))
)))


(define curl (curl_easy_init))
(curl_easy_setopt curl CURLOPT_URL "https://dirty.ru/api/users/1/votes/")
(curl_easy_setopt curl CURLOPT_WRITEFUNCTION (make-callback callback))

(curl_easy_perform curl)
(curl_easy_cleanup curl)
