#!/usr/bin/ol

(import (lib curl))

(define callback (syscall 175 (cons
   (list type-string type-int+ type-int+ type-vptr)
   (lambda (ptr nsize nmemb stream)
      (print ptr)
)) #f #f))




(define curl (curl_easy_init))
(curl_easy_setopt curl CURLOPT_URL (c-string "https://dirty.ru/api/users/1/votes/"))
(curl_easy_setopt curl CURLOPT_WRITEFUNCTION callback)

(curl_easy_perform curl)
(curl_easy_cleanup curl)
