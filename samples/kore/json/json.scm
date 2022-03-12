#!/usr/bin/env ol

(import (otus fasl)
   (otus ffi)
   (lib kore)
   (lang embed))

(import (file json))

(define (page req)
   ; http data body
   ; this is not a lazy algorithm!
   (define data (vptr->bytevector
      (kore_http_body_data req)
      (kore_http_body_length req)))

   ; json
   (define json (read-json (bytevector->list data)))
   (print " json: " json)

   (let*((out "answer: \n")
         (out (string-append out "foo.bar: " ((json 'foo {}) 'bar "<not found>") "\n"))
         (out (string-append out "foo.integer: " ((json 'foo {}) 'integer "<not found>") "\n")))

      (define response (string->utf8 out))
      (http_response req 200 response (size response)))
   KORE_RESULT_OK)

; compile:
(fasl-save (make-kore-page page) "tmp.bin")
