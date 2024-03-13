#!/usr/bin/env ol
(import (otus ffi))
(import (lib leveldb))

(print "leveldb version: " (leveldb_major_version) "." (leveldb_minor_version))

;; --------------------------------
;;   OPEN
;; --------------------------------

; simple open/close
(define db (leveldb:open "testdb"))
(if db (leveldb_close db))

; smart open
(define db (leveldb:open "testdb" {
   'create_if_missing #true
   'compression leveldb_no_compression
}))


;; --------------------------------
;;   READ
;; --------------------------------
(define (do_read db)
   (print "--- read ---")

   ; any ol objects
   (print "object: "
      (leveldb:get db "i-want-to-read-an-object"))

   (display "reading internal object... ")
   (define oread (leveldb:get db "+"))
   (print "internal object: " oread)
   (print "try to use loaded object: (oread 1 2 3): " (if oread (oread 1 2 3)))


   ; integer
   (print "integer: "
      (leveldb:get db "i-want-to-read-an-integer" integer?))

   (print "integer with options 1: "
      (leveldb:get db "i-want-to-read-an-integer" integer? {}))

   (print "integer with options 2: "
      (leveldb:get db "i-want-to-read-an-integer" {
         'verify_checksums #false
         'fill_cache #true
      } integer?))

   ; string
   (print "reading string by string key: "
      (leveldb:get db "string-key-string-value" string?))
   (print "reading string by integer key: "
      (leveldb:get db 123777 string?))
)

; negative test, try to read non existent data if database just created and empty
(do_read db)

;; --------------------------------
;;   WRITE
;; --------------------------------

;; SMART WRITE
; writing string key and string value
(leveldb:put db "string-key-string-value" "a-string-value")

; writing integer key and string value
(leveldb:put db 123777 "a-string-123777-value")

; writing string key and string value
(leveldb:put db 1234567 7654321)

; writing internal object (a '+' function)...
(leveldb:put db "+" +)


; reread
(do_read db)

(leveldb_close db)
; -=( iterators )=--------------------------

(define db (leveldb:open "intint" {
   'create_if_missing #true
   'compression leveldb_no_compression
}))

(for-each (lambda (x)
      (leveldb:put db x (* x x)))
   (iota 8))


(define iter (leveldb_create_iterator db (leveldb:make-readoptions {})))
(leveldb_iter_seek_to_first iter)
(let loop ()
   (when (eq? (leveldb_iter_valid iter) 1)
      (define kl (box 0))
      (define key (leveldb_iter_key iter kl))
      (display "key: ")
      (display (decode key (unbox kl) integer?))

      (define vl (box 0))
      (define value (leveldb_iter_value iter vl))
      (display ", value: ")
      (display (decode value (unbox vl) integer?))

      (print)

      (leveldb_iter_next iter)
      (loop)))
(leveldb_iter_destroy iter)

; done
(leveldb_close db)
(print "Ok.")
