#!/usr/bin/env ol
(import (otus ffi))
(import (lib leveldb))

(print "leveldb version: " (leveldb_major_version) "." (leveldb_minor_version))

;; OPEN
(define db (leveldb:open "testdb"))

;; SMART WRITE
(display "writing string value... ")
(if (leveldb:put db "a-string-key" "a-string-value")
   (print "ok."))

(display "writing integer value... ")
(if (leveldb:put db 1234567 7654321)
   (print "ok."))

(display "writing internal object (a '+' function)... ")
(if (leveldb:put db "+" +)
   (print "ok."))

;; SMART READ
(display "reading string value... ")
(define sread (leveldb:get db "a-string-key" string?))
(print "string value: " sread)

(display "reading integer value... ")
(define iread (leveldb:get db 1234567 integer?))
(print "integer value: " iread)

(display "reading internal object... ")
(define oread (leveldb:get db "+"))
(print "internal object: " oread)
(print "try to use loaded object: (oread 1 2 3): " (oread 1 2 3))

;; CLOSE
(leveldb_close db)

; done.
(print "Ok.")
