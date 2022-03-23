#!/usr/bin/env ol
(import (otus ffi))
(import (lib leveldb))

(print "leveldb_major_version: " (leveldb_major_version))
(print "leveldb_minor_version: " (leveldb_minor_version))

;; OPEN
(define options (leveldb_options_create))
(leveldb_options_set_create_if_missing options 1)
(define err (make-vptr))
(define db (leveldb_open options "testdb" err))
(unless (equal? err NULL)
   (runtime-error "Open fail." err))

;; reset error var
(leveldb_free err)

(define woptions (leveldb_writeoptions_create))

;; SMART WRITE
(unless (leveldb:put db "a-string-key" "a-string-value")
   (runtime-error "Write strings fail." err))

(unless (leveldb:put db 1234567 7654321)
   (runtime-error "Write numbers fail." err))

;; SMART READ
(define sread (leveldb:get db "a-string-key" string?))
(if sread
   (print "value: " sread)
else
   (runtime-error "Read strings fail." err))

(define iread (leveldb:get db 1234567 integer?))
(if iread
   (print "value: " iread)
else
   (runtime-error "Read integer fail." err))

;; SMART DELETE
(unless (leveldb:delete db "the-key")
   (runtime-error "Delete string fail." err))
(unless (leveldb:delete db "the-key")
   (runtime-error "Delete integer fail." err))

;; CLOSE
(leveldb_close db)

;; DESTROY
(leveldb_destroy_db options "testdb" err)
(unless (equal? err NULL)
   (runtime-error "Read fail." err))
(leveldb_free err)

; done.
(print "Ok.")
