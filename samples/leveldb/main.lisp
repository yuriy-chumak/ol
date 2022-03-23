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

;; WRITE
(define woptions (leveldb_writeoptions_create))
(leveldb_put db woptions
   (cons type-string "the-key") 7
   (cons type-string "some-value") 11 err) ; 11 with completive '\0'
(unless (equal? err NULL)
   (runtime-error "Write fail." err))
(leveldb_free err)

;; READ
(define roptions (leveldb_readoptions_create))
(define read_len (box 0))
(define read (leveldb_get db roptions 
   (cons type-string "the-key") 7
   read_len err))
(unless (equal? err NULL)
   (runtime-error "Read fail." err))
(leveldb_free err)

(print "value: " (vptr->string read))


;; DELETE
(leveldb_delete db woptions
   (cons type-string "the-key") 7
   err)
(unless (equal? err NULL)
   (runtime-error "Read fail." err))
(leveldb_free err)

;; CLOSE
(leveldb_close db)

;; DESTROY
(leveldb_destroy_db options "testdb" err)
(unless (equal? err NULL)
   (runtime-error "Read fail." err))
(leveldb_free err)

; done.
(print "Ok.")
