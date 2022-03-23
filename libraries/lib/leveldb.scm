;;; LevelDB interface for Otus Lisp
;;; https://github.com/yuriy-chumak/ol
;;; https://github.com/google/leveldb

;;; Copyright (c) 2022 Yuriy Chumak
;;; All rights reserved.
;;;
;;; --------------------------------------------------------------
;;; This program is free software;  you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; --------------------------------------------------------------
(define-library (lib leveldb)
   (version 1.0)
   (license MIT/LGPL3)
   (description "otus-lisp leveldb interface")

(export
   make-void*
   ;;  make-sqlite3 make-sqlite3_stmt
   leveldb_t*
   ;; typedef struct leveldb_cache_t leveldb_cache_t;
   ;; typedef struct leveldb_comparator_t leveldb_comparator_t;
   ;; typedef struct leveldb_env_t leveldb_env_t;
   ;; typedef struct leveldb_filelock_t leveldb_filelock_t;
   ;; typedef struct leveldb_filterpolicy_t leveldb_filterpolicy_t;
   ;; typedef struct leveldb_iterator_t leveldb_iterator_t;
   ;; typedef struct leveldb_logger_t leveldb_logger_t;
   leveldb_options_t*
   ;; typedef struct leveldb_randomfile_t leveldb_randomfile_t;
   leveldb_readoptions_t*
   ;; typedef struct leveldb_seqfile_t leveldb_seqfile_t;
   ;; typedef struct leveldb_snapshot_t leveldb_snapshot_t;
   ;; typedef struct leveldb_writablefile_t leveldb_writablefile_t;
   leveldb_writebatch_t*
   leveldb_writeoptions_t*

   leveldb_open
   leveldb_close

   leveldb_put
   leveldb_delete
   leveldb_write
   leveldb_get

   ;; leveldb_create_iterator
   ;; leveldb_create_snapshot
   ;; leveldb_release_snapshot
   ;; leveldb_property_value
   ;; leveldb_approximate_sizes
   ;; leveldb_compact_range

   leveldb_destroy_db
   ;; leveldb_repair_db

   ;; leveldb_iter_destroy
   ;; leveldb_iter_valid
   ;; leveldb_iter_seek_to_first
   ;; leveldb_iter_seek_to_last
   ;; leveldb_iter_seek
   ;; leveldb_iter_next
   ;; leveldb_iter_prev
   ;; leveldb_iter_key
   ;; leveldb_iter_value
   ;; leveldb_iter_get_error

   ;; ; writebatch
   ;; leveldb_writebatch_create
   ;; leveldb_writebatch_destroy
   ;; leveldb_writebatch_clear
   ;; leveldb_writebatch_put
   ;; leveldb_writebatch_delete
   ;; leveldb_writebatch_iterate
   ;; leveldb_writebatch_append

   ; options
   leveldb_options_create
   ;; leveldb_options_destroy
   ;; leveldb_options_set_comparator
   ;; leveldb_options_set_filter_policy
   leveldb_options_set_create_if_missing
   ;; leveldb_options_set_error_if_exists
   ;; leveldb_options_set_paranoid_checks
   ;; leveldb_options_set_env
   ;; leveldb_options_set_info_log
   ;; leveldb_options_set_write_buffer_size
   ;; leveldb_options_set_max_open_files
   ;; leveldb_options_set_cache
   ;; leveldb_options_set_block_size
   ;; leveldb_options_set_block_restart_interval
   ;; leveldb_options_set_max_file_size

   ;; leveldb_options_set_compression
   ;;    leveldb_no_compression
   ;;    leveldb_snappy_compression

   ;; leveldb_comparator_create
   ;; leveldb_comparator_destroy

   ;; ; filter policy
   ;; leveldb_filterpolicy_create
   ;; leveldb_filterpolicy_destroy
   ;; leveldb_filterpolicy_create_bloom

   ; read options
   leveldb_readoptions_create
   ;; leveldb_readoptions_destroy
   ;; leveldb_readoptions_set_verify_checksums
   ;; leveldb_readoptions_set_fill_cache
   ;; leveldb_readoptions_set_snapshot

   ; write options
   leveldb_writeoptions_create
   ;; leveldb_writeoptions_destroy
   ;; leveldb_writeoptions_set_sync

   ;; ; cache
   ;; leveldb_cache_create_lru
   ;; leveldb_cache_destroy

   ;; ; env
   ;; leveldb_create_default_env
   ;; leveldb_env_destroy

   ;; leveldb_env_get_test_directory

   ; utility
   leveldb_free
   leveldb_major_version
   leveldb_minor_version
   
   ; smart interface
   leveldb:get
   leveldb:delete
   leveldb:put)

; ============================================================================
(import
   (otus lisp)
   (otus ffi))

(cond-expand
   ((or Linux Android)
      (begin
         (define leveldb (load-dynamic-library "libleveldb.so"))))
   (Windows
      (begin
         (define leveldb (load-dynamic-library "leveldb.dll"))))
   (else
      (begin
         (runtime-error "leveldb: unknown platform" (uname)))))

(begin
   (define (make-void*) (vm:cast 0 type-vptr))

   (if (not leveldb)
      (runtime-error "LevelDB library not found. Please, install one." #null))

   ; Types
   (define leveldb_t* fft-void*)
   ;; typedef struct leveldb_cache_t leveldb_cache_t;
   ;; typedef struct leveldb_comparator_t leveldb_comparator_t;
   ;; typedef struct leveldb_env_t leveldb_env_t;
   ;; typedef struct leveldb_filelock_t leveldb_filelock_t;
   ;; typedef struct leveldb_filterpolicy_t leveldb_filterpolicy_t;
   ;; typedef struct leveldb_iterator_t leveldb_iterator_t;
   ;; typedef struct leveldb_logger_t leveldb_logger_t;
   (define leveldb_options_t* fft-void*)
   ;; typedef struct leveldb_randomfile_t leveldb_randomfile_t;
   (define leveldb_readoptions_t* fft-void*)
   ;; typedef struct leveldb_seqfile_t leveldb_seqfile_t;
   ;; typedef struct leveldb_snapshot_t leveldb_snapshot_t;
   ;; typedef struct leveldb_writablefile_t leveldb_writablefile_t;
   (define leveldb_writebatch_t* fft-void*)
   (define leveldb_writeoptions_t* fft-void*)

   ; * internal types
   (define void* fft-void*)
   (define int fft-int)
   (define char* type-string)
   (define char** fft-void**)
   (define void fft-void)
   (define size_t fft-size-t)

   (define uint8_t fft-uint8)

   ; -----------------------------------------------

   ; Run-Time Library Version Numbers
   (define leveldb_open (leveldb leveldb_t* "leveldb_open" leveldb_options_t* char* char**))
   (define leveldb_close (leveldb void "leveldb_close" leveldb_t*))

   (define leveldb_free (leveldb void "leveldb_free" void*))
   (define leveldb_major_version (leveldb int "leveldb_major_version"))
   (define leveldb_minor_version (leveldb int "leveldb_minor_version"))

   (define leveldb_put (leveldb void "leveldb_put" leveldb_t* leveldb_writeoptions_t* fft-any size_t fft-any size_t char**))
   (define leveldb_delete (leveldb void "leveldb_delete" leveldb_t* leveldb_writeoptions_t* fft-any size_t char**))
   (define leveldb_write (leveldb void "leveldb_write" leveldb_t* leveldb_writeoptions_t* leveldb_writebatch_t* char**))
   (define leveldb_get (leveldb fft-void* "leveldb_get" leveldb_t* leveldb_readoptions_t* fft-any size_t (fft* size_t) char**))

   ;; leveldb_create_iterator
   ;; leveldb_create_snapshot
   ;; leveldb_release_snapshot
   ;; leveldb_property_value
   ;; leveldb_approximate_sizes
   ;; leveldb_compact_range

   (define leveldb_destroy_db (leveldb void "leveldb_destroy_db" leveldb_options_t* char* fft-void**))
   ;; leveldb_repair_db

   ;; leveldb_iter_destroy
   ;; leveldb_iter_valid
   ;; leveldb_iter_seek_to_first
   ;; leveldb_iter_seek_to_last
   ;; leveldb_iter_seek
   ;; leveldb_iter_next
   ;; leveldb_iter_prev
   ;; leveldb_iter_key
   ;; leveldb_iter_value
   ;; leveldb_iter_get_error

   ;; ; writebatch
   ;; leveldb_writebatch_create
   ;; leveldb_writebatch_destroy
   ;; leveldb_writebatch_clear
   ;; leveldb_writebatch_put
   ;; leveldb_writebatch_delete
   ;; leveldb_writebatch_iterate
   ;; leveldb_writebatch_append

   ; options
   (define leveldb_options_create (leveldb leveldb_options_t* "leveldb_options_create"))
   ;; leveldb_options_destroy
   ;; leveldb_options_set_comparator
   ;; leveldb_options_set_filter_policy
   (define leveldb_options_set_create_if_missing (leveldb void "leveldb_options_set_create_if_missing" leveldb_options_t* uint8_t))
   ;; leveldb_options_set_error_if_exists
   ;; leveldb_options_set_paranoid_checks
   ;; leveldb_options_set_env
   ;; leveldb_options_set_info_log
   ;; leveldb_options_set_write_buffer_size
   ;; leveldb_options_set_max_open_files
   ;; leveldb_options_set_cache
   ;; leveldb_options_set_block_size
   ;; leveldb_options_set_block_restart_interval
   ;; leveldb_options_set_max_file_size

   ;; leveldb_options_set_compression
   ;;    leveldb_no_compression
   ;;    leveldb_snappy_compression

   ;; leveldb_comparator_create
   ;; leveldb_comparator_destroy

   ;; ; filter policy
   ;; leveldb_filterpolicy_create
   ;; leveldb_filterpolicy_destroy
   ;; leveldb_filterpolicy_create_bloom

   ; read options
   (define leveldb_readoptions_create (leveldb leveldb_readoptions_t* "leveldb_readoptions_create"))
   ;; leveldb_readoptions_destroy
   ;; leveldb_readoptions_set_verify_checksums
   ;; leveldb_readoptions_set_fill_cache
   ;; leveldb_readoptions_set_snapshot

   ; write options
   (define leveldb_writeoptions_create (leveldb leveldb_writeoptions_t* "leveldb_writeoptions_create"))
   ;; leveldb_writeoptions_destroy
   ;; leveldb_writeoptions_set_sync

   ;; ; cache
   ;; leveldb_cache_create_lru
   ;; leveldb_cache_destroy

   ;; ; env
   ;; leveldb_create_default_env
   ;; leveldb_env_destroy

   ;; leveldb_env_get_test_directory
)

;; High-level Interface
(begin
   (define (encode-arg arg)
      (cond
         ((integer? arg)
            (if (> arg #xFFFFFFFF)
               (runtime-error "Too big numeric key" arg))
            (list (cons (fft* fft-long-long) (box arg)) 8))
         ((string? arg)
            (define len (+ (string-length arg) 1)) ; +1 for ending '\0'
            (list (cons type-string arg) len))
         (else
            (runtime-error "Unsupported key type" arg))))

   (define (leveldb:put database key value)
      (define err (make-vptr))
      (apply leveldb_put (append
         (list database (leveldb_writeoptions_create))
         (encode-arg key)
         (encode-arg value)
         (list err)))
      (define ok (equal? err NULL))
      (leveldb_free err)
      ok)

   (define (leveldb:delete database key)
      (define err (make-vptr))
      (apply leveldb_delete (append
         (list database (leveldb_writeoptions_create))
         (encode-arg key)
         (list err)))
      (define ok (equal? err NULL))
      (leveldb_free err)
      ok)

   (define (leveldb:get database key key-type)
      (define err (make-vptr))
      (define len (box 0))
      (define out
      (apply leveldb_get (append
         (list database (leveldb_readoptions_create))
         (encode-arg key)
         (list len err))))
      (define ok
         (if (equal? err NULL)
            (case key-type
               (string?   ; ignore string length, we assume that string is null-terminated
                  (vptr->string out))
               (integer?
                  (bytevector->int64 (vptr->bytevector out 8) 0)))))

      (leveldb_free err)
      ok)
))
