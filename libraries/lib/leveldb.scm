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

   ; Limitations:
   ;  * leveldb_get supports only 32-bit length data. todo.

(export
   make-void*
   ;;  make-sqlite3 make-sqlite3_stmt
   leveldb_t*
   ;; typedef struct leveldb_cache_t leveldb_cache_t;
   ;; typedef struct leveldb_comparator_t leveldb_comparator_t;
   ;; typedef struct leveldb_env_t leveldb_env_t;
   ;; typedef struct leveldb_filelock_t leveldb_filelock_t;
   ;; typedef struct leveldb_filterpolicy_t leveldb_filterpolicy_t;
   leveldb_iterator_t*
   ;; typedef struct leveldb_logger_t leveldb_logger_t;
   leveldb_options_t*
   ;; typedef struct leveldb_randomfile_t leveldb_randomfile_t;
   leveldb_readoptions_t*
   ;; typedef struct leveldb_seqfile_t leveldb_seqfile_t;
   leveldb_snapshot_t*
   ;; typedef struct leveldb_writablefile_t leveldb_writablefile_t;
   leveldb_writebatch_t*
   leveldb_writeoptions_t*

   leveldb_open
   leveldb_close

   leveldb_put
   leveldb_delete
   leveldb_write
   leveldb_get

   leveldb_create_iterator
   leveldb_create_snapshot
   leveldb_release_snapshot
   ;; leveldb_property_value
   ;; leveldb_approximate_sizes
   ;; leveldb_compact_range

   leveldb_destroy_db
   ;; leveldb_repair_db

   leveldb_iter_destroy
   leveldb_iter_valid
   leveldb_iter_seek_to_first
   leveldb_iter_seek_to_last
   leveldb_iter_seek
   leveldb_iter_next
   leveldb_iter_prev
   leveldb_iter_key
   leveldb_iter_value
   leveldb_iter_get_error

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
   leveldb_options_destroy
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

   leveldb_options_set_compression
      leveldb_no_compression
      leveldb_snappy_compression

   ;; leveldb_comparator_create
   ;; leveldb_comparator_destroy

   ;; ; filter policy
   ;; leveldb_filterpolicy_create
   ;; leveldb_filterpolicy_destroy
   ;; leveldb_filterpolicy_create_bloom

   ; read options
   leveldb_readoptions_create
   leveldb_readoptions_destroy
   leveldb_readoptions_set_verify_checksums
   leveldb_readoptions_set_fill_cache
   leveldb_readoptions_set_snapshot

   ; write options
   leveldb_writeoptions_create
   leveldb_writeoptions_destroy
   leveldb_writeoptions_set_sync

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
   ; -----------------------
   leveldb:open
   leveldb:close
   leveldb:make-options

   leveldb:get
   leveldb:get-string  ; todo
   leveldb:get-integer ; todo
   leveldb:make-readoptions
;   leveldb:get-boolean ;?
   leveldb:delete
   leveldb:put
   leveldb:make-writeoptions

   encode
   decode
)

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
   (define leveldb_iterator_t* fft-void*)
   ;; typedef struct leveldb_logger_t leveldb_logger_t;
   (define leveldb_options_t* fft-void*)
   ;; typedef struct leveldb_randomfile_t leveldb_randomfile_t;
   (define leveldb_readoptions_t* fft-void*)
   ;; typedef struct leveldb_seqfile_t leveldb_seqfile_t;
   (define leveldb_snapshot_t* fft-void*)
   ;; typedef struct leveldb_writablefile_t leveldb_writablefile_t;
   (define leveldb_writebatch_t* fft-void*)
   (define leveldb_writeoptions_t* fft-void*)

   ; * internal types
   (define void* fft-void*)
   (define int fft-int)
   (define char* type-string)
   (define char** fft-void**)
   (define void fft-void)
   (define size_t fft-size_t)

   (define uint8_t fft-uint8)
   ;(define bool fft-unsigned-char)

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
   (define leveldb_get (leveldb fft-void* "leveldb_get" leveldb_t* leveldb_readoptions_t* fft-any size_t (fft& fft-uint32) char**)) ; (fft& size_t)

   (define leveldb_create_iterator (leveldb leveldb_iterator_t* "leveldb_create_iterator" leveldb_t* leveldb_readoptions_t*))
   (define leveldb_create_snapshot (leveldb leveldb_snapshot_t* "leveldb_create_snapshot" leveldb_t*))
   (define leveldb_release_snapshot (leveldb void "leveldb_release_snapshot" leveldb_t* leveldb_snapshot_t*))
   ;; leveldb_property_value
   ;; leveldb_approximate_sizes
   ;; leveldb_compact_range

   (define leveldb_destroy_db (leveldb void "leveldb_destroy_db" leveldb_options_t* char* fft-void**))
   ;; leveldb_repair_db

   (define leveldb_iter_destroy (leveldb void "leveldb_iter_destroy" leveldb_iterator_t*))
   (define leveldb_iter_valid (leveldb fft-unsigned-char "leveldb_iter_valid" leveldb_iterator_t*))
   (define leveldb_iter_seek_to_first (leveldb void "leveldb_iter_seek_to_first" leveldb_iterator_t*))
   (define leveldb_iter_seek_to_last (leveldb void "leveldb_iter_seek_to_last" leveldb_iterator_t*))
   (define leveldb_iter_seek (leveldb void "leveldb_iter_seek" leveldb_iterator_t* fft-any size_t))
   (define leveldb_iter_next (leveldb void "leveldb_iter_next" leveldb_iterator_t*))
   (define leveldb_iter_prev (leveldb void "leveldb_iter_prev" leveldb_iterator_t*))
   (define leveldb_iter_key (leveldb fft-void* "leveldb_iter_key" leveldb_iterator_t* (fft& fft-uint32)))
   (define leveldb_iter_value (leveldb fft-void* "leveldb_iter_value" leveldb_iterator_t* (fft& fft-uint32)))
   (define leveldb_iter_get_error (leveldb void "leveldb_iter_get_error" leveldb_iterator_t* char**))

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
   (define leveldb_options_destroy (leveldb void "leveldb_options_destroy" leveldb_options_t*))
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

   (define leveldb_options_set_compression (leveldb void "leveldb_options_set_compression" leveldb_options_t* int))
      (define leveldb_no_compression 0)
      (define leveldb_snappy_compression 1)

   ;; leveldb_comparator_create
   ;; leveldb_comparator_destroy

   ;; ; filter policy
   ;; leveldb_filterpolicy_create
   ;; leveldb_filterpolicy_destroy
   ;; leveldb_filterpolicy_create_bloom

   ; read options
   (define leveldb_readoptions_create (leveldb leveldb_readoptions_t* "leveldb_readoptions_create"))
   (define leveldb_readoptions_destroy (leveldb void "leveldb_readoptions_destroy" leveldb_readoptions_t*))
   (define leveldb_readoptions_set_verify_checksums (leveldb void "leveldb_readoptions_set_verify_checksums" leveldb_readoptions_t* fft-unsigned-char))
   (define leveldb_readoptions_set_fill_cache (leveldb void "leveldb_readoptions_set_fill_cache" leveldb_readoptions_t* fft-unsigned-char))
   (define leveldb_readoptions_set_snapshot (leveldb void "leveldb_readoptions_set_snapshot" leveldb_readoptions_t* leveldb_snapshot_t*))

   ; write options
   (define leveldb_writeoptions_create (leveldb leveldb_writeoptions_t* "leveldb_writeoptions_create"))
   (define leveldb_writeoptions_destroy (leveldb void "leveldb_writeoptions_destroy" leveldb_writeoptions_t*))
   (define leveldb_writeoptions_set_sync (leveldb void "leveldb_writeoptions_set_sync" leveldb_writeoptions_t* fft-unsigned-char))

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

   (define (leveldb:make-options options)
      (define op (leveldb_options_create))
      ; apply options:
      (ff-fold (lambda (? key value)
                  (case key
                     ('create_if_missing
                        (leveldb_options_set_create_if_missing op (if value 1 0)))
                     ;'comparator
                     ;'filter_policy
                     ;'error_if_exists
                     ;'paranoid_checks
                     ;'env
                     ;'info_log
                     ;'write_buffer_size
                     ;'max_open_files
                     ;'cache
                     ;'block_size
                     ;'block_restart_interval
                     ;'max_file_size
                     ('compression ; leveldb_no_compression | leveldb_snappy_compression
                        (leveldb_options_set_compression op value))
                     (else
                        (print-to stderr "leveldb: unknown option " key " with value " value))))
         #f
         options)
      op)

   (define leveldb:open
      ; options can be ff or previously created options
      (define (leveldb:open filename options)
         (define op (if (ff? options)
            (leveldb:make-options options)
            options))
         
         (define err (make-vptr))
         (define database (leveldb_open op filename err))
         (if (ff? options) ; cleanup
            (leveldb_options_destroy op))

         (if (equal? err NULL)
            database
         else
            (print-to stderr "leveldb open error:\n   " (vptr->string err))
            (leveldb_free err)
            #false))
      (case-lambda
         ((filename)
            (leveldb:open filename {}))
         ((filename options)
            (leveldb:open filename options))))

   (define (leveldb:close database)
      (leveldb_close database)
      #true)


   ; * internal
   (define (encode arg)
      (cond
         ((and (integer? arg) (< arg #x10000000000000000)) ; 64-bit
            (list (cons (fft* fft-long-long) (box arg)) (sizeof fft-long-long)))
         ((string? arg)
            (define len (+ (string-length arg) 1)) ; +1 for ending '\0'
            (list (cons type-string arg) len))
         ((bytevector? arg)
            (define len (size arg))
            (list (cons fft-void* arg) len))
         (else
            (define bin (fasl-encode arg))
            (define len (length bin))
            (list (cons (fft* fft-char) bin) len))))

   (define (leveldb:make-writeoptions options)
      (define op (leveldb_writeoptions_create))
      ; apply options:
      (ff-fold (lambda (? key value)
                  (case key
                     ('sync
                        (leveldb_writeoptions_set_sync op (if value 1 0)))
                     (else
                        (print-to stderr "leveldb: unknown write option " key " with value " value))))
         #f
         options)
      op)

   ; put
   (define leveldb:put
      (define (leveldb:put database key value options)
         (define op (if (ff? options)
            (leveldb:make-writeoptions options)
            options))

         (define err (make-vptr))
         (apply leveldb_put (append
            (list database op)
            (encode key)
            (encode value)
            (list err)))
         (if (ff? options) ; cleanup
            (leveldb_writeoptions_destroy op))

         (if (equal? err NULL)
            #true
         else
            (print-to stderr "leveldb put error: " (vptr->string err))
            (leveldb_free err)
            #false))
      (case-lambda
         ((database key value)
            (leveldb:put database key value {}))
         ((database key value options)
            (leveldb:put database key value options))))

   ; delete
   (define (leveldb:delete database key)
      (define err (make-vptr))
      (apply leveldb_delete (append
         (list database (leveldb_writeoptions_create))
         (encode key)
         (list err)))
      (if (equal? err NULL)
         #true
      else
         (print-to stderr "leveldb delete error: " (vptr->string err))
         (leveldb_free err)
         #false))

   ; -------------------------------------------
   (define (leveldb:make-readoptions options)
      (define op (leveldb_readoptions_create))
      ; apply options:
      (ff-fold (lambda (? key value)
                  (case key
                     ('verify_checksums
                        (leveldb_readoptions_set_verify_checksums op (if value 1 0)))
                     ('fill_cache
                        (leveldb_readoptions_set_fill_cache op (if value 1 0)))
                     ('snapshot
                        (leveldb_readoptions_set_snapshot op value))
                     (else
                        (print-to stderr "leveldb: unknown read option " key " with value " value))))
         #f
         options)
      op)

   (define decode
      (case-lambda
         ((ptr len)
            (fasl-decode (bytevector->list (vptr->bytevector ptr len)) #false))
         ((ptr len returntype)
            (case returntype
               (string?   ; ignore string length, we assume that string is null-terminated
                  (vptr->string ptr))
               (integer?  ; we assume that integer is 64-bit width
                  (assert (eq? len 8))
                  (bytevector->int64 (vptr->bytevector ptr 8) 0))
               (bytevector?
                  ((vptr->bytevector ptr len)))
               (else
                  (fasl-decode (bytevector->list (vptr->bytevector ptr len)) #false))))))

   ; get
   (define leveldb:get
      (define (leveldb:get database key options returntype)
         (define op (if (ff? options)
            (leveldb:make-readoptions options)
            options))

         (define err (make-vptr))
         (define len (box 0))
         (define out
            (apply leveldb_get (append
               (list database op)
               (encode key)
               (list len err))))
         (if (ff? options) ; cleanup
            (leveldb_readoptions_destroy op))

         (if (equal? err NULL)
         then
            (when (> (unbox len) 0)
               (decode out (unbox len) returntype))
         else
            (print-to stderr "leveldb get error: " (vptr->string err))
            (leveldb_free err)
            #false))
      (case-lambda
         ((database key)
            (leveldb:get database key {} #false))
         ((database key opt/type)
            (if (or (ff? opt/type) (eq? (type opt/type) type-vptr))
               (leveldb:get database key opt/type #false)
               (leveldb:get database key {} opt/type)))
         ((database key opt/type type/opt)
            (if (or (ff? opt/type) (eq? (type opt/type) type-vptr))
               (leveldb:get database key opt/type type/opt)
               (leveldb:get database key type/opt opt/type)))))

   (define (leveldb:get-string database key)
      (leveldb:get database key string?))

   (define (leveldb:get-integer database key)
      (leveldb:get database key integer?))

))
