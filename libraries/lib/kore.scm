(define-library (lib kore)
   (version 1.0)
   (license MIT+LGPL3)
   (description
      "kore.io ol ffi interface")

(export
   kore
   
   KORE_RESULT_ERROR
   KORE_RESULT_OK
   KORE_RESULT_RETRY

   http_request*
   kore_buf*

   http_populate_get
   http_response

   http_argument_get
   http_argument_get_byte
   http_argument_get_int16
   http_argument_get_int32

   kore-constructor!
)

(import
   (scheme core) (owl io)
   (otus ffi))

(begin

   (vm:set! ffi 0 (make-vptr) 0 8)

   (define (kore type name . prototype)
      (let ((rtti (cons type prototype))
            (function (make-vptr)))
         (lambda args
            (ffi function rtti args))))

   (define KORE_RESULT_ERROR	0)
   (define KORE_RESULT_OK		1)
   (define KORE_RESULT_RETRY	2)

   (define HTTP_ARG_TYPE_BYTE   1)
   (define HTTP_ARG_TYPE_INT16  2)
   (define HTTP_ARG_TYPE_UINT16 3)
   (define HTTP_ARG_TYPE_INT32  4)
   (define HTTP_ARG_TYPE_UINT32 5)
   (define HTTP_ARG_TYPE_STRING 6)
   (define HTTP_ARG_TYPE_INT64  7)
   (define HTTP_ARG_TYPE_UINT64 8)
   (define HTTP_ARG_TYPE_FLOAT  9)
   (define HTTP_ARG_TYPE_DOUBLE 10)

   (define http_request* type-vptr)
   (define kore_buf* type-vptr)

   (define http_populate_get (kore fft-void "http_populate_qs" http_request*))
   ;; (http_populate_get req)

   (define http_response     (kore fft-void "http_response"    http_request* fft-int fft-void* fft-int))


   ; get
   (define http_argument_get (kore fft-int "http_argument_get" http_request* type-string fft-void* fft-any fft-int))
   
   (define (http_argument_get_byte req name out)
      (http_argument_get req name #F (cons fft-int8& out) HTTP_ARG_TYPE_BYTE))
   (define (http_argument_get_int16 req name out)
      (http_argument_get req name #F (cons fft-int16& out) HTTP_ARG_TYPE_INT16))
   (define (http_argument_get_int32 req name out)
      (http_argument_get req name #F (cons fft-int32& out) HTTP_ARG_TYPE_INT32))

   ;; ;; http_argument_get_uint16
   ;; ;; http_argument_get_int32
   ;; ;; http_argument_get_uint32
   ;; ;; http_argument_get_int64
   ;; ;; http_argument_get_uint64
   ;; ;; http_argument_get_float
   ;; ;; http_argument_get_double





   ;; (define kore_mem_init (kore fft-void "kore_mem_init"))
   ;; (define kore_msg_init (kore fft-void "kore_msg_init"))
   ;; (define kore_log_init (kore fft-void "kore_log_init"))


	;; (define kore_platform_init (kore fft-void "kore_platform_init"))
	;; (define http_parent_init (kore fft-void "http_parent_init"))
	;; (define kore_curl_sysinit (kore fft-void "kore_curl_sysinit"))
	;; (define kore_pgsql_sys_init (kore fft-void "kore_pgsql_sys_init"))
	;; (define kore_auth_init (kore fft-void "kore_auth_init"))
	;; (define kore_validator_init (kore fft-void "kore_validator_init"))
	;; (define kore_filemap_init (kore fft-void "kore_filemap_init"))
	;; (define kore_acme_init (kore fft-void "kore_acme_init"))
	;; (define kore_domain_init (kore fft-void "kore_domain_init"))
	;; (define kore_module_init (kore fft-void "kore_module_init"))
	;; (define kore_tls_init (kore fft-void "kore_tls_init"))


   (define kore-constructor!
      (vm:new 63 (lambda (args)
         (print "constructor")
         (define kore (dlopen #false))
         (define svptr (size nullptr))

         (vm:set! ffi 0 (dlsym kore "OLVM_ffi") 0 svptr)

         (define (set! function name)
            (vm:set! (ref function 3) 0 (dlsym kore name) 0 svptr))

         (set! http_populate_get "http_populate_qs")
         (set! http_response "http_response")
         (set! http_argument_get "http_argument_get")

         #T)))
))
