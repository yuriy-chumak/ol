(define-library (lib glib-2)
   (export
      void
      gint guint gulong gsize gpointer
      gdouble
      gchar* gboolean

      TRUE FALSE NULL
      g_free

      G_APPLICATION_FLAGS_NONE

      GCallback
      GClosureNotify
      GConnectFlags
      GApplication*
      GError*
      g_error_free

      ;
      GObject*
      g_object_ref
      g_object_unref

      GValue*
      make-GValue
      g_value_init
      g_value_get_gtype
      g_value_set_char
      g_value_get_char
      g_value_set_int64
      g_value_get_int64

      GDateTime*
      g_date_time_new_from_unix_utc
      g_date_time_format
      g_date_time_unref

      GType

      G_CALLBACK
      G_TYPE_GTYPE

      g_signal_connect
      g_application_run
      g_application_quit
   )
   (import
      (scheme core)
      (owl string)
      (otus ffi))

(begin
   (define TRUE 1)
   (define FALSE 0)
   (define NULL (vm:cast 0 fft-void*))

   (define G_APPLICATION_FLAGS_NONE 0)

   (define gint fft-int)
   (define guint fft-unsigned-int)
   (define gulong fft-unsigned-long)
   (define gpointer fft-void*)
   (define gdouble fft-double)
   (define gchar fft-char)
   (define gchar* type-string)
   (define gsize fft-unsigned-int)
   (define gboolean gint)
   (define gint64 fft-signed-long-long)
   (define void fft-void)

   (define GCallback type-callable) ; void (*GCallback)(void)
   (define GClosureNotify type-callable) ; void (*GClosureNotify)(gpointer, GClosure)
   (define GConnectFlags fft-int) ; enum
   (define GApplication* fft-void*)

   (define G_CALLBACK make-callback)
)
(cond-expand
   (Linux
      (begin
         (define GLIB (or
            (load-dynamic-library "libglib-2.0.so") ))
         (define GOBJECT (or
            (load-dynamic-library "libgobject-2.0.so")
            (load-dynamic-library "libgobject-2.0.so.0") ))
         (define GIO (or
            (load-dynamic-library "libgio-2.0.so")
            (load-dynamic-library "libgio-2.0.so.0") ))
      ))
   (Windows
      (begin
         (define GLIB
            (load-dynamic-library "libglib-2.0-0.dll"))
         (unless GLIB (runtime-error "Can't load libglib-2.0-0.dll"
            "try installing dlls from https://github.com/yuriy-chumak/libol-gtk-3/releases/"))
         (define GOBJECT
            (load-dynamic-library "libgobject-2.0-0.dll"))
         (unless GOBJECT (runtime-error "Can't load libgobject-2.0-0.dll"
            "try installing dlls from https://github.com/yuriy-chumak/libol-gtk-3/releases/"))
         (define GIO
            (load-dynamic-library "libgio-2.0-0.dll"))
         (unless GIO (runtime-error "Can't load libgio-2.0-0.dll"
            "try installing dlls from https://github.com/yuriy-chumak/libol-gtk-3/releases/"))
      ))
)
; common part
(begin

(define g_free (GLIB void "g_free" gpointer))

(define GObject* type-vptr)
(define g_object_ref (GOBJECT gpointer "g_object_ref" gpointer))
(define g_object_unref (GOBJECT void "g_object_unref" gpointer))

(define GError* type-vptr)
(define g_error_free (GOBJECT void "g_error_free" GError*))

(define g_signal_connect_data (GOBJECT gulong "g_signal_connect_data" gpointer type-string GCallback gpointer GClosureNotify GConnectFlags))
(define (g_signal_connect instance detailed_signal c_handler data)
   (g_signal_connect_data instance detailed_signal c_handler data #false 0))
(define g_application_run (GIO gint "g_application_run" GApplication* gint (fft* fft-void*)))
(define g_application_quit (GIO void "g_application_quit" GApplication*))

(define GDateTime* type-vptr)
(define g_date_time_new_from_unix_utc (GLIB GDateTime* "g_date_time_new_from_unix_utc" gint64))
(define g_date_time_format (GLIB type-vptr "g_date_time_format" GDateTime* gchar*))
(define g_date_time_unref (GLIB void "g_date_time_unref" GDateTime*))

; --=( GType )=---------------
(define GType fft-unsigned-long)
(setq G_TYPE_FUNDAMENTAL_SHIFT 2)
(setq G_TYPE_MAKE_FUNDAMENTAL (lambda (x)
   (let* ((hi lo (vm:shl x G_TYPE_FUNDAMENTAL_SHIFT)))
      lo)))
;...
(setq G_TYPE_INT64 (G_TYPE_MAKE_FUNDAMENTAL 10))
;...
(setq G_TYPE_STRING (G_TYPE_MAKE_FUNDAMENTAL 16))
;...

(define G_TYPE_GTYPE ((GOBJECT GType "g_gtype_get_type"))) ; GIO?


; --=( GValue )=---------

(define GValue* type-vptr)
(define g_value_init (GIO GValue* "g_value_init" GValue* GType))

(define g_value_get_gtype (GIO GType "g_value_get_gtype" GValue*))
; gchar
(define g_value_set_char (GIO void "g_value_set_char" GValue* gchar))
(define g_value_get_char (GIO gchar "g_value_get_char" GValue*))
; gint64
(define g_value_set_int64 (GIO void "g_value_set_int64" GValue* gint64))
(define g_value_get_int64 (GIO gint64 "g_value_get_int64" GValue*))
; strings
(define g_value_set_string (GIO void "g_value_set_string" GValue* gchar*))
(define g_value_get_string (GIO gchar* "g_value_get_string" GValue*))

(define make-GValue
   (define (make) (make-bytevector 24)) ; GType(8) + gdouble(8)
   (case-lambda
      (()   (make))
      ((x)  (let ((v (make)))
               (cond
                  ((integer? x)
                     (g_value_init v G_TYPE_INT64)
                     (g_value_set_int64 v x))
                  ((string? x)
                     (g_value_init v G_TYPE_STRING)
                     (g_value_set_string v x))
                  (else #f))
               v))))

))