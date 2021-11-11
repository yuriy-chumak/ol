(define-library (lib glib-2)
   (export
      gint guint gulong gpointer
      gdouble
      gchar* gboolean

      TRUE FALSE NULL

      G_APPLICATION_FLAGS_NONE

      GCallback
      GClosureNotify
      GConnectFlags
      GApplication*
      GError*
      g_error_free

      ;
      GObject*
      g_object_unref

      G_CALLBACK

      g_signal_connect
      g_application_run
      g_application_quit
   )
   (import
      (scheme core)
      (otus ffi))

(begin
(define TRUE 1)
(define FALSE 0)
(define NULL (vm:make type-vptr 0))

(define G_APPLICATION_FLAGS_NONE 0)

(define gint fft-int)
(define guint fft-unsigned-int)
(define gulong fft-unsigned-long)
(define gpointer fft-void*)
(define gdouble fft-double)
(define gchar* type-string)
(define gboolean gint)
(define void fft-void)

(define GCallback type-callable) ; void (*GCallback)(void)
(define GClosureNotify type-callable) ; void (*GClosureNotify)(gpointer, GClosure)
(define GConnectFlags fft-int) ; enum
(define GApplication* fft-void*)

(define G_CALLBACK make-callback)

(define GLIB (load-dynamic-library "libglib-2.0.so"))
(define GOBJECT (load-dynamic-library "libgobject-2.0.so"))
(define GIO (load-dynamic-library "libgio-2.0.so"))

(define GObject* fft-void*)
(define g_object_unref (GOBJECT void "g_object_unref" gpointer))

(define GError* fft-void*)
(define g_error_free (GOBJECT void "g_error_free" GError*))

(define g_signal_connect_data (GOBJECT gulong "g_signal_connect_data" gpointer type-string GCallback gpointer GClosureNotify GConnectFlags))
(define (g_signal_connect instance detailed_signal c_handler data)
   (g_signal_connect_data instance detailed_signal c_handler data #false 0))
(define g_application_run (GIO gint "g_application_run" GApplication* gint fft-void*))
(define g_application_quit (GIO void "g_application_quit" GApplication*))

))