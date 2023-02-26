(define-library (lib gdk-3)
   (export
      GSourceFunc
      gdk_threads_add_idle
      gdk_threads_add_timeout

      GdkGLContext*
   )
   (import
      (scheme core)
      (otus ffi)
      (lib glib-2))

(begin
   (define GDK (load-dynamic-library "libgdk-3.so"))

   (define GSourceFunc type-callable)

   (define gdk_threads_add_idle (GDK guint "gdk_threads_add_idle" GSourceFunc gpointer))
   (define gdk_threads_add_timeout (GDK guint "gdk_threads_add_timeout" guint GSourceFunc gpointer))

   (define GdkGLContext* fft-void*)
))