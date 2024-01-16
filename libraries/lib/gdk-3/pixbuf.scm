(define-library (lib gdk-3 pixbuf)
   (export
      GdkPixbuf*
      GdkPixbufLoader*

      gdk_pixbuf_loader_new
      gdk_pixbuf_loader_write
      gdk_pixbuf_loader_close
      gdk_pixbuf_loader_get_pixbuf
      gdk_pixbuf_flip
      gdk_pixbuf_scale_simple
   )
   (import
      (scheme core)
      (otus ffi)
      (lib glib-2))

(cond-expand
   (Linux
      (begin
         (define PIXBUF (or
            (load-dynamic-library "libgdk-3.so")
            (load-dynamic-library "libgdk-3.so.0") ))
      ))
   (Windows
      (begin
         (define PIXBUF (load-dynamic-library "libgdk_pixbuf-2.0-0.dll"))
         (unless PIXBUF (runtime-error "Can't load libgdk_pixbuf-2.0-0.dll"))
      )) )

(begin
   (define GdkPixbuf* type-vptr)
   (define GdkPixbufLoader* type-vptr)

   (define gdk_pixbuf_loader_new (PIXBUF GdkPixbufLoader* "gdk_pixbuf_loader_new"))
   (define gdk_pixbuf_loader_write (PIXBUF gboolean "gdk_pixbuf_loader_write" GdkPixbufLoader* type-bytevector gsize (fft& GError*)))
   (define gdk_pixbuf_loader_close (PIXBUF gboolean "gdk_pixbuf_loader_close" GdkPixbufLoader* (fft& GError*)))
   (define gdk_pixbuf_loader_get_pixbuf (PIXBUF GdkPixbuf* "gdk_pixbuf_loader_get_pixbuf" GdkPixbufLoader*))

   (define GdkInterpType gint)
   ; GDK_INTERP_NEAREST 0
   ; GDK_INTERP_TILES 1
   ; GDK_INTERP_BILINEAR 2
   ; GDK_INTERP_HYPER 3
   (define gdk_pixbuf_flip (PIXBUF GdkPixbuf* "gdk_pixbuf_flip" GdkPixbuf* gboolean))
   (define gdk_pixbuf_scale_simple (PIXBUF GdkPixbuf* "gdk_pixbuf_scale_simple" GdkPixbuf* gint gint GdkInterpType))

))