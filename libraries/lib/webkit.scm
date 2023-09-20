(define-library (lib webkit)
   (export

      webkit_web_view_new
      webkit_web_view_load_uri
   )

   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 widget))

(begin
   (define WEBKIT (or
      (load-dynamic-library "libwebkit2gtk-4.0.so")
      (load-dynamic-library "libwebkit2gtk-4.0.so.37")))

   (define WebKitWebView* type-vptr)

   (define webkit_web_view_new (WEBKIT GtkWidget* "webkit_web_view_new"))
   (define webkit_web_view_load_uri (WEBKIT fft-void "webkit_web_view_load_uri" WebKitWebView* type-string))
))
