(define-library (lib gtk-3 gtk)
   (export
      GTK3

      (exports (lib glib-2)))
   (import
      (scheme core)
      (otus ffi)
      (lib glib-2))

(begin
   (define GTK3 (load-dynamic-library "libgtk-3.so"))

))