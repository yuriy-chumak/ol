(define-library (lib gtk-3 application)
   (export
      GtkApplication*

   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkApplication* type-vptr)

   (define (GtkApplication props)
      ;...
      #false)
))
