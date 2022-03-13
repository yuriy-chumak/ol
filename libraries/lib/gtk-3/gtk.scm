(define-library (lib gtk-3 gtk)
   (export
      GTK3
      GTK_CALLBACK

      (exports (lib glib-2)))
   (import
      (scheme core)
      (otus ffi)
      (lib glib-2))

(begin
   (define GTK3 (load-dynamic-library "libgtk-3.so"))

   (define-syntax GTK_CALLBACK
      (syntax-rules ()
         ((GTK_CALLBACK (userdata) . rest)
            (vm:pin (cons
               (cons gint (list gpointer))
               (lambda (userdata)
                  .rest))))
         ((GTK_CALLBACK (app userdata) . rest)
            (vm:pin (cons
               (cons gint (list GObject* gpointer))
               (lambda (app userdata)
                  .rest))))))

))