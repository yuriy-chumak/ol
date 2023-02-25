(define-library (lib gtk-3 gtk)
   (export
      GTK3

      GtkCallback
      GTK_CALLBACK

      ; 
      GtkOrientation
      GTK_ORIENTATION_HORIZONTAL
      GTK_ORIENTATION_VERTICAL

      GType


      (exports (lib glib-2)))
   (import
      (scheme core)
      (otus ffi)
      (lib glib-2))

(begin
   (define GTK3 (load-dynamic-library "libgtk-3.so"))

   (define GtkOrientation gint)
   (define GTK_ORIENTATION_HORIZONTAL 0)
   (define GTK_ORIENTATION_VERTICAL 1)

   (define GType gint)

   (define GtkCallback type-callable)
   (define-syntax GTK_CALLBACK
      (syntax-rules ()
         ((GTK_CALLBACK (userdata) . rest)
            (vm:pin (cons
               (cons gint (list gpointer))
               (lambda (userdata)
                  .rest))))
         ((GTK_CALLBACK (object userdata) . rest)
            (vm:pin (cons
               (cons gint (list GObject* gpointer))
               (lambda (object userdata)
                  .rest))))
         ((GTK_CALLBACK (object arg1 userdata) . rest)
            (vm:pin (cons
               (cons gint (list GObject* GObject* gpointer))
               (lambda (object arg1 userdata)
                  .rest))))
      ))

))