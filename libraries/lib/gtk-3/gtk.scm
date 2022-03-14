(define-library (lib gtk-3 gtk)
   (export
      GTK3

      GtkCallback
      GTK_CALLBACK

      ; 
      GtkOrientation
      GTK_ORIENTATION_HORIZONTAL
      GTK_ORIENTATION_VERTICAL

      GtkMessageType
      GTK_MESSAGE_INFO
      GTK_MESSAGE_WARNING
      GTK_MESSAGE_QUESTION
      GTK_MESSAGE_ERROR
      GTK_MESSAGE_OTHER

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

   (define GtkMessageType gint)
   (define GTK_MESSAGE_INFO 0)
   (define GTK_MESSAGE_WARNING 1)
   (define GTK_MESSAGE_QUESTION 2)
   (define GTK_MESSAGE_ERROR 3)
   (define GTK_MESSAGE_OTHER 4)

   (define GType gint)

   (define GtkCallback type-callable)
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