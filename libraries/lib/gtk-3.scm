(define-library (lib gtk-3)
   (export
      make-GtkTextIter
      GtkTextIter*

      ; 
      GtkTextView*

      ; todo: move to "stock"
      GTK_STOCK_CANCEL
      GTK_RESPONSE_CANCEL
      GTK_STOCK_OPEN
      GTK_RESPONSE_ACCEPT

      (exports (lib gtk-3 gtk))
      (exports (lib gtk-3 application))

      (exports (lib gtk-3 widget))
      (exports (lib gtk-3 window))
      (exports (lib gtk-3 container))
      (exports (lib gtk-3 box))
      (exports (lib gtk-3 bbox))
      (exports (lib gtk-3 adjustment))

      (exports (lib gtk-3 label))
      (exports (lib gtk-3 button))
      (exports (lib gtk-3 list-store))
      (exports (lib gtk-3 file-chooser))
      (exports (lib gtk-3 file-chooser-dialog))
      (exports (lib gtk-3 builder)))

   (import
      (scheme core)
      (otus ffi)
      (lib glib-2)
      ;; (lib cairo)

      (lib gtk-3 gtk)
      (lib gtk-3 application)

      (lib gtk-3 widget)
      (lib gtk-3 window)
      (lib gtk-3 container)
      (lib gtk-3 box)
      (lib gtk-3 bbox)
      (lib gtk-3 adjustment)

      (lib gtk-3 label)
      (lib gtk-3 button)
      (lib gtk-3 list-store)
      (lib gtk-3 file-chooser)
      (lib gtk-3 file-chooser-dialog)
      (lib gtk-3 builder))

(begin

   ; (lib gtk textiter)
   (define |sizeof GtkTextIter| 80)
   (define (make-GtkTextIter)
      (make-bytevector |sizeof GtkTextIter| 0))
   (define GtkTextIter* fft-void*)

   (define GtkTextView* fft-void*)

   (define GTK_STOCK_CANCEL    "gtk-cancel")
   (define GTK_RESPONSE_CANCEL -6)
   (define GTK_STOCK_OPEN      "gtk-open")
   (define GTK_RESPONSE_ACCEPT -3)
))