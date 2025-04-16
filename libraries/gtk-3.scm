(define-library (gtk-3)
   (export
      Gtk:init
      Gtk:main
      Gtk:quit
      ;; gtk_main_quit
      ;; gtk_main_iteration
      ;; gtk_events_pending

      ;; gtk_check_version

      ;; make-GtkTextIter
      ;; GtkTextIter*

      ;; ; 
      ;; GtkTextView*

      ;; ; todo: move to "stock"
      ;; GTK_STOCK_CANCEL
      ;; GTK_RESPONSE_CANCEL
      ;; GTK_STOCK_OPEN
      ;; GTK_RESPONSE_ACCEPT

      (exports (gtk-3 application))

      ;; (exports (lib gtk-3 gtk))
      (exports (gtk-3 widget))
      (exports (gtk-3 window))
      (exports (gtk-3 container))

      ;; (exports (lib gtk-3 box))
      ;; (exports (lib gtk-3 bbox))
      ;; (exports (lib gtk-3 adjustment))

      (exports (gtk-3 label))
      (exports (gtk-3 button))
      ;; (exports (lib gtk-3 list-store))
      ;; (exports (lib gtk-3 file-chooser))
      ;; (exports (lib gtk-3 file-chooser-dialog))
      (exports (gtk-3 builder)))

   (import
      (scheme core)
      (otus ffi)
      (lib glib-2)

      (lib gdk-3)
      (only (otus async) sleep)
      (lib gtk-3 gtk)

      (gtk-3 application)

      (gtk-3 widget)
      (gtk-3 container)
      (gtk-3 window)

      ;; (lib gtk-3 box)
      ;; (lib gtk-3 bbox)
      ;; (lib gtk-3 adjustment)

      (gtk-3 label)
      (gtk-3 button)
      ;; (lib gtk-3 list-store)
      ;; (lib gtk-3 file-chooser)
      ;; (lib gtk-3 file-chooser-dialog)
      (gtk-3 builder))

(begin
   (define Gtk:init (case-lambda
      (() (gtk_init '(0) #f))
      ((a1) (if (ff? a1)
               (let* ((argv (a1 'argv #f)))
                  (gtk_init (list (if argv (length argv) 0)) argv)
                  (if (a1 'multithreaded #f)
                     (gdk_threads_add_idle (G_CALLBACK
                        (GTK_CALLBACK (userdata)
                           (sleep 0)
                           TRUE)) #f)))
            else
               (runtime-error "gtk-init: invalid options" a1)))
      ((a1 a2)
            (gtk_init a1 a2))
   ))

   (define Gtk:main gtk_main)
   (define Gtk:quit gtk_main_quit)
))