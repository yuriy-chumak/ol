(define-library (Gtk 3)
   (export
      Gtk:init
      Gtk:main
      Gtk:quit
      ;; gtk_main_iteration
      ;; gtk_events_pending
      GTK_CALLBACK

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

      (exports (Gtk 3 Application))

      ;; (exports (lib gtk-3 gtk))
      (exports (Gtk 3 Widget))
      (exports (Gtk 3 Container))
      (exports (Gtk 3 Window))

      ;; (exports (lib gtk-3 box))
      ;; (exports (lib gtk-3 bbox))
      ;; (exports (lib gtk-3 adjustment))

      (exports (Gtk 3 Label))
      (exports (Gtk 3 Button))
      ;; (exports (lib gtk-3 list-store))
      ;; (exports (lib gtk-3 file-chooser))
      ;; (exports (lib gtk-3 file-chooser-dialog))
      (exports (Gtk 3 Builder)))

   (import
      (scheme core)
      (otus ffi)
      (lib glib-2)

      (lib gdk-3)
      (only (otus async) sleep)
      (lib gtk-3 gtk)

      (Gtk 3 Application)

      (Gtk 3 Widget)
      (Gtk 3 Container)
      (Gtk 3 Window)

      ;; (lib gtk-3 box)
      ;; (lib gtk-3 bbox)
      ;; (lib gtk-3 adjustment)

      (Gtk 3 Label)
      (Gtk 3 Button)
      ;; (lib gtk-3 list-store)
      ;; (lib gtk-3 file-chooser)
      ;; (lib gtk-3 file-chooser-dialog)
      (Gtk 3 Builder))

(begin
   (define Gtk:init (case-lambda
      (() (gtk_init '(0) #f))
      ((a1) (if (ff? a1)
               (let* ((argv (a1 'argv #f)))
                  (gtk_init (list (if argv (length argv) 0)) argv)
                  (if (a1 'multithreaded #f)
                     (gdk_threads_add_idle (G_CALLBACK
                        (GTK_CALLBACK (userdata)
                           (sleep 0) ; handle waiting threads
                           TRUE))    ; G_SOURCE_CONTINUE
                        #f)))
            else
               (runtime-error "gtk-init: invalid options" a1)))
      ; legacy (native) call
      ((a1 a2)
            (gtk_init a1 a2))
   ))

   (define Gtk:main gtk_main)
   (define Gtk:quit gtk_main_quit)
))