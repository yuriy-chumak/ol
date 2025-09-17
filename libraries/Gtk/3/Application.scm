(define-library (Gtk 3 Application)
   (description "Gtk Application class")
   (export
      GtkApplication
   )
   (import
      (scheme base)
      (only (lib gdk-3) gdk_threads_add_idle)
      (only (otus async) sleep)

      (Gtk 3 Gtk)

      (lib gtk-3 application))

(begin
   (define GtkApplication
      (define (make ctor ptr options)
         (define this {
            'class 'Application
            'Ptr* ptr  ; raw pointer

            'Application ptr

            'run (lambda (command-line)
               (let ((status (g_application_run ptr (length command-line) command-line)))
                  (g_object_unref ptr)
                  status))

            'quit (lambda ()
               (g_application_quit ptr))

            'set-activate-handler (GtkEventHandler "activate" ())
         })

         ;; handle options
         ; emitted on the primary instance when an activation occurs
         (when (options 'on-activate #f)
            ((this 'set-activate-handler) (options 'on-activate)))
         ; ol: allow running actors in the background
         (when (options 'multithreaded #f)
            (gdk_threads_add_idle (G_CALLBACK
               (GTK_CALLBACK (userdata)
                  (sleep 0) ; handle waiting threads
                  TRUE))    ; G_SOURCE_CONTINUE
               #f))

         ;; smart object
         (GObject this))

   ; defaults
   (define default-id "org.gtk.example")
   (define default-flags G_APPLICATION_FLAGS_NONE)

   ; main
   (case-lambda
      (()   (make make (gtk_application_new default-id default-flags)))
      ((a1) (cond
               ((vptr? a1)
                  (make make a1 #e))
               ((string? a1)
                  (make make (gtk_application_new a1 default-flags) #e))
               ((integer? a1)
                  (make make (gtk_application_new default-id a1) #e))
               ((ff? a1)
                  (make make (gtk_application_new
                              (a1 'id default-id)
                              (a1 'flags default-flags)) a1))
               (else
                  (error "GtkApplication" a1))))
      ; inheritance: todo

      ; legacy (native) call
      ((a1 a2)
            (cond
               ; (app_id flags)
               ((and (string? a1) (integer? a2))
                  (make make (gtk_application_new a1 a2) #e))
               ; (flags app_id)
               ((and (string? a2) (integer? a1))
                  (make make (gtk_application_new a2 a1) #e))
               (else
                  (error "GtkApplication" a1 a2))))
   ))

))
