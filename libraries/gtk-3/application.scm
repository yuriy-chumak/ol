(define-library (gtk-3 application)
   (description "Application class")
   (export
      GtkApplication
   )
   (import
      (scheme base)
      (otus ffi) (owl ff)
      (lib gdk-3)
      (only (otus async) sleep)

      (lib gtk-3 gtk)
      (lib gtk-3 application))

(begin
   (import (owl io))
   (define GtkApplication
      (define (make ptr options)
         (define this {
            'ptr ptr
            'run (lambda (command-line)
               (let ((status (g_application_run ptr (length command-line) command-line)))
                  (g_object_unref ptr)
                  status))

            'quit (lambda ()
               (g_application_quit ptr))

            'set-activate-handler (lambda (handler)
               (define callback
                  (cond
                     ((eq? (type handler) type-callable) ; callback
                        handler)
                     ((and (eq? (type handler) type-enum+) ; pin?
                           (pair? (vm:deref handler))
                           (function? (cdr (vm:deref handler))))
                        (G_CALLBACK handler))
                     ((function? handler)
                        (G_CALLBACK
                           (vm:pin (cons
                              (cons gint (list GtkApplication* type-vptr))
                              (lambda (app userdata)
                                 (handler (make app #e)))))))
                     (else
                        (runtime-error "GtkApplication" "invalid handler"))))
               (g_signal_connect ptr "activate" callback #f))
         })

         ; handle options
         (if (options 'on-activate #f)
            ((this 'set-activate-handler) (options 'on-activate)))
         (if (options 'multithreaded #f)
            (gdk_threads_add_idle (G_CALLBACK
               (GTK_CALLBACK (userdata)
                  (sleep 0)
                  TRUE))
               #f))
         ; smart object
         (GObject this))

   ; defaults
   (define default-id "org.gtk.example")
   (define default-flags G_APPLICATION_FLAGS_NONE)

   ; main
   (case-lambda
      (()   (make (gtk_application_new default-id default-flags)))
      ((a1) (cond
               ((eq? (type a1) type-vptr)
                  (make a1 #e))
               ((string? a1)
                  (make (gtk_application_new a1 default-flags) #e))
               ((integer? a1)
                  (make (gtk_application_new default-id a1) #e))
               ((ff? a1)
                  (make (gtk_application_new
                              (a1 'id default-id)
                              (a1 'flags default-flags)) a1))
               (else
                  (runtime-error "GtkApplication" "invalid argument"))))
      ((a1 a2)
            (cond
               ((and (string? a1) (integer? a2))
                  (make (gtk_application_new a1 a2) #e))
               ((and (string? a2) (integer? a1))
                  (make (gtk_application_new a2 a1) #e))
               (else
                  (runtime-error "GtkApplication" "invalid arguments combination"))))
   ))

))
