(define-library (lib gtk-3 button)
   (export
      ; c types
      GtkButton*
      ; c interface
      gtk_button_new
      gtk_button_new_with_label
      gtk_button_set_label
      gtk_button_get_label

      ; lisp
      GtkButton
   )
   (import
      (scheme core)
      (otus ffi) (owl ff)
      (owl string)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkButton* type-vptr)

   (define gtk_button_new (GTK3 GtkWidget* "gtk_button_new"))
   (define gtk_button_new_with_label (GTK3 GtkWidget* "gtk_button_new_with_label" type-string))
   (define gtk_button_get_label (GTK3 type-string "gtk_button_get_label" GtkButton*))
   (define gtk_button_set_label (GTK3 void "gtk_button_set_label" GtkButton* type-string))

   ; lisp interface
   (define GtkButton
      (define (make ptr options)
         (define this {
            'ptr ptr
            'widget ptr

            ; Fetches the text from the label of the button.
            'get-text (lambda ()
               (gtk_button_get_label ptr))

            ; Sets the text of the label of the button.
            'set-text (lambda (text)
               (gtk_button_set_label ptr text))

            ; Sets the 'clicked' button event handler.
            'set-click-handler (lambda (handler)
               (define callback
                  (cond
                     ((and (eq? (type handler) type-enum+) ; pin?
                           (function? (vm:deref handler)))
                        handler)
                     ((function? handler)
                        (vm:pin (cons
                           (cons gint (list GtkWidget* type-vptr))
                           (lambda (widget userdata)
                              (handler (make widget #false))))))
                     (else
                        (runtime-error "GtkButton" "invalid handler"))))
               (g_signal_connect ptr "clicked" (G_CALLBACK callback) #f)
            )
         })
         (when (ff? options)
            ; apply options
            (if (options 'on-click #f)
               ((this 'set-click-handler) (options 'on-click)))
         )

         ; smart object
         (GtkThis this))

   ; defaults
   (define default-text "button")

   ; main
   (case-lambda
      (()   (make (gtk_button_new)))
      ((a1) (cond
               ((eq? (type a1) type-vptr)
                  (make a1 #f))
               ((string? a1)
                  (make (gtk_button_new_with_label a1) #f))
               ((ff? a1)
                  (make (gtk_button_new_with_label (a1 'text default-text)) a1))
               (else
                  (runtime-error "GtkButton" "invalid argument")))) ))

))
