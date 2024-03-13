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
         (define base (GtkWidget ptr))
         (define this (ff-replace base {
            ; Fetches the text from the label of the button.
            'get-text (lambda ()
                  (gtk_button_get_label ptr))

            ; Sets the text of the label of the button.
            'set-text (lambda (text)
                  (gtk_button_set_label ptr text))

            ; Sets the 'clicked' button event handler.
            'set-click-handler (GtkEventHandler "clicked" (widget userdata)
                  (make widget #false))


            ; internals
            'super base
            'setup (lambda (this options)
               ((base 'setup) this options)

               (if (options 'text #f)
                  ((this 'set-text) (options 'text)))
               (if (options 'on-click #f)
                  ((this 'set-click-handler) (options 'on-click)))
               #true)
         }))
         ; apply options
         (when (ff? options)
            ((this 'setup) this options))
         ; smart object
         (GtkThis this))

   ; defaults
   (define default-text "button")

   ; main
   (case-lambda
      (()   (make (gtk_button_new) #f))
      ((a1) (cond
               ((eq? (type a1) type-vptr)
                  (make a1 #f))
               ((string? a1)
                  (make (gtk_button_new_with_label a1) #f))
               ((ff? a1)
                  (make (gtk_button_new_with_label (a1 'text default-text)) a1))
               (else
                  (runtime-error "GtkButton: invalid argument" a1))))
      ((a1 op) (cond
               ((and (eq? (type a1) type-vptr) (ff? op))
                  (make a1 op))
               (else
                  (runtime-error "GtkButton: invalid arguments" (cons a1 op))) ))
   ))

))
