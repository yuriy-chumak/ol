(define-library (gtk-3 button)
   (export
      GtkButton
   )
   (import
      (scheme core)
      (otus ffi) (owl ff)
      (owl string)
      (lib gtk-3 gtk)
      (lib gtk-3 bin)
      (lib gtk-3 label)
      (lib gtk-3 button)
      
      (gtk-3 widget))

(begin
   (define GtkButton
      (define (make ptr options)
         (define base (GtkWidget ptr options))
         (define this (ff-replace base {
            ; Fetches the text from the label of the button.
            'get-text (lambda ()
                  (gtk_button_get_label ptr))

            ; Sets the text of the label of the button.
            'set-text (lambda (text)
                  (gtk_button_set_label ptr text))
            ; Sets the labels text and attributes from markup.
            'set-markup (lambda (markup)
                  (gtk_label_set_markup (gtk_bin_get_child ptr) markup))

            ; Sets the 'clicked' button event handler.
            'set-click-handler (GtkEventHandler "clicked" (widget userdata)
                  (make widget #e))


            ; internals
            'super base
         }))
         ; setup and return
         (if (options 'text #f)
            ((this 'set-text) (options 'text)))
         (if (options 'on-click #f)
            ((this 'set-click-handler) (options 'on-click)))
         ; smart object
         (GObject this))

   ; defaults
   (define default-text "button")

   ; main
   (case-lambda
      (()   (make (gtk_button_new) #e))
      ((a1) (cond
               ((eq? (type a1) type-vptr)
                  (make a1 #e))
               ((string? a1)
                  (make (gtk_button_new_with_label a1) #e))
               ((ff? a1)
                  (make (gtk_button_new_with_label (a1 'text default-text)) a1))
               (else
                  (runtime-error "GtkButton: invalid argument" a1))))
      ((a1 op) (cond
               ((and (eq? (type a1) type-vptr) (ff? op))
                  (make a1 op))
               (else
                  (runtime-error "GtkButton: invalid arguments" (cons a1 op)))))
   ))

))
