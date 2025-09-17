(define-library (Gtk 3 Button)
   (export
      GtkButton
   )
   (import
      (scheme base)

      (Gtk 3 Gtk)
      (Gtk 3 Widget)

      (lib gtk-3 gtk)
      (lib gtk-3 bin)
      (lib gtk-3 label)
      (lib gtk-3 button))

(begin
   (define GtkButton
      (define (make ctor ptr options)
         (define base (GtkWidget ctor ptr options))
         (define this (ff-replace base {
            'class 'Button  'superclass 'Widget
            'super base

            'Button ptr

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
            'set-click-handler (GtkEventHandler "clicked" ())

         }))

         ; setup and return
         (when (options 'text #f)
            ((this 'set-text) (options 'text)))
         (when (options 'on-click #f)
            ((this 'set-click-handler) (options 'on-click)))

         ; smart object
         (GObject this))

   ; defaults
   (define default-text "button")

   ; main
   (case-lambda
      (()   (make make (gtk_button_new) #e))
      ((a1) (cond
               ((vptr? a1)
                  (make make a1 #e))
               ; (GtkButton "text")
               ((string? a1)
                  (make make (gtk_button_new_with_label a1) #e))
               ; (GtkButton options)
               ((ff? a1)
                  (make make (gtk_button_new_with_label (a1 'text default-text)) a1))
               (else
                  (error "GtkButton" a1))))
      ((a1 op) (cond
               ((and (vptr? a1) (ff? op))
                  (make make a1 op))
               (else
                  (error "GtkButton" a1 op))))
      ; inheritance
      ((a1 a2 a3) (cond
               ((ctor? a1)
                  (make a1 a2 a3))
               (else
                  (error "GtkButton" a1 a2 a3)) ))
   ))

))
