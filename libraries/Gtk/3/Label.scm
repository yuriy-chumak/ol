(define-library (Gtk 3 Label)
   (export
      GtkLabel
   )
   (import
      (scheme base)

      (Gtk 3 Gtk)
      (Gtk 3 Widget)

      (lib gtk-3 label))
      
(begin
   ; lisp interface
   (GTK_CLASS Label Widget {
         ; Fetches the text from the label of the button.
         'get-text (lambda ()
            (gtk_label_get_text ptr))
         ; Sets the text of the label of the button.
         'set-text (lambda (text)
            (gtk_label_set_text ptr text))
         ; universal "get or set"
         'text (case-lambda
            (()(gtk_label_get_text ptr))
            ((text)
               (gtk_label_set_text ptr text)))

         ; Sets the labels text and attributes from markup.
         'set-markup (lambda (markup)
            (gtk_label_set_markup ptr markup))
      }

      ;; init
      (('text . 'set-text)
       ('markup . 'set-markup))

      ; defaults
      (define default-text "a label")

      ; main
      (GTK_CLASS:CONSTRUCTORS
      (()   (make make (gtk_label_new default-text) #e))
      ((a1) (cond
               ((vptr? a1)
                  (make make a1 #e))
               ((string? a1)
                  (make make (gtk_label_new a1) #e))
               ((ff? a1)
                  (make make (gtk_label_new (a1 'text default-text)) a1))
               (else
                  (error "GtkLabel" a1))))
   ))

))
