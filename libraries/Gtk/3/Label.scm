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
   (define GtkLabel
      (define (make ctor ptr options)
         (define base (GtkWidget ptr options))
         (define this (ff-replace base {
            'class 'Label  'superclass 'Widget
            'super base

            'Label ptr

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
         }))

         ; setup and return
         (if (options 'text #f)
            ((this 'set-text) (options 'text)))
         (if (options 'markup #f)
            ((this 'set-markup) (options 'markup)))
         (GObject this))

   ; defaults
   (define default-text "a label")

   ; main
   (case-lambda
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
      ((a1 op) (cond
               ((and (vptr? a1) (ff? op))
                  (make make a1 op))
               (else
                  (error "GtkLabel" a1 op))))
      ; Inheritance
      ((a1 a2 a3) (if (ctor? a1)
                     (make a1 a2 a3)
                     (error "GtkWidget" a1 a2 a3)))
      
   ))

))
