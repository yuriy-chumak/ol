(define-library (lib gtk-3 label)
   (export
      GtkLabel*

      gtk_label_new
      gtk_label_get_text
      gtk_label_set_text
      gtk_label_set_markup

      ; lisp
      GtkLabel
   )
   (import
      (scheme base)
      (otus ffi) (owl ff)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkLabel* type-vptr)

   (define gtk_label_new (GTK3 GtkWidget* "gtk_label_new" type-string))
   (define gtk_label_get_text (GTK3 type-string "gtk_label_get_text" GtkLabel*))
   (define gtk_label_set_text (GTK3 void "gtk_label_set_text" GtkLabel* type-string))

   (define gtk_label_set_markup (GTK3 void "gtk_label_set_markup" GtkLabel* type-string))

   ; lisp interface
   (define GtkLabel
      (define (make ptr options)
         (define base (GtkWidget ptr))
         (define this (ff-replace base {
            ; Fetches the text from the label of the button.
            'get-text (lambda ()
               (gtk_label_get_text ptr))

            ; Sets the text of the label of the button.
            'set-text (lambda (text)
               (gtk_label_set_text ptr text))

            ; Sets the labels text and attributes from markup.
            'set-markup (lambda (markup)
               (gtk_label_set_markup ptr markup))

            ; internals
            'super base
            'setup (lambda (this options)
               ((base 'setup) this options)

               (if (options 'text #f)
                  ((this 'set-text) (options 'text)))
               (if (options 'markup #f)
                  ((this 'set-markup) (options 'markup)))

               #true)
         }))
         ; apply options
         ((this 'setup) this options)

         ; smart object
         (GObject this))

   ; defaults
   (define default-text "a label")

   ; main
   (case-lambda
      (()   (make (gtk_label_new default-text) #f))
      ((a1) (cond
               ((eq? (type a1) type-vptr)
                  (make a1 #f))
               ((string? a1)
                  (make (gtk_label_new a1) #f))
               ((ff? a1)
                  (make (gtk_label_new (a1 'text default-text)) a1))
               (else
                  (runtime-error "GtkLabel: invalid argument" a1))))
      ((a1 op) (cond
               ((and (eq? (type a1) type-vptr) (ff? op))
                  (make a1 op))
               (else
                  (runtime-error "GtkLabel: invalid arguments" (cons a1 op)))))
   ))

))
