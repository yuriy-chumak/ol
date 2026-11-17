(define-library (Gtk 3 CssProvider)
   (export
      GtkCssProvider
   )
   (import
      (scheme base)

      (Gtk 3 Gtk)
      (lib gtk-3 css-provider))

(begin
   (GTK_CLASS CssProvider #f {
         ; Loads data into css_provider, and by doing so clears any previously loaded information.
         'load-from-data (lambda (data)
            (gtk_css_provider_load_from_data ptr data -1 #f))
         'to-string (lambda ()
            (gtk_css_provider_to_string ptr))
      }
      (
         ('css . 'load-from-data)   ; css in a string
         ('file . 'load-from-file)) ; css in a file

   ; main
   (GTK_CLASS:CONSTRUCTORS
      (()   (make make (gtk_css_provider_new)))
      ((a1) (cond
               ((vptr? a1)
                  (make make a1 #e))
               ((string? a1)
                  (make make (gtk_css_provider_new) {'css a1}))
               ((ff? a1)
                  (make make (gtk_css_provider_new) a1))
               (else
                  (error "GtkApplication" a1))))
   ))

))
