(define-library (Gtk 3 CssProvider)
   (export
      GtkCssProvider
   )
   (import
      (scheme base)

      (Gtk 3 Gtk)
      (lib gtk-3 css-provider))

(begin
   (import (owl io))
   (define GtkCssProvider
      (define (make ctor ptr options)
         (define this {
            'class 'CssProvider
            'Ptr* ptr  ; raw pointer

            'CssProvider ptr

            ; Loads data into css_provider, and by doing so clears any previously loaded information.
            'load-from-data (lambda (data)
               (gtk_css_provider_load_from_data ptr data -1 #f))
            'to-string (lambda ()
               (gtk_css_provider_to_string ptr))
         })

         ;; handle options
         ; css in a string
         (when (options 'css #f)
            ((this 'load-from-data) (options 'css)))
         ; css in a file
         (when (options 'file #f)
            ((this 'load-from-file) (options 'file)))

         ;; smart object
         (GObject this))

   ; main
   (case-lambda
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
      ; inheritance: todo
   ))

))
