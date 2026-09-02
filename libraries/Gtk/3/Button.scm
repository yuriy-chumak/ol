(define-library (Gtk 3 Button)
   (export
      GtkButton
      ;; _GtkButton
   )
   (import
      (scheme base)

      (Gtk 3 Gtk)
      (Gtk 3 Widget)

      (lib gtk-3 gtk)
      (lib gtk-3 bin)
      (lib gtk-3 label)
      (lib gtk-3 image)
      (lib gtk-3 container)
      (lib gtk-3 button))

(begin
   ; helper function
   (define set-label-markup (G_CALLBACK
      (GTK_CALLBACK ((GObject* widget) (gint userdata))
         (when (g_type_check_instance_is_a widget GTK_TYPE_LABEL)
            (gtk_label_set_markup widget (vm:deref userdata))))))

   (GTK_CLASS Button Widget {
         ; Fetches the text from the label of the button.
         'get-text (lambda ()
               (gtk_button_get_label ptr))
         ; Sets the text of the label of the button.
         'set-text (lambda (text)
               (gtk_button_set_label ptr text))
         ; Sets the labels text and attributes from markup.
         'set-markup (lambda (markup)
               (define label (gtk_bin_get_child ptr))
               ; simple case, child is a GtkLabel
               (if (g_type_check_instance_is_a label GTK_TYPE_LABEL)
                  (gtk_label_set_markup label markup)
               ; difficult case, GtkLabel is somewhere there
               else
                  (define text (vm:pin markup))
                  (gtk_container_foreach (gtk_bin_get_child label) set-label-markup text)
                  (vm:unpin text))) ; avoid memory leaks

         'set-image (lambda (name isize)
            (cond
               ((string? name)
                  (gtk_button_set_image ptr (gtk_image_new_from_stock name isize))
                  (gtk_button_set_always_show_image ptr #t)) ; images inside buttons are not visible by default
               (else
                  (error "unsupported"))))

         'set-relief (lambda (relief)
            (gtk_button_set_relief ptr
               (case relief
                  ('normal GTK_RELIEF_NORMAL)
                  ('half GTK_RELIEF_HALF)
                  ('none GTK_RELIEF_NONE))))

         ; Sets the 'clicked' button event handler.
         'set-click-handler (GtkEventHandler "clicked" ())
      }

      ;; init
      (('on-click . 'set-click-handler)
       ('text . 'set-text)
       ('markup . 'set-markup))
      
      ;; defaults
      (define default-text "button")

      ;; main
      (GTK_CLASS:CONSTRUCTORS
         (()   (make make (gtk_button_new) #e))
         ((a1) (cond
                  ((vptr? a1)
                     (make make a1 #e))
                  ; (GtkButton "text"):
                  ((string? a1)
                     (make make (gtk_button_new_with_label a1) #e))
                  ; (GtkButton options):
                  ((ff? a1)
                     (make make (gtk_button_new_with_label (a1 'text default-text)) a1))
                  (else
                     (error "GtkButton" a1))))
      ))

))
