(define-library (lib gtk-3)
   (export
      GtkWidget*
      GtkApplication*
      GtkWindow*

      GApplicationFlags

      gtk_application_new
      gtk_application_window_new
      gtk_window_set_title
      gtk_window_set_default_size
      gtk_widget_show_all

      gtk_container_add

      ; todo: move next to (lib gtk bbox)
      gtk_button_box_new
      GTK_ORIENTATION_HORIZONTAL
      GTK_ORIENTATION_VERTICAL
      gtk_button_new_with_label
   )
   (import
      (scheme core)
      (otus ffi)
      (lib glib-2))

(begin

(define GtkWidget* type-vptr)
(define GtkApplication* type-vptr)
(define GApplicationFlags fft-int)
(define GtkWindow* type-vptr)
(define GtkCointainer* type-vptr)

(define GTK (load-dynamic-library "libgtk-3.so"))

(define gtk_application_new (GTK GtkApplication* "gtk_application_new" type-string GApplicationFlags))
(define gtk_application_window_new (GTK GtkWidget* "gtk_application_window_new" GtkApplication*))

(define gtk_window_set_title (GTK fft-void "gtk_window_set_title" GtkWindow* type-string))
(define gtk_window_set_default_size (GTK fft-void "gtk_window_set_default_size" GtkWindow* gint gint))

(define gtk_widget_show_all (GTK fft-void "gtk_widget_show_all" GtkWidget*))

(define gtk_container_add (GTK fft-void "gtk_container_add" GtkCointainer* GtkWidget*))

; (lib gtk bbox)
(define GtkOrientation fft-int) ; enum
(define GTK_ORIENTATION_HORIZONTAL 0)
(define GTK_ORIENTATION_VERTICAL 1)

(define gtk_button_box_new (GTK GtkWidget* "gtk_button_box_new" GtkOrientation))
(define gtk_button_new_with_label (GTK GtkWidget* "gtk_button_new_with_label" type-string))

))