#!/usr/bin/env ol

(import
   (lib glib-2)
   (lib gtk-3)
   (lib gdk-3))
(import (only (otus syscall) strftime))

(import (lib gtk-3 toggle-button))
(import (lib gtk-3 progress-bar))

(define on_show_text_toggled
   (GTK_CALLBACK (button progressbar)
      (define show_text (gtk_toggle_button_get_active button))
      (if (eq? show_text 1)
      then
         (gtk_progress_bar_set_show_text progressbar 1)
         (gtk_progress_bar_set_text progressbar (strftime "%F %H:%M:%S\0"))
      else
         (gtk_progress_bar_set_show_text progressbar 0))
      TRUE))

(define on_activity_mode_toggled
   (GTK_CALLBACK (button progressbar)
      (define activity_mode (gtk_toggle_button_get_active button))
      (if (eq? activity_mode 1)
      then
         (gtk_progress_bar_pulse progressbar)
      else
         (gtk_progress_bar_set_fraction progressbar 0.0))
      TRUE))

(define on_right_to_left_toggled
   (GTK_CALLBACK (button progressbar)
      (define value (gtk_toggle_button_get_active button))
      (gtk_progress_bar_set_inverted progressbar value)
      TRUE))

(define on_timeout
   (GTK_CALLBACK (progressbar)
      (define value (gtk_progress_bar_get_fraction progressbar))
      (define new-value (+ value 0.01))

      (gtk_progress_bar_set_fraction progressbar (if (< new-value 1) new-value 0))
      TRUE))

; -=( main )=--------------------------------------------------------------------
(gtk_init '(0) #f)

(define builder (gtk_builder_new_from_file "templates/11.1. Progress Bar.glade"))
(gtk_builder_connect_signals builder #f)

(define window (gtk_builder_get_object builder "window"))
(gtk_widget_show_all window)

; Progress Bar
(define progressbar (gtk_builder_get_object builder "progressbar"))
; no need to g_object_unref !

; Show Text
(define button (gtk_builder_get_object builder "show_text"))
(g_signal_connect button "toggled" (G_CALLBACK on_show_text_toggled) progressbar)

; Activity Mode
(define button (gtk_builder_get_object builder "activity"))
(g_signal_connect button "toggled" (G_CALLBACK on_activity_mode_toggled) progressbar)

; Right to Left
(define button (gtk_builder_get_object builder "right_to_left"))
(g_signal_connect button "toggled" (G_CALLBACK on_right_to_left_toggled) progressbar)

(g_object_unref builder)

; background worker
(gdk_threads_add_timeout 50 (G_CALLBACK on_timeout) progressbar)

; done
(define quit
   (GTK_CALLBACK (widget userdata)
      (print "Close pressed. Bye-bye.")
      (gtk_main_quit)))

(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)
(gtk_main)
