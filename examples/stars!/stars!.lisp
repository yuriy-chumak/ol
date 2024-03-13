#!/usr/bin/env ol

(import
   (lib gtk-3)
   (lib gdk-3)
   (lib gtk-3 entry)
   (lib gtk-3 spinner)
   (lib gtk-3 message-dialog)
   (lib gtk-3 header-bar)

   (stars! rest))
(server "http://127.0.0.1:4002")

; ================================================================
(gtk_init '(0) #f)

(define builder (gtk_builder_new_from_file "stars!/stars!.glade"))
(gtk_builder_connect_signals builder #f)

(define window (gtk_builder_get_object builder "window"))
(gtk_widget_show_all window)

(define header-bar (gtk_builder_get_object builder "header-bar"))
(define (set-status str)
   (gtk_header_bar_set_subtitle header-bar str))
(define b_reconnect (gtk_builder_get_object builder "b_reconnect"))


; -=( Login Dialog )=---------------------------------------------
,load "login.lisp"
; ----------------------------------------------------------------
,load "profile.lisp"

; -- done --------------
(g_object_unref builder)
; -----------------------------------------------------------------


; -- asyncify ----------
; we should provide this one function
; to be able process asyncs and coroutines while
; gtk is in foreground
(define on_timeout
   (GTK_CALLBACK (userdata)
      (sleep 0) ; just switch context
      TRUE))
(gdk_threads_add_idle (G_CALLBACK on_timeout) NULL)

; -- destroy --
(define destroy
   (GTK_CALLBACK (widget userdata)
      (print "Close pressed. Bye-bye.")
      (gtk_main_quit)))
(g_signal_connect window "destroy" (G_CALLBACK destroy) NULL)

; -- startup --
(define ping
   (GTK_CALLBACK (userdata)
      (define ping (GET "/api/ping"))
      (print "ping: " ping)
      (if (number? ping)
         (case ping
            (200
               ; send message to update races list
               (print "ok"))
            (401
               ; show login dialog
               (gtk_dialog_run login-dialog))
            (403
               (GtkMessageDialog window {
                  'type GTK_MESSAGE_ERROR
                  'buttons GTK_BUTTONS_CLOSE
                  'message "Sorry, you'r banned!"
               }))
            (else
               (GtkMessageDialog window {
                  'type GTK_MESSAGE_ERROR
                  'buttons GTK_BUTTONS_CLOSE
                  'message "Something wrong."
               }))))
      FALSE)) ; stop function
(gdk_threads_add_timeout 100 (G_CALLBACK ping) NULL)
(g_signal_connect b_reconnect "clicked" (G_CALLBACK ping) NULL)

(gtk_main)
