#!/usr/bin/env ol

(import
   (lib gtk-3)
   (lib gdk-3)
   (lib gtk-3 entry)
   (lib gtk-3 spinner)
   (lib gtk-3 message-dialog)

   (stars! rest))
(server "http://127.0.0.1:4002")

; ===============================================================
(gtk_init '(0) #f)

(define builder (gtk_builder_new_from_file "stars!/login.glade"))
(gtk_builder_connect_signals builder #f)

(define window (gtk_builder_get_object builder "window"))
(gtk_widget_show_all window)

; -- fields -----
(define b_login (gtk_builder_get_object builder "b_login"))
(define e_login (gtk_builder_get_object builder "e_login"))
(define e_password (gtk_builder_get_object builder "e_password"))
(define w_spinner (gtk_builder_get_object builder "w_spinner"))

(define b_login_clicked
   (GTK_CALLBACK (widget userdata)
      (gtk_widget_set_sensitive b_login 0)
      (gtk_spinner_start w_spinner)

      (async (lambda ()
         ; let's do login:
         (define login (POST "/api/login" {
            'login (gtk_entry_get_text e_login)
            'password (gtk_entry_get_text e_password)
         }))
         (if login
         then
            (if (ff? login); 200 and json
            then
               (session (login 'session))
               ; ok!
               (print (GET "/api/races"))
               ; (mail 'main 'logged-in!)
            else ; error
               (GtkMessageDialog window {
                  'type GTK_MESSAGE_ERROR
                  'buttons GTK_BUTTONS_CLOSE
                  'message (case login
                     (401 (string-append "401 Unauthorized" "\n"
                                         "Invalid username or/and password. Try again."))
                     (403 (string-append "403 Forbidden" "\n"
                                         "You'r banned. Sorry."))
                     (else
                          (string-append "Error number " (number->string login 10))))
               }))
         else
            (GtkMessageDialog window {
               'type GTK_MESSAGE_ERROR
               'buttons GTK_BUTTONS_CLOSE
               'message "No network?"
            }))

         (gtk_spinner_stop w_spinner)
         (gtk_widget_set_sensitive b_login 1)))

      TRUE))

(g_signal_connect b_login "clicked" (G_CALLBACK b_login_clicked) NULL)

; -- done --------------
(g_object_unref builder)

; -- asyncify ----------
; we should provide this one function
; to be able process asyncs and coroutines while
; gtk is in foreground
(define on_timeout
   (GTK_CALLBACK (userdata)
      (sleep 0) ; just switch context
      TRUE))
(gdk_threads_add_idle (G_CALLBACK on_timeout) NULL)


; done
(define quit
   (GTK_CALLBACK (widget userdata)
      (print "Close pressed. Bye-bye.")
      (gtk_main_quit)))

(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)
(gtk_main)

;; (define answer (POST "http://127.0.0.1:4002/api/login" { 'login "user@1" 'password "123456"}))
;; (print answer)

;; (if answer
;;    (session (answer 'session))
;;    (halt 1))

;; (print (GET "http://127.0.0.1:4002/api/races"))
;; (print "ok.")
