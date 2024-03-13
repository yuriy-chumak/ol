(define login-dialog (gtk_builder_get_object builder "login-dialog"))

; -- fields ----
(define b_login (gtk_builder_get_object builder "b_login"))
(define b_cancel (gtk_builder_get_object builder "b_cancel"))

; -- handlers --
(define b_login_clicked
   (define e_login (gtk_builder_get_object builder "e_login"))
   (define e_password (gtk_builder_get_object builder "e_password"))
   (define w_spinner (gtk_builder_get_object builder "w_spinner"))

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
               (gtk_widget_hide login-dialog)
               (set-status "Список всех ваших рас и игр в которые вы играете")
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

(define b_cancel_clicked
   (GTK_CALLBACK (widget userdata)
      (gtk_widget_hide login-dialog)

      TRUE))
(g_signal_connect b_cancel "clicked" (G_CALLBACK b_cancel_clicked) NULL)
(g_signal_connect login-dialog "response" (G_CALLBACK b_cancel_clicked) NULL)
