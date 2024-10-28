#!/usr/bin/env ol
(import (lib glib-2)
   (lib gtk-3))
(import (only (olvm syscalls) strftime))

;; explicitly init
(gtk_init {
   'multithreaded #true
})

;; load ui from the file
(define builder (GtkBuilder "2.0.Glade.glade"))

;; demo infinite loop
(async (lambda ()
   (let infinity-loop ()

      (define label (GtkLabel
         ((builder 'get-object) "label")))
      (define button (GtkButton
         ((builder 'get-object) "button")))

      ((label 'set-text) (strftime "%X"))
      ((button 'set-text) (strftime "%S"))

      (wait 1000)
      (infinity-loop))))

;; setup main window
(define window (GtkWindow
   ((builder 'get-object) "window") {
      'title "Glade Multithreaded Example"
      'on-destroy (lambda (this)
         ; properly stop running threads
         (for-each kill (running-threads))
         (gtk_main_quit))
   }))

;; show it
((window 'show-all))

;; run
(gtk_main)