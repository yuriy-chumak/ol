#!/usr/bin/env ol
(import (Gtk 3.0))
(import (only (olvm syscalls) strftime))

;; explicit init
(Gtk:init {
   'multithreaded #true
})

;; load ui from the file
(define builder (GtkBuilder "2.0.Glade.glade"))

;; demo infinite loop
(async 'demo (lambda ()
   (define label ((builder 'get-Label) "label"))

   (let infinity-loop ()
      ((label 'set-text) (strftime "%X"))

      (wait 1000)
      (infinity-loop))))

;; setup main window
(define window ((builder 'get-Window) "window" {
   'title "Glade Multithreaded Example"
   'on-destroy (lambda (this)
      (kill 'demo) ; stop running threads properly
      
      ; when we do a (Gtk:main) we should use (Gtk:quit)
      ;   instead of (GtkApplication 'quit)
      (Gtk:quit))
}))

;; display the window
((window 'show-all))

;; run
(Gtk:main)
