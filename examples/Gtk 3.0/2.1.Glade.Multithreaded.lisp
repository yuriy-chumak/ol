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
(async (lambda ()
   (define label ((builder 'get-Label) "label"))

   (let infinity-loop ()
      ((label 'set-text) (strftime "%X"))

      (wait 1000)
      (infinity-loop))))

;; setup main window
(define window ((builder 'get-Window) "window" {
   'title "Glade Multithreaded Example"
   'on-destroy (lambda (this)
      ; stop running threads properly
      (for-each kill (running-threads))
      (Gtk:quit))
}))

;; show it
((window 'show-all))

;; run
(Gtk:main)