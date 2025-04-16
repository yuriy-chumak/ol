#!/usr/bin/env ol
(import (gtk-3))
(import (only (olvm syscalls) strftime))

;; explicit init
(Gtk:init {
   'multithreaded #true
})

;; load ui from the file
(define builder (GtkBuilder "2.0.Glade.glade"))

;; demo infinite loop
(async (lambda ()
   (define label (GtkLabel
      ((builder 'get-object) "label")))
   (define button (GtkButton
      ((builder 'get-object) "button")))

   (let infinity-loop ()
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
         (Gtk:quit))
   }))

;; show it
((window 'show-all))

;; run
(Gtk:main)