#!/usr/bin/env ol
(import (lib glib-2)
   (lib gtk-3))
(import (only (otus syscall) strftime))

;; application activate
(define (activate appl)
   ; create customized window with title
   (define window (GtkWindow appl {
      'title "Multithreaded Window"
      'width 640 'height 360
      'icon "dialog-information"

      ; properly stop running threads
      'on-destroy (lambda (this)
         (print "Close pressed. Bye-bye.")
         (for-each kill (running-threads))
         (g_application_quit (gtk_window_get_application (this 'ptr))))
   }))

   ; add a label to the window
   (define label (GtkLabel
      "Lorem ipsum dolor sit amet,\nconsectetur adipiscing elit."))
   ((window 'add) label)

   ; run demo infinite loop
   (async (lambda ()
      (let infinity-loop ()
         (sleep 10000)
         ((label 'set-markup) (strftime "<big>%c</big>"))
         (infinity-loop))))

   ; show it
   ((window 'show-all)))

;; create an application
(define app (GtkApplication {
   'multithreaded #true

   'on-activate activate
}))

;; run
((app 'run) (command-line))
