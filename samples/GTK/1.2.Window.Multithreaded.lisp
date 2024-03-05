#!/usr/bin/env ol
(import (lib glib-2)
   (lib gtk-3))

;; application activate
(define (activate appl)
   ; create an empty window with title
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

   ; show it
   ((window 'show-all)))

; run demo infinity loop
(async (lambda ()
   (let infinity-loop ()
      (display ".")
      (sleep 10000)
      (infinity-loop))))

;; create an application
(define app (GtkApplication {
   'multithreaded #true

   'on-activate activate
}))

;; run
((app 'run) (command-line))
