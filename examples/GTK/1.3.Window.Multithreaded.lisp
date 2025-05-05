#!/usr/bin/env ol
(import (gtk-3))
(import (only (olvm syscalls) strftime))

;; application setup
(define (activate appl)
   ; create customized window with title
   (define window (GtkWindow appl {
      'title "Multithreaded Gtk-3 Window"
      'width 640  'height 360
      'icon "dialog-information"

      ; proper stopping of running threads
      'on-destroy (lambda (this)
         (print "Close pressed. Bye-bye.")
         (for-each kill (running-threads))
         (define app ((
            (GtkWindow (this 'ptr)) 'get-application)))
         ((app 'quit)))
   }))

   ; add a label to the window
   (define label (GtkLabel
      "Lorem ipsum dolor sit amet,\nconsectetur adipiscing elit."))
   ((window 'add) label)

   ; run demo infinite loop
   (async (lambda ()
      (let infinity-loop ()
         ((label 'set-markup) (strftime "<big>%c</big>"))
         (wait 1000)
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
